#' Clean worklist
#'
#' Cleans and postprocesses worklist tibbles after fetching them
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
#'
#' @keywords internal
#' @noRd

clean_worklist <- function(df) {
  df %>%
    mutate(did = row_number()) %>%
    filter(
      !str_detect(exam_description, "RESEARCH"),
      !str_detect(exam_description, "FORFILE"),
      ris_mrn %not_in% c("00000000", "11111111", "22222222")
    ) %>%
    # Exclude Inpt./UCC CTs (redundant as safeguard, also done later):
    filter(!(modality_code == "CT" & (patient_status_code == "I" | str_detect(patient_location_code, "UCC|SCC|ER")))) %>%
    # Exclude Inpt./UCC MRIs of Neuro:
    filter(!(service == "Neuro" & (patient_status_code == "I" | str_detect(patient_location_code, "UCC|SCC|ER")))) %>%
    left_join(loc_by_state, by = "location_code") %>%
    left_join(exam_weights, by = "exam_code") %>%
    mutate(
      wet_read_request = ifelse(wet_read_request == "wet_read", "wet_read", ifelse(str_detect(toupper(order_comments), "STAT|WET"), "stat", "normal")),
      # Radiograph weighting hardcoded:
      exam_weighting = ifelse(modality_code == "CR", ifelse(body_site == "WHOLE-BODY", .4, .2), exam_weighting),
      # Prevent NAs in exam weights (impute 1.3)
      exam_weighting = replace_na(exam_weighting, 1.3)
    ) %>%
    mutate(modifier = ifelse(referring_md_name %in% ref_peds, "PEDS", modifier)) %>%
    # De-duplicate:
    distinct(ris_accession_number, .keep_all = TRUE)
}


#' Fetch scheduled exams
#'
#' Gets worklist of scheduled exams today + next 3 days from RadOps.diagnostic_master_v
#' Requires database RadOps_UX (username) and RadOps_PW (password) to be set
#' in GlobalEnv.
#'
#' @import dplyr
#'
#' @export

fetch_scheduled <- function() {
  con <- prep_conn_sqlserver(
    conn_name = "RadOps",
    server = "MYSQLSERVER",
    port = 88888,
    instance = "MSSQLSERVER",
    domain = "MyCompany",
    database = "MyDB"
  )

  sched_today <- mdb_query(
    con,
    "SELECT * 
     FROM radiology_master_table 
     WHERE cast(ris_scheduled_dt as DATE) BETWEEN 
     cast(getdate() as DATE) AND cast(getdate()+3 as DATE)
     AND archive_vs_dictated='Dictated' 
     AND service in ('Body', 'Muscl')
     AND activity_status_code = 'S';"
  )

  worklist_today <- sched_today %>% clean_worklist()
}


#' Fetch completed exams
#'
#' Gets worklist of completed exams up to current date from RadOps.RadDataView
#' (includes max. 90 days old to include foreign exams) and filters relevant exams.
#' Requires database RadOps_UX (username) and RadOps_PW (password) to be set
#' in GlobalEnv.
#'
#' @param reassign Defaults to FALSE = ignores cases previously touched by ignyt.
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#'
#' @import dplyr
#' @importFrom vctrs vec_rbind
#' @importFrom lubridate %--% years days ymd_hms minutes
#' @importFrom DBI dbDisconnect
#'
#' @export

fetch_completed <- function(reassign = FALSE, service = "Body") {
  con <- prep_conn_sqlserver(
    conn_name = "RadOps",
    server = "MYSQLSERVER",
    port = 88888,
    instance = "MSSQLSERVER",
    domain = "MyCompany",
    database = "MyDB"
  )
  if (service == "Body") {
    service_filter <- "'Body', 'Muscl'"
  } else {
    service_filter <- paste0("'", service, "'")
  }

  if (reassign) {
    last_quarter <- mdb_query(con, sprintf("SELECT *
                                  FROM RadDataView t1
                                  WHERE cast(t1.[Begin Date] as date) BETWEEN cast(getdate()-90 as DATE) AND cast(getdate() as DATE)
                                  AND t1.[Radiologist RIS ID] = '0'
                                  AND t1.Service in (%s)
                                  AND t1.[Activity Status Code] = 'Exam completed';", service_filter))
  } else {
    last_quarter <- mdb_query(con, sprintf("SELECT t1.*, t2.ris_accession_number
                                  FROM RadDataView t1
                                  LEFT JOIN ignyt.log_assigned t2
                                  ON t1.[Accession Number] = t2.ris_accession_number
                                  WHERE t2.ris_accession_number IS NULL
                                  AND cast(t1.[Begin Date] as date) BETWEEN cast(getdate()-90 as DATE) AND cast(getdate() as DATE)
                                  AND t1.[Radiologist RIS ID] = '0'
                                  AND t1.Service in (%s)
                                  AND t1.[Activity Status Code] = 'Exam completed';", service_filter)) %>%
      select(-ris_accession_number) %>%
      filter(`Exam Completed` < ymd_hms(Sys.time()) - minutes(10))
  }

  last_quarter %<>%
    rename_at(names(datamart_colnames), ~datamart_colnames) %>%
    # Placed here instead of clean_worklist b/c of lack of dob in anonymized testing data
    mutate(age = round(dob %--% ris_exam_end_dt / years(1), 1)) %>%
    mutate(age = replace_na(age, 99.99)) # Age is used for CT PEDS cut-off (in core_divide_ct)

  days_lookahead <- switch(wday(today()), 1, 1, 1, 1, 1, 3, 2)
  upcoming_appointments <- mdb_query(con, sprintf("SELECT MRN, VIST_DATE 
                                                       FROM datamart.cadence_upcoming_visits
                                                       WHERE MRN in ('%s')", paste(last_quarter$ris_mrn, collapse = "', '"))) %>%
    filter(VIST_DATE < Sys.time() + days(days_lookahead))

  DBI::dbDisconnect(con)

  worklist_today <- last_quarter %>%
    left_join(upcoming_appointments, by = c("ris_mrn" = "MRN")) %>%
    mutate(wet_read_request = ifelse(is.na(VIST_DATE), "normal", "wet_read")) %>%
    select(-VIST_DATE) %>%
    clean_worklist()
}

#' Prepare CT/MR worklist
#'
#' Adds dmt column to worklist and assigns best/most likely DMT.
#' Uses internal reference tables (exported in namespace).
#' General DMT will be assigned to all exams where no other DMT can
#' reliably be assigned.
#' Returns tibble of CT and MRI exams with dmt in last column.
#'
#' @param worklist Tibble as produced by fetch_*() functions
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#'
#' @import dplyr
#' @importFrom stringr str_detect
#'
#' @export

prep_worklist <- function(worklist, service = "Body") {
  worklist_today <- worklist %>%
    filter(modality_code %in% c("CT", "MR")) %>%
    # Excluding certain exams that shouldn't automatically be assigned:
    filter(!str_detect(exam_description, "DELAY|LIMITED|SCOUT|RESEARCH|SIM|FILM|VIRTUAL")) %>%
    mutate(dmt = as.character(NA))

  if (service == "Body") {
    # Bone service to bone ----------------------------------------------------

    dmt_assigned <- worklist_today %>%
      filter(service == "Muscl") %>%
      mutate(dmt = "bone")
    worklist_today %<>% filter(service != "Muscl")

    # Match by protocol description -------------------------------------------

    for (i in 1:length(match_desc_list)) {
      worklist_today %<>%
        mutate(dmt = ifelse(
          str_detect(
            protocol_description,
            paste0(match_desc_list[[i]], collapse = "|")
          ),
          names(match_desc_list[i]), dmt
        ))

      if (nrow(worklist_today %>% filter(!is.na(dmt))) > 0) {
        dmt_assigned %<>% vec_rbind(worklist_today %>% filter(!is.na(dmt)))
        worklist_today %<>% filter(is.na(dmt))
      }
    }

    # Match by referring physician --------------------------------------------

    # PEDs first
    worklist_today %<>% mutate(dmt = ifelse(referring_md_name %in% ref_peds, "peds", dmt))

    dmt_assigned %<>% vec_rbind(worklist_today %>% filter(!is.na(dmt)))
    worklist_today %<>% filter(is.na(dmt))

    # Then rest:
    for (i in 1:nrow(ref_dmts)) {
      worklist_today %<>%
        mutate(dmt = ifelse(
          referring_md_name == ref_dmts$referring_md_name[i],
          ref_dmts$dmt.1[i], dmt
        ))
      if (nrow(worklist_today %>% filter(!is.na(dmt))) > 0) {
        dmt_assigned %<>% vec_rbind(worklist_today %>% filter(!is.na(dmt)))
        worklist_today %<>% filter(is.na(dmt))
      }
    }

    # Match by history --------------------------------------------------------

    for (i in 1:length(match_hx_list)) {
      worklist_today %<>%
        mutate(dmt = ifelse(
          str_detect(
            tolower(history),
            paste0(match_hx_list[[i]], collapse = "|")
          ),
          names(match_hx_list[i]), dmt
        ))

      if (nrow(worklist_today %>% filter(!is.na(dmt))) > 0) {
        dmt_assigned %<>% vec_rbind(worklist_today %>% filter(!is.na(dmt)))
        worklist_today %<>% filter(is.na(dmt))
      }
    }

    # Match by protocol -------------------------------------------------------

    for (i in 1:length(match_prot_list)) {
      worklist_today %<>%
        mutate(dmt = ifelse(
          str_detect(
            exam_description,
            paste0(match_prot_list[[i]], collapse = "|")
          ),
          names(match_prot_list[i]), dmt
        ))

      if (nrow(worklist_today %>% filter(!is.na(dmt))) > 0) {
        dmt_assigned %<>% vec_rbind(worklist_today %>% filter(!is.na(dmt)))
        worklist_today %<>% filter(is.na(dmt))
      }
    }


    # Exclude MRI and assign Breast and Thyroid DMT to CTs --------------------

    worklist_today[which(worklist_today$modality_code == "MR"), ] %<>% mutate(dmt = "general")
    dmt_assigned %<>% vec_rbind(worklist_today %>% filter(!is.na(dmt)))
    worklist_today %<>% filter(is.na(dmt))

    for (i in 1:length(match_hx_ct_list)) {
      worklist_today %<>%
        mutate(dmt = ifelse(
          str_detect(
            tolower(history),
            paste0(match_hx_ct_list[[i]], collapse = "|")
          ),
          names(match_hx_ct_list[i]), dmt
        ))

      if (nrow(worklist_today %>% filter(!is.na(dmt))) > 0) {
        dmt_assigned %<>% vec_rbind(worklist_today %>% filter(!is.na(dmt)))
        worklist_today %<>% filter(is.na(dmt))
      }
    }

    # Assign generic DMT ------------------------------------------------------

    if (nrow(worklist_today) > 0) {
      worklist_today %<>% mutate(dmt = "general")
      dmt_assigned %<>% vec_rbind(worklist_today)
    }

    # Reassign DMT to certain MRIs:
    dmt_assigned[which(dmt_assigned$modality_code == "MR"), ] %<>% mutate(
      # Make certain exams general again
      dmt = ifelse(tolower(history) %in% c("pancreatic cyst", "panc cyst", "pancreas cyst"), "general", dmt),
      dmt = ifelse(dmt == "rectal" & str_detect(exam_description, "ABDOMEN|LIVER|CHOLANGIOGRAM"), "general", dmt),
      # Remove CT specific DMTs
      dmt = ifelse(dmt == "gastric_mixed", "bone", dmt)
    )

    return(dmt_assigned)
  } else if (service == "Neuro") {
    return(worklist_today %>% mutate(dmt = "general"))
  }
}
