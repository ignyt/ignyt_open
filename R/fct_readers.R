#' Fetch today's QGenda schedule
#'
#' Connects to the QGenda RadOps SQL table and gets today's readers.
#' Database username and password default to environment
#' variables (database-name_UX and _PW).
#' Returns a dataframe.
#'
#' @param tomorrow_shift Whether to get today's or tomorrow's roster. Defaults to today.
#' @param date Optional parameter to get readers of a specific date (format YYYY-MM-DD)
#' @param service Department service. Defaults to "Body", can be "Neuro" or "Nuclear"
#'
#' @import dplyr DBI
#' @importFrom tidyr replace_na
#' @importFrom DBI dbDisconnect
#' @importFrom stringr str_detect
#'
#' @export

prep_readers <- function(tomorrow_shift = FALSE, date = NULL, service = "Body") {
  qgenda_con <- prep_conn_sqlserver(
    conn_name = "RadOps",
    server = "MYSQLSERVER",
    port = 88888,
    instance = "MSSQLSERVER",
    domain = "MyCompany",
    database = "MyDB"
  )
  
  if (is.null(date)) {
    if (tomorrow_shift) {
      sql_query <- "SELECT * 
                      FROM QGenda_Schedules_View 
                      WHERE convert(varchar(10), schedule_date, 102) = convert(varchar(10), getdate()+1, 102)"
    } else {
      sql_query <- "SELECT * 
                      FROM QGenda_Schedules_View 
                      WHERE convert(varchar(10), schedule_date, 102) = convert(varchar(10), getdate(), 102)"
    }
  } else {
    sql_query <- sprintf("SELECT * 
                    FROM QGenda_Schedules_View 
                    WHERE cast(schedule_date as DATE) = cast('%s' as date)", date)
  }

  faculty_today <- mdb_query(qgenda_con, sql_query)
  DBI::dbDisconnect(qgenda_con)

  if (service == "Body") {
    faculty_today %<>%
      left_join(min_exams, by = "schedule_Names") %>%
      filter(!is.na(min_exams) | str_detect(task_name, "DOD|DMT|Late|Weekend"))

    # Fix if User Name etc. are missing in the QGendaViews SQL DB:
    if (nrow(faculty_today[which(is.na(faculty_today$`User Name`) | faculty_today$`Provider Number` == ""), ]) > 0) {
      fac_ms <- faculty_today[which(is.na(faculty_today$`User Name`) | faculty_today$`Provider Number` == ""), c("User Name", "Email", "Provider Number", "Radiologist RIS ID")]
      fac_ms %<>% left_join(fac_dmts_ct[, c("User Name", "Email", "Provider Number", "Radiologist RIS ID")], by = "Provider Number") %>%
        select(-c(`User Name.x`:`Radiologist RIS ID.x`)) %>%
        rename(
          `User Name` = `User Name.y`,
          `Email` = `Email.y`,
          `Provider Number` = `Provider Number.y`,
          `Radiologist RIS ID` = `Radiologist RIS ID.y`
        ) %>%
        mutate(`Provider Number` = as.character(`Provider Number`), `Radiologist RIS ID` = as.character(`Radiologist RIS ID`))

      faculty_today[which(is.na(faculty_today$`User Name`) | faculty_today$`Provider Number` == ""), c("User Name", "Email", "Provider Number", "Radiologist RIS ID")] <- fac_ms
    }
    faculty_today %<>% filter(`User Name` %in% fac_dmts$`User Name`)
    return(faculty_today)
  } else if (service == "Neuro") {
    faculty_today %<>%
      left_join(min_exams_neuro, by = "schedule_Names") %>%
      filter(!is.na(min_exams)) %>%
      filter(`User Name` %in% fac_dmts_neuro$`User Name`)
  }
}

#' Subtract DOD and DMT duties from min_exams
#'
#' Used in shift_rads and xover_rads to adjust
#' min_exams for DOD and DMT duties
#'
#' @param faculty_today Dataframe as returned from prep_readers()
#' @param shift "am" or "pm"
#'
#' @import dplyr
#' @importFrom stringr str_detect
#'
#' @return
#' @noRd
adj_dod_dmt <- function(faculty_today, shift) {
  shift_dmts <- dmt_meetings %>%
    filter(shift == shift) %>%
    pull(dmt_name) %>%
    paste0(collapse = "|")

  shiftSHIFT <- paste(tolower(shift), toupper(shift), sep = "|")

  if (nrow(filter(faculty_today, str_detect(task_name, "DOD"))) > 0) {
    DODs <- faculty_today %>%
      filter(str_detect(task_name, "DOD") & str_detect(task_name, shiftSHIFT)) %>%
      pull(Name)
    faculty_today[which(faculty_today$Name %in% DODs), ]$min_exams <- faculty_today[which(faculty_today$Name %in% DODs), ]$min_exams - 2
  }

  shift_dmts <- dmt_meetings[which(dmt_meetings$shift == shift), ]$dmt_name %>%
    paste(collapse = "|")

  if (nrow(filter(faculty_today, str_detect(task_name, shift_dmts))) > 0) {
    DMTs <- faculty_today %>%
      filter(str_detect(task_name, shift_dmts)) %>%
      pull(Name)
    faculty_today[which(faculty_today$Name %in% DMTs), ]$min_exams <- faculty_today[which(faculty_today$Name %in% DMTs), ]$min_exams - 3
  }

  return(faculty_today)
}

#' Prepare shift readers
#'
#' Takes dataframe returned from "prep_readers" and
#' selects readers for given shift or modality.
#' Returns a dataframe.
#'
#' @param modality Can be either "CT" or "MR"
#' @param shift Can be either "am", "pm", "Evening Shift" or "Night Shift"
#' @param date Optional parameter to get readers of a specific date (format YYYY-MM-DD)
#' @param service Defaults to "Body", can alternatively be "Neuro" or "Nuclear"
#' @param dmt_df Reference datarame with dmts for faculty (defaults to fac_dmts provided with package)
#' @param faculty Default = TRUE. When FALSE returns CT or MR fellows.
#' @param pin_test Whether to use test pin names. Defaults to FALSE.
#' @param only_carryover Default = FALSE at the beginning of every shift.
#' If true does not add any min_exams (used by CR and cleanup)
#' @param tomorrow Gets tomorrow's shift (defaults to FALSE = today's shift).
#' Used for testing in the evening.
#'
#' @import dplyr stringr
#' @importFrom emo ji
#'
#' @export

shift_rads <- function(modality = NULL, shift, date = NULL, service = "Body", dmt_df = fac_dmts, faculty = TRUE,
                       pin_test = FALSE, only_carryover = FALSE, tomorrow = FALSE) {
  modality <- toupper(modality)

  if (service == "Body") if (!modality %in% c("CT", "MR", "CR") & faculty == TRUE) stop("Select valid modality")
  if (!shift %in% c("am", "pm", "Evening Shift", "Night Shift")) stop("Select valid shift")

  faculty_today <- prep_readers(service = service, tomorrow_shift = tomorrow, date = date)
  late_staff <- faculty_today[which(str_detect(faculty_today$task_name, "Late")), ]$Name %>%
    # Duplicate in xover_rads function, maybe unify functions
    c(faculty_today[which(faculty_today$task_name %in% c("GYN DMT", "Head and Neck DMT", "Lymphoma DMT", "Colorectal DMT", "Mixed Tumor DMT")), ]$Name)

  if (service == "Neuro") {
    shift_df <- faculty_today %>%
      filter(str_detect(tolower(task_name), shift)) %>%
      left_join(fac_dmts_neuro %>%
        select(`Provider Number`, ny.license:pref.3) %>%
        # Change once data is cleaned up:
        filter(!is.na(dmt.0)),
      by = "Provider Number"
      ) %>%
      carryover_weights(pin_test, service) %>%
      mutate(
        ny.license = replace_na(ny.license, "NY"),
        nj.license = replace_na(nj.license, "None"),
        sum_assigned = 0
      )
    return(shift_df)
  }

  if (shift %in% c("Evening Shift", "Night Shift")) {
    return(faculty_today %>%
      filter(schedule_Names == shift) %>%
      left_join(dmt_df %>%
        select(`Provider Number`, ny.license:pref.3) %>%
        filter(modality == modality),
      by = "Provider Number"
      ) %>%
      carryover_weights(pin_test) %>%
      mutate(
        ny.license = replace_na(ny.license, "NY"),
        nj.license = replace_na(nj.license, "None"),
        sum_assigned = 0
      ) %>% {
        if (modality == "MR") filter(., !is.na(dmt.0)) else filter(., is.na(dmt.0))
      })
  }


  if (only_carryover) {
    faculty_today$min_exams <- 0
    # No cleanup round for main CT readers
    faculty_today %<>% filter(!task_name %in% c("Body - CT am", "Body - CT pm", "Body - CT** am", "Body - CT** pm"))
  } else {
    faculty_today <- adj_dod_dmt(faculty_today, shift)
  }

  faculty_today %<>% mutate(swim = ifelse(Name %not_in% late_staff, TRUE, FALSE))


  if (faculty == TRUE) {
    dmt_df <- filter(dmt_df, mod == modality)
    if (modality == "CT") {
      shift_df <- faculty_today %>%
        filter(
          str_detect(task_name, "CT|PED|Weekend") | str_detect(schedule_Names, "GI am"),
          str_detect(task_name, shift) | str_detect(task_name, "Weekend"),
          !str_detect(task_name, "PET|Cardiac")
        )
    } else if (modality == "MR") {
      shift_df <- faculty_today %>%
        filter(
          str_detect(task_name, "MR|Weekend"),
          str_detect(task_name, shift) | str_detect(task_name, "Weekend"),
          !str_detect(task_name, "PET|Cardiac")
        )
    } else if (modality == "CR") {
      shift_df <- filter(
        faculty_today,
        str_detect(task_name, modality),
        str_detect(task_name, shift)
      ) %>%
        distinct()
    }

    shift_df <- shift_df %>%
      left_join(
        dmt_df %>% select(`Provider Number`, ny.license:pref.3),
        by = "Provider Number"
      )
  } else if (faculty == FALSE) {
    shift_df <- faculty_today %>%
      filter(str_detect(task_name, "Fellow - CT|Fellow - MR")) %>%
      left_join(
        dmt_df %>% select(`Provider Number`, ny.license:pref.3),
        by = "Provider Number"
      ) %>%
      select(-c(`Provider Number`, `User Name`)) %>%
      left_join(
        fellow_table %>% select(`Provider Number`, `Provider Number`, `User Name`)
      ) %>%
      distinct(`Provider Number`, .keep_all = TRUE)
  }

  shift_df <- carryover_weights(shift_df, pin_test) %>%
    mutate(
      ny.license = replace_na(ny.license, "NY"),
      nj.license = replace_na(nj.license, "None")
    ) %>%
    mutate(sum_assigned = 0) %>%
    filter(
      !is.na(`User Name`),
      !is.na(`Provider Number`)
    )

  if (any(is.na(shift_df$min_exams))) {
    warning("NAs in min_exams: ", emo::ji("clap"), "update", emo::ji("clap"), "yo", emo::ji("clap"), "tables", emo::ji("clap"), ": ", shift_df[which(is.na(shift_df$min_exams)), ]$schedule_Names)
    shift_df %<>% mutate(min_exams = replace_na(min_exams, 4)) %>%
      slice_min_exams()
  }
  return(shift_df)
}

#' Prepare crossover readers (CT<>MR)
#'
#' Takes dataframe returned from "prep_readers" and selects readers
#' from one modality who still have capacity and gives back the other
#' DMT and vice versa.
#' Returns a dataframe.
#'
#' @param modality Can be either "CT" or "MR". Will select those DMT categories but the opposite shift on QGenda.
#' @param shift Can be either "am" or "pm"
#' @param date Optional parameter to get readers of a specific date (format YYYY-MM-DD)
#' @param dmt_df Reference datarame with dmts for faculty (defaults to fac_dmts provided with package)
#' @param pin_test Whether to use test pin names. Defaults to FALSE.
#' @param only_carryover Default = TRUE for cleanup
#' @param tomorrow Default = FALSE (= today's roster)
#'
#' @import dplyr stringr
#' @importFrom emo ji
#'
#' @export

xover_rads <- function(modality, shift, date = NULL, dmt_df = fac_dmts,
                       pin_test = FALSE, only_carryover = TRUE,
                       tomorrow = FALSE) {
  modality <- toupper(modality)

  if (!modality %in% c("CT", "MR")) stop("Select valid modality")

  if (!shift %in% c("am", "pm")) stop("Select valid shift")

  faculty_today <- prep_readers(tomorrow_shift = tomorrow, date = date)

  if (only_carryover) {
    faculty_today$min_exams <- 0
    # No cleanup round for main CT readers
    faculty_today %<>% filter(!task_name %in% c("Body - CT am", "Body - CT pm", "Body - CT** am", "Body - CT** pm"))
  } else {
    faculty_today <- adj_dod_dmt(faculty_today, shift)
  }

  late_staff <- faculty_today[which(str_detect(faculty_today$task_name, "Late")), ]$Name %>%
    # Duplicate in shift_rads function, maybe unify functions
    c(faculty_today[which(faculty_today$task_name %in% c("GYN DMT", "Head and Neck DMT", "Lymphoma DMT", "Colorectal DMT", "Mixed Tumor DMT")), ]$Name)

  faculty_today %<>% mutate(swim = ifelse(Name %not_in% late_staff, TRUE, FALSE))

  dmt_df <- filter(dmt_df, mod == modality)

  modality <- ifelse(modality == "CT", "MR", "CT") # Swaps modalities (important for "if"s below)

  shift_df <- faculty_today %>%
    filter(
      str_detect(task_name, sprintf("%s|PED|Weekend", modality)),
      str_detect(task_name, shift),
      !str_detect(task_name, "PET|Cardiac")
    )

  shift_df <- shift_df %>%
    left_join(
      dmt_df %>% select(`Provider Number`, ny.license:pref.3),
      by = "Provider Number"
    )

  if (modality == "CT") shift_df <- filter_at(shift_df, vars(dmt.0:dmt.4), any_vars(not_na(.)))

  shift_df <- carryover_weights(shift_df, pin_test) %>%
    mutate(
      ny.license = replace_na(ny.license, "NY"),
      nj.license = replace_na(nj.license, "None")
    ) %>%
    mutate(sum_assigned = 0) %>%
    filter(
      !is.na(`User Name`),
      !is.na(`Provider Number`)
    )

  if (any(is.na(shift_df$min_exams))) {
    warning("NAs in min_exams: ", emo::ji("clap"), "update", emo::ji("clap"), "yo", emo::ji("clap"), "tables", emo::ji("clap"), ": ", shift_df[which(is.na(shift_df$min_exams)), ]$schedule_Names)
    shift_df %<>% mutate(min_exams = replace_na(min_exams, 11.5)) %>%
      slice_min_exams()
  }
  return(shift_df)
}
