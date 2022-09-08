#' divide_trainee core function
#'
#' Creates dataframe with CT's assigned to fellows.
#' Expects dataframes with specific columns as returned
#' by the "prep" functions
#'
#' Returns a dataframe with one row per exam
#'
#' @param worklist,trainees Data frames (tibbles) as produced by "prep" functions.
#' @param n_exams Number of exams to be assigned to each fellow.
#' @param bypass_ps Bypasses check of PS360 database (kicks out dictated/queued exams). Defaults to FALSE.
#'
#' @import dplyr stringr
#' @importFrom purrr map_dfr
#'
#' @export

divide_train <- function(worklist, trainees, n_exams, bypass_ps = FALSE) {
  if (!is.logical(bypass_ps)) {
    warning("Invalid bypass_ps value, falling back to FALSE")
    bypass_ps <- FALSE
  }
  worklist %<>% filter(patient_status_code != "I", !str_detect(patient_location_code, "UCC|SCC|ER"))

  # PS integrity check -----------------------------------------------------

  if (bypass_ps == FALSE) {
    worklist_ps <- left_join(
      worklist,
      ps_get_status(worklist$ris_accession_number),
      by = "ris_accession_number"
    )
    log_not_assigned_ps <- worklist_ps %>% filter(ps_status != "Completed")
    worklist_ps %<>% filter(ps_status == "Completed")
  } else if (bypass_ps == TRUE) {
    worklist_ps <- worklist %>% mutate(ps_status = "Completed")
    log_not_assigned_ps <- worklist_ps[0, ]
  }

  if (nrow(worklist_ps) == 0) {
    return(worklist_ps)
  }

  # Actual assignment -------------------------------------------------------

  train_unnested <- purrr::map_dfr(seq_len(n_exams), ~trainees) %>%
    arrange(schedule_Names)

  ct_mr <- train_unnested %>%
    count(schedule_Names) %>%
    mutate(modality = str_extract(schedule_Names, "CT|MR"))
  train_wl <- worklist_ps %>% filter(wet_read_request == "normal", modality_code %in% c("CT", "MR"))
  if (all(c("MR", "CT") %in% ct_mr$modality)) {
    train_wl <- bind_rows(
      train_wl[which(train_wl$modality_code == "CT"), ][1:ct_mr[which(ct_mr$modality == "CT"), ]$n, ],
      train_wl[which(train_wl$modality_code == "MR"), ][1:ct_mr[which(ct_mr$modality == "MR"), ]$n, ]
    )
  } else if (is.null(ct_mr$modality)) {
    return(train_unnested %<>% mutate(ris_accession_number = "1111111"))
  } else if (ct_mr$modality == "MR") {
    train_wl <- train_wl[which(train_wl$modality_code == "MR"), ][1:ct_mr[which(ct_mr$modality == "MR"), ]$n, ]
  } else if (ct_mr$modality == "CT") {
    train_wl <- train_wl[which(train_wl$modality_code == "CT"), ][1:ct_mr[which(ct_mr$modality == "CT"), ]$n, ]
  } else {
    return(train_unnested %<>% mutate(ris_accession_number = "1111111"))
  }
  if (nrow(train_wl) < nrow(train_unnested)) train_unnested <- train_unnested[1:nrow(train_wl), ]

  train_unnested %<>% bind_cols(train_wl) %>% mutate(dmt_match = 0)
}
