
#' Divide radiographs core function
#'
#' This function assigns up to 7 exams to the primary
#' CR attending and the rest to attendings on the overflow
#' list (usually CT and MR) with the highest number of
#' exams left in the pinned balance sheet.
#'
#' @param cr_df  Worklist
#' @param atts CT/MRI attendings; dataframe with multiple rows
#' @param bypass_ps Bypasses check of PS360 database (kicks out dictated/queued exams). Defaults to FALSE.
#'
#' @usage divide_cr(cr_df, atts, bypass_ps)
#'
#' @importFrom tidyr unnest
#' @importFrom stringr str_detect
#' @import dplyr
#'
#' @export

divide_cr <- function(cr_df, atts, bypass_ps = FALSE) {
  cr_df <- cr_df %>% filter(
    modality_code == "CR",
    patient_status_code == "O",
    !str_detect(patient_location_code, "UCC|SCC|ER")
  )

  if (nrow(cr_df) == 0 | is.null(nrow(cr_df))) {
    return(cr_df)
  }

  if (!is.logical(bypass_ps)) {
    warning("Invalid bypass_ps value, falling back to FALSE")
    bypass_ps <- FALSE
  }

  if (bypass_ps == FALSE) {
    cr_df %<>% left_join(
      ps_get_status(cr_df$ris_accession_number),
      by = "ris_accession_number"
    )
    cr_df %<>% filter(ps_status == "Completed")
  } else if (bypass_ps == TRUE) {
    cr_df %<>% mutate(ps_status = "Completed")
  }

  if (nrow(cr_df) == 0 | is.null(nrow(cr_df))) {
    return(NA)
  }

  cr_df %<>% mutate(state_code = ifelse(is.na(state_code), "NJ", state_code)) %>%

    atts() %<>% mutate(assigned_exams = NA)
  atts$assigned_exams %<>% as.list()

  # Bone exclusive cases (skeletal surveys only)

  if (any(atts$dmt.1 == "bone") & nrow(cr_df %>% filter(!stringr::str_detect(exam_description, "SURVEY"))) > 0) {
    att[which(att$dmt.1 == "bone"), ][which.max(att[which(att$dmt.1 == "bone"), ]$min_exams), ]$assigned_exams[[1]] <- cr_df %>% filter(!stringr::str_detect(exam_description, "SURVEY"))
    att[which(att$dmt.1 == "bone"), ][which.max(att[which(att$dmt.1 == "bone"), ]$min_exams), ]$min_exams <- att[which(att$dmt.1 == "bone"), ][which.max(att[which(att$dmt.1 == "bone"), ]$min_exams), ]$min_exams - (cr_df %>% filter(!stringr::str_detect(exam_description, "SURVEY")) %>% pull(exam_weighting) %>% sum())
    cr_df %<>% filter(!stringr::str_detect(exam_description, "SURVEY"))
  } else {
    cr_df %<>% filter(!stringr::str_detect(exam_description, "SURVEY"))
  }

  # Future: Make other Muscl exams conditional

  # Now PEDs

  if (nrow(cr_df[which(cr_df$modifier == "PED"), ]) > 0) {
    if (nrow(atts[which(str_detect(atts$schedule_Names, "PED")), ]) > 0) {
      atts[which(str_detect(atts$schedule_Names, "PED")), ]$assigned_exams[[1]] <- cr_df[which(cr_df$modifier == "PED"), ]
      atts[which(str_detect(atts$schedule_Names, "PED")), ]$min_exams <- atts[which(str_detect(atts$schedule_Names, "PED")), ]$min_exams - sum(cr_df[which(cr_df$modifier == "PED"), ]$exam_weighting, na.rm = TRUE)
      cr_df <- cr_df[-which(cr_df$modifier == "PED"), ]
    }
  }

  if (nrow(cr_df) == 0) {
    return(tidyr::unnest(atts, assigned_exams))
  }

  # Rest:

  for (i in 1:nrow(cr_df)) {
    lic <- cf_df[i, ]$state_code
    ix <- 1:nrow(atts)
    if (lic == "NJ") ix <- which(atts$nj.license == "NJ")
    atts[ix, ][which.max(atts[ix, ]$min_exams), ]$assigned_exams[[1]] %<>% force_rbind(cf_df[i, ])
    atts[ix, ][which.max(atts[ix, ]$min_exams), ]$min_exams <- atts[ix, ][which.max(atts[ix, ]$min_exams), ]$min_exams - cf_df[i, ]$exam_weighting
  }

  return(tidyr::unnest(atts, assigned_exams))
}
