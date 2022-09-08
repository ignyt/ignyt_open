#' Get simulated radiographs of a 15 minute window
#'
#' Function returns the worklist on a given day
#' and time, including examinations the 15 minutes before.
#' (Assumes that CR Rmd is run in 15 minute intervals and
#' everything before has been assigned)
#'
#' @param day_n Which day should be selected (1:7)
#' @param time What timepoint should be simulated (character with format HH:MM)
#'
#' @importFrom dplyr %>% mutate filter
#' @importFrom lubridate as_date minutes hm ymd_hm
#'
#' @export

sim_crs <- function(day_n, time) {
  if (!day_n %in% 1:7) stop("Select valid test day (1:7)")
  if (is.na(hm(time))) stop("Provide valid time in 24h format (hh:mm)")

  cut_off <- paste(testweek_days[day_n], time, sep = ", ") %>% ymd_hm()
  last_assignment <- cut_off - minutes(15)

  crs <- sim_worklist("all") %>%
    filter(
      modality_code == "CR",
      between(ris_exam_end_dt, last_assignment, cut_off)
    )

  crs
}

#' Get a simulated worklist
#'
#' Function returns either the worklist of a single day
#' or of the whole week.
#'
#' @param day_n Which day should be returned (1:7), "all" returns whole week.
#'
#' @importFrom dplyr %>% mutate filter
#' @importFrom lubridate as_date as_datetime
#'
#' @export

sim_worklist <- function(day_n) {
  if (day_n == "all") {
    test_week %>%
      mutate(ris_finalized_dt = as_datetime("1900-01-01")) %>%
      clean_worklist()
  } else {
    if (!day_n %in% 1:7) stop("Select valid test day (1:7)")
    # Internal dataset:
    test_week %>%
      filter(
        ris_exam_end_dt <= testweek_days[day_n],
        ris_finalized_dt >= testweek_days[day_n],
      ) %>%
      mutate(
        ris_finalized_dt = as_datetime("1900-01-01"),
        ris_accession_number = as.character(ris_accession_number),
        ris_mrn = as.character(ris_mrn)
        ) %>%
      clean_worklist()
  }
}

#' Simulate assignment
#'
#' Assings an "OK" to all exams by default. Number of exams for simulated
#' failures can be specified as the only parameter.
#' Expects and returns data frame (analogous to core assign function).
#'
#' @param df (tibbles) as produced by "prep" functions.
#' @param n_fail Number of cases that should be simulated to fail (picked randomly)
#'
#' @importFrom dplyr mutate filter
#'
#' @export

sim_assign <- function(df, n_fail = 0) {
  df %<>% mutate(assigned_status = "not yet")
  all_rows <- 1:nrow(df)
  if (n_fail > 0) {
    if (n_fail > nrow(df)) stop("Too many simulated failures or too little test data")
    rows_fail <- sample(all_rows, n_fail)
    rows_success <- setdiff(all_rows, rows_fail)
    for (r in rows_fail) {
      df[r, ]$assigned_status <- "Simulated failure"
    }
  } else {
    rows_success <- all_rows
  }
  for (r in rows_success) {
    df[r, ]$assigned_status <- "OK"
  }
  return(df)
}

#' Simulate QGenda assignment
#'
#' Gets QGenda tibble for given day (1:7).
#'
#' @param day_n Which day should be returned (1:7)
#' @param mod Modality to filter by (can be empty)
#'
#' @importFrom dplyr %>% filter
#' @importFrom stringr str_detect
#' @importFrom lubridate as_date
#'
#' @aliases sim_fac_day_mod
#' @export

sim_fac_day <- function(day_n, mod = NULL) {
  # Internal dataset:
  test_faculty <- test_faculty %>% filter(schedule_date == testweek_days[day_n])
}

sim_fac_day_mod <- function(day_n, mod) {
  # Internal dataset:
  test_faculty <- test_faculty %>% filter(schedule_date == testweek_days[day_n])

  if (!missing(mod)) {
    test_faculty <- test_faculty %>% filter(stringr::str_detect(schedule_Names, mod))
  }
}

#' Simulate preparation of shift readers
#'
#' Takes dataframe returned from "sim_fac_day" and
#' selects readers for given shift or modality.
#' Returns a dataframe.
#'
#' @param day_n Which day should be returned (1:7)
#' @param modality Can be either "CT" or "MR"
#' @param shift Can be either "am", "pm", "Evening Shift" or "Night Shift"
#' @param dmt_df Reference datarame with dmts for faculty (select fac_ref_dmts _ct or _mr provided with package)
#' @param faculty Default = TRUE. When FALSE returns CT or MR fellows.
#' @param pin_board Passed to pin functions, defaults to "none" (no pinboard used)
#' @param only_carryover Default = FALSE at the beginning of every shift.
#' If true does not add any min_exams (used by CR and cleanup)
#'
#'
#' @export

sim_shift_rads <- function(day_n, modality, shift, dmt_df = fac_dmts, faculty = TRUE, pin_board = "none", only_carryover = FALSE) {
  if (!day_n %in% 1:7) stop("Select valid test day (1:7)")
  modality <- toupper(modality)
  if (modality %not_in% c("CT", "MR", "CR")) stop("Select valid modality")
  if (shift %not_in% c("am", "pm", "Evening Shift", "Night Shift")) stop("Select valid shift")

  faculty_today <- sim_fac_day(day_n) %>% select(-c(ny.license:mr_dmt.4))

  if (shift %in% c("Evening Shift", "Night Shift")) {
    return(
      faculty_today %>%
       filter(schedule_Names == shift) %>%
       left_join(dmt_df %>%
        select(`Provider Number`, ny.license:pref.3) %>%
        filter(modality == "MR"),
        by = "Provider Number"
        ) %>% 
       mutate(swim = TRUE)
      )
  }

  if (only_carryover) faculty_today$min_exams <- 0
  
  faculty_today %<>% mutate(swim = ifelse(str_detect(task_name, "Late"), FALSE, TRUE))

  if (faculty == TRUE) {
    if (modality == "CT") {
      dmt_df <- filter(dmt_df, mod == "CT")
      shift_df <- faculty_today %>%
        filter(
          str_detect(task_name, sprintf("%s|PED", modality)),
          str_detect(task_name, shift),
          !str_detect(task_name, "PET|Cardiac")
        )
    } else if (modality == "MR") {
      dmt_df <- filter(dmt_df, mod == "MR")
      shift_df <- faculty_today %>%
        filter(
          str_detect(task_name, modality),
          str_detect(task_name, shift),
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
    shift_df %<>% left_join(
      dmt_df %>% select(`Provider Number`, ny.license:pref.3),
      by = "Provider Number"
    )
  } else if (faculty == FALSE) {
    shift_df <- faculty_today %>%
      filter(str_detect(task_name, sprintf("Fellow - %s", toupper(modality))))
  }

  if (pin_board != "none") shift_df %<>% carryover_weights(test_pin = TRUE, pin_board = pin_board)

  shift_df %<>% mutate(
    ny.license = replace_na(ny.license, "NY"),
    nj.license = replace_na(nj.license, "None")
  ) %>%
    filter(!is.na(min_exams)) %>% 
    mutate(sum_assigned = 0)
}
