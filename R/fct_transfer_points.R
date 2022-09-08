#' Transfer assigned exam points between two data.frames
#'
#' Used to transfer the two columns min_exams and sum_assigned
#' from data.frame A to data.frame B. Column "Name" is used
#' as a unique identifier.
#' Use when perfoming multiple assignments of the same reader
#' within a single document without updating the balance sheet.
#'
#' @param df_a Dataframe A (origin)
#' @param df_b Dataframe B (target)
#'
#' @importFrom dplyr slice left_join arrange select rename %>%
#'
#' @export

transfer_points <- function(df_a, df_b) {
  df_a %<>% select(Name, min_exams, sum_assigned) %>%
    rename(carryover_min = min_exams, carryover_assigned = sum_assigned) %>%
    group_by(Name) %>%
    slice(which.min(carryover_min)) %>%
    ungroup()
  df_joined <- left_join(df_b, df_a, by = "Name") %>% filter(!is.na(min_exams))
  df_joined[which(!is.na(df_joined$carryover_min)), c("min_exams", "sum_assigned")] <- df_joined[which(!is.na(df_joined$carryover_min)), c("carryover_min", "carryover_assigned")]
  df_joined %<>% select(-c(carryover_min, carryover_assigned))
}
