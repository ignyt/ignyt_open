#' Reference table: Locations mapped to state
#'
#' State of different hospital locations used to avoid assigment of
#' NJ exams to physicians who do not have a license
#'
#' @format A data frame with 12 rows and 2 variables:
#' \describe{
#'   \item{location_code}{RIS code of different locations}
#'   \item{state_code}{State of the location. Currently only 2 states hardcoded: NY for New York or NJ for New Jersey locations}
#' }

loc_by_state <- tibble::tibble(
  "location_code" = c(
    "HOSP", "HOSP301", "HOSP72", "HOSPMRDR", "HOSP74",
    "HOSPEAST", "HOSPSHIRE", "HOSP53"
  ),
  "state_code" = c(rep("NY", 5), rep("NJ", 3))
)

# usethis::use_data(loc_by_state, overwrite = TRUE)
