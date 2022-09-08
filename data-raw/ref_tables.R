library(readr)
library(dplyr)
library(tidyr)
library(usethis)

#' Reference tables: DMT assignment of faculty members
#'
#' DMT assignment of faculty members. fac_dmts is a joined version of the ct and mr tables.
#' Variables are dictated by the clinical roster API or database. The most important columns
#' used by ignyt are indicated below.
#' The DMT codes are different for CT and MRI due to differences in protocols/workflow.
#' Bone/Soft tissue sarcoma DMT needs to be in dmt.1 if present.
#' "fellow_table" is an extra table containing all current fellows.
#'
#'
#' @format A data frame with n(faculty) rows and 18/19 variables:
#' \describe{
#'   \item{mod}{Modality. Only exists in "fac_dmts"}
#'   \item{Provider Number}{Used to assign report in PowerScribe via PS360 API}
#'   \item{nj.license}{"NJ" if present, other statuses mean no license (yet)}
#'   \item{dmt.0}{For MRI. Can only be "general" or NA}
#'   \item{dmt.1:dmt:4}{For CT and MRI, but slightly different codes for each}
#'   \item{pref.1:pref.3}{Preferences of readers. Only used for CT optimization.}
#'   ...
#' }

fellow_table <- read_csv2("data-raw/fellows.csv")

fac_dmts <- read_csv2("data-raw/dmt_faculty.csv.gz") %>%
  ignyt:::pivot_dmts(fellow_table)

fac_dmts_ct <- filter(fac_dmts, mod == "CT") %>% select(-dmt.0)
fac_dmts_mr <- filter(fac_dmts, mod == "MR")

#' References for DMT labeling of exams
#'
#' match_* are lists containing strings for DMT matching by exam description (desc)
#' and by the given history (hx). ref_dmts is a tibble with referring physicians
#' and their DMT. For now only referrers with unambiguous DMT (dmt.1) are used.
#'
#' @format Two lists and a data frame
#' \describe{
#'   \item{match_desc_list}{Named list, names = DMTs, elements = strings for matching}
#'   \item{match_hx_list}{Named list, names = DMTs, elements = strings for matching}
#'   \item{ref_dmts}{106 obs. of 7 variables. Multiple DMTs per ref. possible, ignyt only uses ref. with a single DMT for now.}
#' }
ref_dmts <- read_csv2("data-raw/dmt_ref_phys.csv") %>%
  filter(!is.na(dmt.1))

#' References for PEDs flag
#'
#' Vector matching "referring_md_name" from ris data.
#' List can be dynamically updated via database query.
#'
#' @format Vector (source CSV has several columns)
ref_peds <- read_csv2("data-raw/peds_refs.csv")

#' Reference tables: Exam weights and CT exclusive protocols
#'
#' Reference tibble containing weighting factors. More difficult exams/with
#' more contrast phases, sequences etc. will have a higher weighting to
#' ensure fair workload distribution.
#'
#' @format A data frame with 2 variables (7 additional in source csv):
#' \describe{
#'   \item{exam_code}{Unique exam code for matching with worklists}
#'   \item{exam_weighting}{Weighted units for examination}
#' }
#'
#' Second tibble contains nested dataframe of DMTs and their exclusive exam_codes
#'
#' @format A nested data frame
#' \describe{
#'   \item{dmt}{DMT code}
#'   \item{data}{Contains 1-column data frames with exam codes}
#' }

exam_weights <- read_delim(
  "data-raw/exam_weighting.csv",
  ";",
  locale = locale(decimal_mark = ".")
)

ct_exclusives <- exam_weights %>%
  filter(!is.na(dmt) | dmt == "should not be assigned") %>%
  select(-exam_weighting) %>%
  tidyr::nest(data = c(exam_code))

exam_weights <- select(exam_weights, -dmt)

#' Reference table: Minimal exam units
#'
#' Reference tibble containing minimal exam (weighted units) per shift
#'
#' @format A data frame with 140 rows and 3 variables (2 important for ignyt):
#' \describe{
#'   \item{schedule_Names}{Shift name in QGenda}
#'   \item{min_exams}{Exams to be read}
#'   ...
#' }

min_exams <- read_delim(
  "data-raw/min_exams_per_shift.csv",
  ";",
  locale = locale(decimal_mark = ".")
)

#' Reference table: DMT meetings
#'
#' Contains information in which shift the DMT meetings happen
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'   \item{dmt_name}{DMT name in QGenda}
#'   \item{shift}{am or pm}
#'   ...
#' }

dmt_meetings <- read_csv2("data-raw/dmt_meetings_shift.csv")

# use_data(fac_dmts_ct, overwrite = TRUE)
# use_data(fac_dmts_mr, overwrite = TRUE)
# use_data(fac_dmts, overwrite = TRUE)
# use_data(exam_weights, overwrite = TRUE)
# use_data(min_exams, overwrite = TRUE)
# use_data(ref_dmts, overwrite = TRUE)
