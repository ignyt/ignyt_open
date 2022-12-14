% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_testing.R
\name{sim_shift_rads}
\alias{sim_shift_rads}
\title{Simulate preparation of shift readers}
\usage{
sim_shift_rads(
  day_n,
  modality,
  shift,
  dmt_df = fac_dmts,
  faculty = TRUE,
  pin_board = "none",
  only_carryover = FALSE
)
}
\arguments{
\item{day_n}{Which day should be returned (1:7)}

\item{modality}{Can be either "CT" or "MR"}

\item{shift}{Can be either "am", "pm", "Evening Shift" or "Night Shift"}

\item{dmt_df}{Reference datarame with dmts for faculty (select fac_ref_dmts _ct or _mr provided with package)}

\item{faculty}{Default = TRUE. When FALSE returns CT or MR fellows.}

\item{pin_board}{Passed to pin functions, defaults to "none" (no pinboard used)}

\item{only_carryover}{Default = FALSE at the beginning of every shift.
If true does not add any min_exams (used by CR and cleanup)}
}
\description{
Takes dataframe returned from "sim_fac_day" and
selects readers for given shift or modality.
Returns a dataframe.
}
