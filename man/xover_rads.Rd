% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_readers.R
\name{xover_rads}
\alias{xover_rads}
\title{Prepare crossover readers (CT<>MR)}
\usage{
xover_rads(
  modality,
  shift,
  date = NULL,
  dmt_df = fac_dmts,
  pin_test = FALSE,
  only_carryover = TRUE,
  tomorrow = FALSE
)
}
\arguments{
\item{modality}{Can be either "CT" or "MR". Will select those DMT categories but the opposite shift on QGenda.}

\item{shift}{Can be either "am" or "pm"}

\item{date}{Optional parameter to get readers of a specific date (format YYYY-MM-DD)}

\item{dmt_df}{Reference datarame with dmts for faculty (defaults to fac_dmts provided with package)}

\item{pin_test}{Whether to use test pin names. Defaults to FALSE.}

\item{only_carryover}{Default = TRUE for cleanup}

\item{tomorrow}{Default = FALSE (= today's roster)}
}
\description{
Takes dataframe returned from "prep_readers" and selects readers
from one modality who still have capacity and gives back the other
DMT and vice versa.
Returns a dataframe.
}
