% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_divide_train.R
\name{divide_train}
\alias{divide_train}
\title{divide_trainee core function}
\usage{
divide_train(worklist, trainees, n_exams, bypass_ps = FALSE)
}
\arguments{
\item{worklist, trainees}{Data frames (tibbles) as produced by "prep" functions.}

\item{n_exams}{Number of exams to be assigned to each fellow.}

\item{bypass_ps}{Bypasses check of PS360 database (kicks out dictated/queued exams). Defaults to FALSE.}
}
\description{
Creates dataframe with CT's assigned to fellows.
Expects dataframes with specific columns as returned
by the "prep" functions
}
\details{
Returns a dataframe with one row per exam
}
