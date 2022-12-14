% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcts_powerscribe.R
\name{ps_close}
\alias{ps_close}
\title{Close Powerscribe API Session}
\usage{
ps_close(test = FALSE)
}
\arguments{
\item{test}{If TRUE uses PS test API. Default is FALSE.}
}
\description{
Closes Powerscribe360 API session. Returns HTTP response code.
}
\examples{
\dontrun{
api_response <- ps_close() # if set previously via Sys.setenv

api_response <- ps_close()
}

}
