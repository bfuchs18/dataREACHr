% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_format_dxa.R
\name{util_format_dxa}
\alias{util_format_dxa}
\title{util_format_dxa: process double-entered DXA data for REACH visit 1 and 5}
\usage{
util_format_dxa(dxa_data)
}
\arguments{
\item{dxa_data}{dxa extracted from data from REDCap events}
}
\description{
This function process Ranking Food Item Questionnaire data
}
\examples{

# process data
dxa_formatted <- util_format_dxa(dxa_data)

}
\seealso{
\code{\link[=util_redcap_de]{util_redcap_de()}}
}
