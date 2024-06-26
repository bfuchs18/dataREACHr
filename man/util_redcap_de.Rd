% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_redcap_de.R
\name{util_redcap_de}
\alias{util_redcap_de}
\title{util_redcap_de: Organize double-entry data from REDCap (called within proc_redcap.R)}
\usage{
util_redcap_de(data, agesex_data, return_data = TRUE)
}
\arguments{
\item{data}{double-entry data}

\item{agesex_data}{dataframe with participant_id, v1_age (visit 1 age) and v5_age (visit 5 age), sex}

\item{return_data}{If return_data is set to TRUE, will return a list including: mri_visit_data, tictoc_data, wasi_data, dexa_data, anthro_data (wide and long), intake_data (wide and long)}
}
\description{
This function organizes REDCap double entry data data
}
