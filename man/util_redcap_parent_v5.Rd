% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_redcap_parent_v5.R
\name{util_redcap_parent_v5}
\alias{util_redcap_parent_v5}
\title{util_redcap_parent_v5: Organize parent visit 5 data from REDCap (called within proc_redcap.R)}
\usage{
util_redcap_parent_v5(data, return_data = TRUE)
}
\arguments{
\item{data}{data from REDCap event parent_visit_5_arm_1}

\item{return_data}{If return_data is set to TRUE, will return a list including:
\enumerate{
\item clean raw parent 1 datasets
\item meta-data/.json for each dataset
}}
}
\description{
This function organizes REDCap data from REDCap visit data, event parent_visit_5_arm_1
}
