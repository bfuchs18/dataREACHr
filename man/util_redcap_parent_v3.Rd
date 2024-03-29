% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_redcap_parent_v3.R
\name{util_redcap_parent_v3}
\alias{util_redcap_parent_v3}
\title{util_redcap_parent_v3: Organize parent visit 2 data from REDCap (called within proc_redcap.R)}
\usage{
util_redcap_parent_v3(data, return_data = TRUE)
}
\arguments{
\item{data}{data from REDCap event parent_visit_3_arm_1'}

\item{return_data}{If return_data is set to TRUE, will return a list including:
\enumerate{
\item clean raw parent 1 datasets
\item meta-data/.json for each dataset
}}
}
\description{
This function organizes REDCap data from REDCap visit data, event parent_visit_3_arm_1
}
