% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_redcap_parent_v1.R
\name{util_redcap_parent_v1}
\alias{util_redcap_parent_v1}
\title{util_redcap_parent_v1: Organize parent visit 1 data from REDCap}
\usage{
util_redcap_parent_v1(data, date_data)
}
\arguments{
\item{data}{data from REDCap event parent_visit_1_arm_1}

\item{date_data}{date data for REDCap visit

Will return a list including:
\itemize{
\item{clean raw and scored parent visit 1 datasets}
\item{meta-data formated as json for each dataset}
}

Returned data includes:
\itemize{
\item{demo_data}
\item{infancy_data}
\item{household_data}
\item{rank_data}
\item{puberty_data}
\item{cfq_data}
\item{cebq_data}
\item{efcr_data}
\item{chaos_data}
\item{pss_data}
\item{lbc_data}
}}
}
\description{
This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
}
\examples{

# process REDCap data
parent_visit1_list <- util_redcap_parent_v1(data, date_data)

\dontrun{
}

}
\seealso{
\code{\link[=proc_redcap]{proc_redcap()}}
}
