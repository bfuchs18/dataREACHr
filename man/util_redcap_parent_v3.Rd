% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_redcap_parent_v3.R
\name{util_redcap_parent_v3}
\alias{util_redcap_parent_v3}
\title{util_redcap_parent_v3: Organize parent visit 3 data from REDCap}
\usage{
util_redcap_parent_v3(data, date_data)
}
\arguments{
\item{data}{data from REDCap event parent_visit_3_arm_1}

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
\value{
Will return a list including:
\itemize{
\item{clean raw and scored parent visit 3 datasets}
\item{meta-data formated as json for each dataset}
}

Returned data includes:
\itemize{
\item{visit_data_parent}
\item{spsrq_scored}
\item{pwlb_scored}
\item{tfeq_scored}
\item{class_score}
\item{bisbas_scored}
\item{pstca_scored}
\item{debq_scored}
\item{scpf_scored}
}
}
\description{
This function organizes REDCap data from REDCap visit data, event parent_visit_3_arm_1
}
\examples{

# process REDCap data
parent_visit3_list <- util_redcap_parent_v3(data, date_data)

\dontrun{
}

}
\seealso{
\code{\link[=proc_redcap]{proc_redcap()}}
}
