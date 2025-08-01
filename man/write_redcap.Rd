% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_redcap.R
\name{write_redcap}
\alias{write_redcap}
\title{write_redcap: Write selected data and json files from processed REDCap data}
\usage{
write_redcap(
  base_wd,
  overwrite = FALSE,
  data_list = "all",
  return_data = FALSE
)
}
\arguments{
\item{base_wd}{(string) full path to directory that contains both the untouchedRaw and bids directories}

\item{overwrite}{(logical) logical indicating whether file should be overwritten}

\item{data_list}{list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
\itemize{
\item{'participants' - BIDS specified participants.tsv file}
\item{'anthropometrics' - height, weight, and computed anthropometric data}
\item{'demographics' - compiled demographic data}
\item{'dxa' - verified DXA data}
\item{'household' - compiled demographicinformation about houshold}
\item{'infancy' - compiled demographic information related to infancy}
\item{'intake' - compiled intake data with computed intake values}
\item{'mri_visit' - MRI visit information including Freddy and CAMS}
\item{'parent_updates' - all visit updates}
\item{'researcher_notes' - all visit notes}
\item{'audit' - Alcohol Use Disorders Identification Test}
\item{'bes' - Binge Eating Scale}
\item{'bisbas' - Behavioral Inhibition System/Behavioral Activation System}
\item{'brief2' - Behavioral Rating Inventory of Executive Function-2}
\item{'cbq' - Child Behavior Questionnaire}
\item{'cchip' - Community Childhood Hunger Identification Project}
\item{'cebq' - Children's Eating Behavior Questionnaire}
\item{'cfpq' - Comprehensive Feeding Practices Questionnaire}
\item{'cfq' - Child Feeding Questionnaire}
\item{'chaos' - Confusion, Hubbub, and Order Scale}
\item{'class' - \emph{need}}
\item{'cshq' - Children Sleep Habits Questionnaire}
\item{'debq' - Dutch Eating Behavior Questionnaire}
\item{'efcr' - External Food Cue Responsiveness Scale}
\item{'ffbs' - Family Food Behavior Survey}
\item{'fsq' - \emph{need}}
\item{'hfi' - Fulkerson Home Food Inventory}
\item{'hfias' - Household Food Insecurity Access Scale}
\item{'hfssm' - U.S. Household Food Security Survey Module}
\item{'kbas' - Kid's Brand Awareness Scale}
\item{'lbc' - Lifestyle Behavior Checklist}
\item{'loc' - Loss of Control-Eating Questionnaire}
\item{'pmum' - Problematic Media Use Measure \emph{need}}
\item{'pptq' - Pictorial Personality Traits Questionnaire for Children}
\item{'pss' - Perceived Stress Scale}
\item{'pstca' - \emph{need}}
\item{'puberty' - combination of Tanner and Pubertal Rating Scale}
\item{'pwlb' - Parent Weight-Loss Behavior Questionnaire}
\item{'rank' - Parent ranking of foods sources? \emph{need}}
\item{'scpf' - tructure and Control in Parent Feeding Questionnaire}
\item{'sic' - Stress in Children Questionnaire \emph{need}}
\item{'sleeplog' - Week long sleep log}
\item{'spsrq' - Sensitivity to Punishment and Sensitivity to Reward Questionnaire}
\item{'stq' - Screen Time Questionnaire \emph{need}}
\item{'tfeq' - Three Factor Eating Questionnaire}
}}

\item{return_data}{(logical) return data to working environment. Default = FALSE}
}
\value{
Does not return anything
}
\description{
This function:
\itemize{
\item{1) Calls proc_redcap function to get clean and compiled data and metadata}
\item{2) Exports all or select BIDS-compliant .tsv and .json files into bids/phenotype}
}
}
\details{
To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
}
\examples{

\dontrun{
write_redcap(base_wd, overwrite = FALSE, data_list = 'all')

}


}
