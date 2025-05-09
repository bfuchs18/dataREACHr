% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deriv_foodview.R
\name{deriv_foodview}
\alias{deriv_foodview}
\title{deriv_foodview: Generate derivative databases for Foodview Task analyses}
\usage{
deriv_foodview(data, file_list)
}
\arguments{
\item{data}{a list of list of dataframes. The top-level list represents individual subjects, and each subject has a sublist of dataframes. Each dataframe contains events data for a specific run of the foodview task for a given sub. A suitable list is returned by proc_task, or can be gathered from files in bids/rawdata}

\item{file_list}{a list filenames, where 1 filename is full path to processed foodview events file for 1 run for 1 sub (exported by util_task_foodview). data OR file_list is required.}
}
\value{
a list with: 1) summary_long_by_cond = a long dataframe with summary data by commerical_condition (metrics calculated across runs), 2) summary_long_by_block = a long dataframe with summary data by block
}
\description{
This function generates FoodView derivative databases from participant-level FoodView data
}
\examples{

\dontrun{

# process task data
base_dir = "/Users/baf44/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/"
task_data <- proc_task(base_wd = base_dir, return_data = TRUE)

# create deriv database from data RETURNED by proc_task
food_view_summary <- deriv_foodview(data = task_data$foodview)

# create deriv database from files EXPORTED by proc_task, using a list of file names
file_list <- list.files(file.path(base_dir, "bids", "rawdata"), pattern = "foodview.*events\\\\.tsv$", recursive = TRUE, full.names = TRUE) # include files across all subs and runs
food_view_summary <- deriv_foodview(file_list = file_list)
}
}
