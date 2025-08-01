% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_tasks.R
\name{write_tasks}
\alias{write_tasks}
\title{write_tasks: Write selected data and json files from processed REDCap data}
\usage{
write_tasks(base_wd, overwrite = FALSE, data_list = "all", return_data = FALSE)
}
\arguments{
\item{base_wd}{(string) full path to directory that contains both the untouchedRaw and bids directories}

\item{overwrite}{(logical) logical indicating whether file should be overwritten}

\item{data_list}{list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
\itemize{
\item{'foodview' - fMRI Food Viewing task}
\item{'nihtoolbox' - NIH Toolbox data}
\item{'pit' - Pavlovian Instrumental Transfer task data}
\item{'rrv' - Relative Reinforcing Value of Food task}
\item{'spacegame' - Space Game data (need to finish processing in Matlab)}
\item{'sst' - fMRI Stop-Signal Task data}
}}

\item{return_data}{(logical) return data to working environment. Default = FALSE}
}
\description{
This function:
\itemize{
\item{1) Calls proc_tasks function to organize and clean task data into BIDS-compliant .tsv}
\item{2) Calls proc_tasks_deriv function to generate summary datasets}
\item{2) Exports all or select BIDS-compliant .tsv and .json files into bids/derivatives and/or bids/phenotype}
}
}
\details{
To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
}
\examples{

\dontrun{

# process task data
task_data <- proc_task(base_wd = base_dir, return_data = TRUE)

# export derivative databases
write_tasks(task_data, "path/to/export/")
}

}
