
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataREACHr

<!-- badges: start -->
<!-- badges: end -->

dataREACHr package contains tools to process data for Project REACH at
Penn State. Project REACH is a longitudinal study of the effects of food marketing on children's eating behaviors and neural responses to food cues. Data are collected from children and parents across 5 visits. 

dataREACHr functions:

- organize data and meta-data (JSONS) for surveys (REDCap) and 
behavioral tasks into the [Brain
Imaging Data Structure](https://bids.neuroimaging.io/)

- parse text files from the RRV task into CSVs

- generate group-level dataframes containing questionnaire, demographic,
  intake, and anthropometric data

- generate subject-specific dataframes containing cleaned behavioral
  data

- export BIDS-compliant TSVs and JSONS for survey and behavioral data

## Installation

You can install the development version of dataREACHr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bfuchs18/dataREACHr")
```

## Processing data with dataREACHr

The wrapper function to process survey data is **proc_redcap()**. The wrapper function to process task data is **proc_task()**.

Using these functions requires a directory structure of:

base_dir/ (user defined)

├── untouchedRaw/

├── bids/

│ ├── phenotype/

│ ├── sourcedata/

│ ├── rawdata/

Below is a basic example of how to process survey and task data:

``` r
library(dataREACHr)

#### Set up variables ####

# define path to the base_dir directory (contains untouchedRaw/ and bids/ sub-directories)
base_dir = "/path/to/base_dir/"

# define names of redcap files to process
visit_file_name =  "visit_data.csv"
double_entry_file_name = "double_entry_data.csv"

#### process redcap data ####

# assign paths to data downloaded from redcap
visit_data_path = paste0(base_dir, "/bids/sourcedata/phenotype/", visit_file_name)
data_de_path = paste0(base_dir, "/bids/sourcedata/phenotype/", double_entry_file_name)

# process redcap data and export TSVs and JSONS into bids/; overwrite existing output
proc_redcap(visit_data_path, data_de_path, overwrite = TRUE)

#### process task data ####

# process task data and export TSVs and JSONS into bids/; overwrite existing files in rawdata for all tasks
proc_task(
  base_wd = base_dir,
  overwrite_parsed_rrv = FALSE,
  overwrite_sourcedata = FALSE,
  overwrite_rawdata_vector = c("all_tasks"),
  overwrite_jsons = FALSE
)
```

This README was generated from README.Rmd using
`devtools::build_readme()\`
