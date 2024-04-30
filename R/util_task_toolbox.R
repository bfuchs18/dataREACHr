#' util_task_toolbox: Clean and organize NIH toolbox assessment data into rawdata
#'
#' This function formats and organizes NIH toolbox assessment data from bids/sourcedata into rawdata for a given subject. Assessment data includes responses to each trial in the NIH toolbox.
#'
#'
#' @param sub subject label used in sub-label. Leading zeros not required (integer)
#' @param ses session label used in ses-label (integer)
#' @param bids_wd string with full path to bids directory -- this is the directory that contains sourcedata/ and rawdata/
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a list with 1 cleaned dataframe per run
#'
#' @examples
#'
#' \dontrun{
#' # process assessment (response) data for the NIH toolbox
#' sub001_toolbox_responses <- util_task_toolbox(sub = 001, ses = 1, bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids", return = TRUE)
#'
#' }
#'
#' @importFrom utils read.csv
#' @export

util_task_toolbox <- function(sub, ses, bids_wd, overwrite = FALSE, return_data = TRUE) {

  # bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids"

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(bids_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(bids_wd)) {
      stop("bids_wd must be entered as a string")
    } else if (!file.exists(bids_wd)) {
      stop("bids_wd entered, but file does not exist. Check bids_wd string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("bids_wd must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('util_task_foodview.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # Get subject number without leading zeros
  sub_num <- as.numeric(sub)

  # Set sub and ses strings
  sub_str <- sprintf("sub-%03d", sub_num)
  ses_str <- paste0("ses-", ses)

  # get directory paths
  source_beh_wd <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash)
  raw_beh_wd <- paste0(bids_wd, slash, 'rawdata', slash, sub_str, slash, ses_str, slash, 'beh', slash)
  assessment_source_file <- list.files(source_beh_wd, pattern = "Assessment Data", full.names = TRUE)
  registration_source_file <- list.files(source_beh_wd, pattern = "Registration Data", full.names = TRUE)

  #### Generate file for rawdata #####

  # load data, abort processing if file does not exist

  if (file.exists(assessment_source_file)) {
    assessment_dat <- read.csv(assessment_source_file, header = TRUE)
  } else {
    print(paste(sub_str, "has no NIH toolbox assessment data. Aborting task processing for this sub."))
    return()
  }

  if (file.exists(registration_source_file)) {
    registrant_dat <- read.csv(registration_source_file, header = TRUE)
  } else {
    print(paste(sub_str, "has no NIH toolbox registration data. Aborting task processing for this sub."))
    return()
  }

  # Add registration data to assessment data
  assessment_dat$registration_age <- registrant_dat$Age
  assessment_dat$registration_education <- registrant_dat$Education
  assessment_dat$registration_mothers_education <- registrant_dat$MothersEducation
  assessment_dat$registration_gender <- registrant_dat$Gender
  assessment_dat$registration_handedness <- registrant_dat$Handedness
  assessment_dat$registration_race <- registrant_dat$Race
  assessment_dat$registration_ethnicity <- registrant_dat$Ethnicity

  # make separate columns for task (e.g., "Flanker Inhibitory Control") and test ages (e.g., "Ages 8-11 v2.1") from Inst (e.g., "NIH Toolbox Flanker Inhibitory Control and Attention Test Ages 8-11 v2.1") ??

  # # update columns names
  # names(resp_dat)[names(resp_dat) == "stimName"] <- "stim"
  # names(onset_dat)[names(onset_dat) == "commercial_condfood_cond"] <- "commercial_cond"

  # add subject column
  assessment_dat$sub <- sub_str
  assessment_dat <- assessment_dat %>% dplyr::relocate("sub") # move sub to first column

  #### Save in rawdata #####

  # create bids/rawdata directory if it doesn't exist
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # define output file with path
  outfile <- paste0(raw_beh_wd, sub_str, '_ses-', ses, '_task-toolbox_beh.tsv')

  # export file if doesn't exist or overwrite = TRUE
  if (!file.exists(outfile) | isTRUE(overwrite)) {
    utils::write.table(assessment_dat, outfile, sep = '\t', quote = FALSE, row.names = FALSE )
  }


  #### Return data #####
  if (isTRUE(return_data)){
    return(assessment_data = assessment_dat
    )
  }
}

