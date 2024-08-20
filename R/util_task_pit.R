#' util_task_pit: Clean and organize PIT data into BIDS rawdata
#'
#' This function formats and organizes RRV data from bids/sourcedata into bids/rawdata for a given subject
#'
#' @param sub subject label used in sub-label. Leading zeros not required
#' @param ses session label used in ses-label.
#' @param bids_wd string with full path to bids directory -- this is the directory that contains sourcedata/ and rawdata/
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = TRUE
#'
#' @return If return_data is set to TRUE, will return a dataframe with cleaned data
#'
#' @examples
#'
#' \dontrun{
#' # process task data for the Food View Task
#' pit_data <- util_task_pit(sub = 001, ses = 1, bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids", return = TRUE)
#'
#' }
#' @importFrom utils read.table
#' @importFrom rlang .data
#' @export

util_task_pit <- function(sub, ses, bids_wd, overwrite = FALSE, return_data = TRUE) {

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
    print('util_task_pit.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # Get subject number with leading zeros
  sub <- sprintf("%03d", as.numeric(sub))

  # Set sub and ses strings
  sub_str <- sprintf("sub-%03d", as.numeric(sub))
  ses_str <- paste0("ses-", ses)

  #### Import Data #####

  # get directory paths
  source_beh_wd <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash)

  # make list of csv pit files
  csv_file <- Sys.glob(paste0(source_beh_wd, "*Food-PIT*csv"))

  # load data, abort processing no file or >1 file matches pattern
  if (length(csv_file) == 1) {
    pit_data <- read.csv(csv_file, header = TRUE)
  } else if ( length(csv_file) == 0) {
    print(paste(sub_str, "has no PIT CSV. Aborting task processing for this sub."))
    return()
  } else if (length(csv_file) > 1) {
    print(paste(sub_str, "has more than 1 PIT CSV. Should only have 1. Aborting task processing for this sub."))
    return()
  }

  #### Clean data #####

  # do something to pit_data

  #### Export Data  #####

  # make raw beh directory if it doesn't exist
  raw_beh_wd <- paste0(bids_wd, slash, 'rawdata', slash, sub_str, slash, ses_str, slash, 'beh', slash)
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # export files if don't exist or overwrite = TRUE
  beh_outfile <- paste0(raw_beh_wd, sub_str, '_ses-', ses, '_task-pit_beh.tsv')
  if (!file.exists(beh_outfile) | isTRUE(overwrite)) {
    utils::write.table(pit_data, beh_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a")
  }


  #### Return data #####

  if (isTRUE(return_data)) {
    return(pit_data)
  }
}

