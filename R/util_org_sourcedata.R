#' util_org_sourcedata: Move data from untouchedRaw into bids/sourcedata
#'
#' This function moves data from untouchedRaw into bids/sourcedata
#'
#'
#' @param base_wd string with full path to base directory -- this is the directory that contains untouchedraw/ and bids/sourcedata/
#' @param overwrite logical indicating if data should be overwritten in /sourcedata Default = FALSE
#'
#' @examples
#'
#' # process task data for the Food View Task
#' test <- util_org_sourcedata(data_path)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_org_sourcedata <- function(base_wd, overwrite = FALSE) {

  # base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/"

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("base_wd must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('util_task_foodview_orgraw.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }


  #### FoodView task ####
  foodview_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'foodview_task')

  foodview_files <- list.files(foodview_dir, pattern = 'foodview', full.names = TRUE)

  for (file in foodview_files) {

    # extract file name
    filename <- basename(file)

    # extract subject from file name
    temp <- sub('.*-', '', file) # extract characters after final "-" (replace everything up to and including the last occurrence of a hyphen in the string with "")
    sub_num <- sub('.txt', '', temp) # extract sub number (replace .txt with "")
    sub_str <- sprintf("sub-%03d", as.numeric(sub_num))

    # set sourcedata directory for task files
    sub_task_source_dir <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, "ses-1", slash, "beh")

    # set sourcedata file
    sub_task_source_file <- paste0(sub_task_source_dir, slash, filename)

    # create sub_task_source_dir if it doesnt exist
    if (!dir.exists(sub_task_source_dir)) {
      dir.create(sub_task_source_dir, recursive = TRUE)
    }

    # copy file into sub_task_source_dir if it doesn't exist or overwrite = TRUE
    if (!file.exists(sub_task_source_file) | isTRUE(overwrite)) {

        file.copy(from = file, to = sub_task_source_file)
    }
  }

  #### Stop Signal Task ####
  # to do: reduce reptitive code between sst and food view
  sst_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'sst')

  sst_files <- list.files(sst_dir, pattern = 'stop', full.names = TRUE)

  for (file in sst_files) {

    # extract file name
    filename <- basename(file)

    # extract subject from file name
    temp <- sub('.*-', '', file) # extract characters after final "-" (replace everything up to and including the last occurrence of a hyphen in the string with "")
    sub_num <- sub('.txt', '', temp) # extract sub number (replace .txt with "")
    sub_str <- sprintf("sub-%03d", as.numeric(sub_num))

    # set sourcedata directory for task files
    sub_task_source_dir <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, "ses-1", slash, "beh")

    # set sourcedata file
    sub_task_source_file <- paste0(sub_task_source_dir, slash, filename)

    # create sub_task_source_dir if it doesnt exist
    if (!dir.exists(sub_task_source_dir)) {
      dir.create(sub_task_source_dir, recursive = TRUE)
    }

    # copy file into sub_task_source_dir if it doesn't exist or overwrite = TRUE
    if (!file.exists(sub_task_source_file) | isTRUE(overwrite)) {

      file.copy(from = file, to = sub_task_source_file)
    }
  }
}

