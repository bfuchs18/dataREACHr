#' util_task_untouched_to_source: Move task data from untouchedRaw into bids/sourcedata
#'
#' This function copies task data from untouchedRaw into bids/sourcedata for the following tasks: sst, foodview task, space game, pit task
#'
#'
#' @param base_wd string with full path to base directory -- this is the directory that contains untouchedraw/ and bids/sourcedata/
#' @param overwrite logical indicating if data should be overwritten in /sourcedata Default = FALSE
#'
#' @examples
#'
#' # organize task data in untouchedRaw into sourcedata
#' util_task_untouched_to_source(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/")
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_untouched_to_source <- function(base_wd, overwrite = FALSE) {

  # base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/"

  #### Set up/initial checks #####

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

  #### Define copy_to_source() ####
  copy_to_source <- function(file, sub_str, ses_str, sourcefile_prefix, overwrite = overwrite) {

    # A function to copy a file into sourcedata
    #
    # @param file path to file to be copied into sourcedata. Include full file path and filename (e.g., "path/to/filename.txt") (string)
    # @param sub_str bids-formatted subject string. e.g., "sub-001" (string)
    # @param ses_str bids-formatted session string. e.g., "ses-1" (string)
    # @param sourcefile_prefix (optional) string to prefix filename with in sourcedata (string)
    # @param overwrite logical indicating whether file should be overwritten in sourcedata (logical)

    # check for sourcefile_prefix arg
    prefix_arg <- methods::hasArg(sourcefile_prefix)

    # set sourcedata directory for task files
    sub_task_source_dir <- paste0(base_wd, slash, 'bids', slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, "beh")

    # get file name
    filename <- basename(file)

    # set sourcedata file
    if (isTRUE(prefix_arg)) {
      sub_task_source_file <- paste0(sub_task_source_dir, slash, sourcefile_prefix, filename)

    } else {
      sub_task_source_file <- paste0(sub_task_source_dir, slash, filename)
    }

    # create sub_task_source_dir if it doesnt exist
    if (!dir.exists(sub_task_source_dir)) {
      dir.create(sub_task_source_dir, recursive = TRUE)
    }

    # copy file into sub_task_source_dir
    file.copy(from = file, to = sub_task_source_file, overwrite = overwrite)

  }

  #### FoodView task ####

  foodview_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'foodview_task')
  foodview_files <- list.files(foodview_dir, pattern = 'foodview', full.names = TRUE)

  for (file in foodview_files) {

    # extract subject from file
    temp <- sub('.*-', '', file) # extract characters after final "-" (replace everything up to and including the last occurrence of a hyphen in the string with "")
    sub_num <- sub('.txt', '', temp) # extract sub number (replace .txt with "")
    sub_str <- sprintf("sub-%03d", as.numeric(sub_num))

    # copy to sourcedata
    copy_to_source(file, sub_str, ses_str = 'ses-1', overwrite)
  }

  #### Stop Signal Task ####

  sst_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'sst')
  sst_files <- list.files(sst_dir, pattern = 'stop', full.names = TRUE)

  for (file in sst_files) {

    # extract subject from file
    temp <- sub('.*-', '', file) # extract characters after final "-" (replace everything up to and including the last occurrence of a hyphen in the string with "")
    sub_num <- sub('.txt', '', temp) # extract sub number (replace .txt with "")
    sub_str <- sprintf("sub-%03d", as.numeric(sub_num))

    # copy to sourcedata
    copy_to_source(file, sub_str, ses_str = 'ses-1', overwrite)
  }

  #### Space Game ####

  space_game_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'space_game')
  space_game_files <- list.files(space_game_dir, pattern = 'mbmfNovelStakes', full.names = TRUE)

  for (file in space_game_files) {

    # extract file name
    filename <- basename(file)

    # extract subject from filename
    temp <- sub("(^[^-]+)-.*", "\\1", filename) # extract characters before first "-"
    sub_num <- sub('mbmfNovelStakes_', '', temp) # extract sub number (replace 'mbmfNovelStakes_' with "")
    sub_str <- sprintf("sub-%03d", as.numeric(sub_num))

    # copy to sourcedata
    copy_to_source(file, sub_str, ses_str = 'ses-1', overwrite)
  }

  #### NIH toolbox ####

  toolbox_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'nih-toolbox')
  toolbox_sub_dirs <- list.dirs(toolbox_dir, full.names = TRUE, recursive = FALSE)

  for (sub_dir in toolbox_sub_dirs) {
    # extract directory name
    dirname <- basename(sub_dir)

    # if directory starts with "REACH"
    if (grepl("^REACH", dirname)) {
      # extract subject ID
      sub_str <- sub("REACH_", "", dirname)

      # determine session? -- right now only ses-1 data is uploaded
    }
  }


  #### RRV ####
  rrv_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'rrv_task')
  rrv_sub_dirs <- list.dirs(rrv_dir, full.names = TRUE, recursive = FALSE)

  for (sub_dir in rrv_sub_dirs) {

    # extract directory name
    dirname <- basename(sub_dir)

    # if directory starts with "REACH"
    if (grepl("^REACH", dirname)) {

      # extract subject ID
      sub <- gsub("REACH_|_csv", "", dirname)
      sub_str <- sprintf("sub-%03d", as.numeric(sub))

      # get list of files in sub_dir (but not directories)
      rrv_sub_files <- setdiff(list.files(sub_dir, full.names = TRUE), list.dirs(sub_dir, recursive = FALSE, full.names = TRUE))

      # copy files to sourcedata
      for (file in rrv_sub_files) {
        copy_to_source(file, sub_str, ses_str = "ses-1", sourcefile_prefix = "rrv_", overwrite)
      }
    }
  }

  #### PIT ####

  # for each session
  for (ses_str in c("ses-1", 'ses-2')) {

    # define directory with pit data
    if (ses_str == "ses-1") {
      pit_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'pit_task')
    } else {
      pit_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'pit_task', slash, 'V5 PIT')
    }

    # get list of files in pit_dir (but not directories)
    pit_ses_files <- setdiff(list.files(pit_dir, full.names = TRUE), list.dirs(pit_dir, recursive = FALSE, full.names = TRUE))

    # get list of subs: get file basename and extract characters before first "_"
    ses_subs <- unique(sub("(^[^_]+)_.*", "\\1", basename(pit_ses_files)))

    # for each sub
    for (sub in ses_subs) {

      # extract subject files
      sub_files <- list.files(pit_dir, pattern = (paste0(sub, "_Food-PIT")), full.names = TRUE)

      # extract file extensions
      extensions <- tools::file_ext(sub_files)

      # output warning if .csv not found
      if ( !"csv" %in% extensions ) {
        print(paste("warning: subject", sub, ses_str, "has PIT output files but no csv" ))
      }

      # set subject string
      sub_str <- sprintf("sub-%03d", as.numeric(sub))

      # copy files to sourcedata
      for (file in sub_files) {
        copy_to_source(file, sub_str, ses_str, overwrite)
      }
    }
  }
}
