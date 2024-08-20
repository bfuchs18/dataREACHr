#' util_task_untouched_to_source: Move task data from untouchedRaw into bids/sourcedata
#'
#' This function copies task data from untouchedRaw into bids/sourcedata for the following tasks: sst, foodview task, space game, pit task, nih toolbix
#'
#'
#' @param base_wd string with full path to base directory -- this is the directory that contains untouchedraw/ and bids/sourcedata/
#' @param overwrite logical indicating if data should be overwritten in /sourcedata Default = FALSE
#' @param all_tasks logical indicating if all tasks should be moved to sourcedata. Default = FALSE. If all_tasks = FALSE, user must specify tasks to process in task_vector
#' @param task_vector vector with tasks to process. Must be included in all_tasks = FALSE. Options include: c("sst", "foodview", "spacegame", "nih_toolbox", "rrv", "pit")
#'
#' @examples
#'
#' # organize task data for all tasks in untouchedRaw into sourcedata
#' util_task_untouched_to_source(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", all_tasks = TRUE)
#'
#' # organize task data for space game and NIH toolbox in untouchedRaw into sourcedata
#' util_task_untouched_to_source(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", task_vector = c("spacegame", "nih_toolbox")
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_task_untouched_to_source <- function(base_wd, overwrite = FALSE, all_tasks = FALSE, task_vector) {

  # base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/"

  #### Set up/initial checks ####

  # check that base_wd exist and is a string
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

  # check that task options correctly specified
  task_vector_arg <- methods::hasArg(task_vector)

  if (isFALSE(all_tasks) & isFALSE(task_vector_arg)) {
    stop("Tasks to process not specified: Specify tasks by setting all_tasks = TRUE or providing vector to task_vector parameter")
  }

  if (isTRUE(all_tasks) & isTRUE(task_vector_arg)) {
    stop("all_tasks = TRUE and task_vector parameter were provided. Use only 1 of these options")
  }

  if (isTRUE(task_vector_arg)) {
    if (!is.vector(task_vector)) {
      stop("Input to task_vector must be vector (e.g., task_vector = c('rrv')")
    } else {
      for (task in task_vector) {
        if (!task %in% c("sst","foodview","spacegame","nih_toolbox","rrv","pit")) {
          stop(paste(task, "is not an option for task_vector"))
        }
      }
    }
  } else {
    task_vector = c()
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('util_task_untouched_to_source.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }


  #### Define copy_to_source() ####
  copy_to_source <- function(file, sub_str, ses_str, sourcefile_prefix, overwrite) {

    # copy_to_source: A function to copy a file into sourcedata
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

  if (isTRUE(all_tasks) | "foodview" %in% task_vector) {
    print("-- copying Food View")

    foodview_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'foodview_task')
    foodview_files <- list.files(foodview_dir, pattern = 'foodview', full.names = TRUE)

    for (file in foodview_files) {

      # extract subject from file
      temp <- sub('.*-', '', file) # extract characters after final "-" (replace everything up to and including the last occurrence of a hyphen in the string with "")
      sub_num <- sub('.txt', '', temp) # extract sub number (replace .txt with "")
      sub_str <- sprintf("sub-%03d", as.numeric(sub_num))

      # copy to sourcedata
      copy_to_source(file, sub_str, ses_str = 'ses-1', overwrite = overwrite)
    }
  }


  #### Stop Signal Task ####
  if (isTRUE(all_tasks) | "sst" %in% task_vector) {
    print("-- copying SST")

    sst_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'sst')
    sst_files <- list.files(sst_dir, pattern = 'stop', full.names = TRUE)

    for (file in sst_files) {

      # extract subject from file
      temp <- sub('.*-', '', file) # extract characters after final "-" (replace everything up to and including the last occurrence of a hyphen in the string with "")
      sub_num <- gsub('.txt|_1st.txt', '', temp) # extract sub number (replace '.txt' or '_1st.txt' with "")
      sub_str <- sprintf("sub-%03d", as.numeric(sub_num))

      # copy to sourcedata
      copy_to_source(file, sub_str, ses_str = 'ses-1', overwrite = overwrite)
    }
  }


  #### Space Game ####
  if (isTRUE(all_tasks) | "spacegame" %in% task_vector) {
    print("-- copying Space Game")

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
      copy_to_source(file, sub_str, ses_str = 'ses-1', overwrite = overwrite)
    }
  }


  #### NIH toolbox ####
  if (isTRUE(all_tasks) | "nih_toolbox" %in% task_vector) {

    print("-- copying NIH toolbox")

    # for each session
    for (ses_str in c("ses-1", 'ses-2')) {

      # define directory with data
      if (ses_str == "ses-1") {
        toolbox_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'nih-toolbox', slash, 'V1')
      } else {
        toolbox_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'nih-toolbox', slash, 'V5')
      }

      # get list of subject directories
      toolbox_sub_dirs <- list.dirs(toolbox_dir, full.names = TRUE, recursive = FALSE)

      for (sub_dir in toolbox_sub_dirs) {
        # extract directory name
        dirname <- basename(sub_dir)

        # if directory starts with "REACH"
        if (grepl("^REACH", dirname)) {

          # extract subject ID
          sub <- sub("REACH_", "", dirname)
          sub_str <- sprintf("sub-%03d", as.numeric(sub))

          # get list of files
          toolbox_sub_files <- setdiff(list.files(sub_dir, full.names = TRUE), list.dirs(sub_dir, recursive = FALSE, full.names = TRUE))

          # copy files to sourcedata
          for (file in toolbox_sub_files) {
            copy_to_source(file, sub_str, ses_str, sourcefile_prefix = "toolbox_", overwrite = overwrite)
          }
        }
      }
    }
  }


  #### RRV ####
  if (isTRUE(all_tasks) | "rrv" %in% task_vector) {

    print("-- copying RRV")

    rrv_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'rrv_task')
    rrv_sub_dirs <- list.dirs(rrv_dir, full.names = TRUE, recursive = FALSE)

    for (sub_dir in rrv_sub_dirs) {

      # extract directory name
      dirname <- basename(sub_dir)

      # if directory starts with "REACH"
      if (grepl("^REACH", dirname)) {

        # extract subject ID
        sub <- gsub("REACH_", "", dirname)
        sub_str <- sprintf("sub-%03d", as.numeric(sub))

        # get list of files in sub_dir (but not directories)
        rrv_sub_files <- setdiff(list.files(sub_dir, full.names = TRUE), list.dirs(sub_dir, recursive = FALSE, full.names = TRUE))

        # define expected name of text file to copy
        rrv_txt_file <- paste0(rrv_dir, "/" ,dirname, "/rrv_", sub, ".txt")

        # print message if text file not found
        if (!rrv_txt_file %in% rrv_sub_files) {
          print(paste("RRV text file not found for", sub_str))
        }

        # copy all files to sourcedata
        for (file in rrv_sub_files) {
          copy_to_source(file, sub_str, ses_str = "ses-1", overwrite = overwrite)
        }

      }
    }
  }


  #### PIT ####

  if (isTRUE(all_tasks) | "pit" %in% task_vector) {
    print("-- copying PIT task")

    # for each session
    for (ses_str in c("ses-1", 'ses-2')) {

      # define directory with pit data
      if (ses_str == "ses-1") {
        pit_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'pit_task', slash, 'V4_PIT')
      } else {
        pit_dir <- paste0(base_wd, slash, 'untouchedRaw', slash, 'pit_task', slash, 'V5_PIT')
      }

      # get list of files in pit_dir
      pit_ses_files <- list.files(pit_dir, "Food-PIT")

      # get list of subs: get first 3 characters in file name
      ses_subs <- unique(substr(pit_ses_files, 1, 3))

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
          copy_to_source(file, sub_str, ses_str, overwrite = overwrite)
        }
      }
    }
  }
}
