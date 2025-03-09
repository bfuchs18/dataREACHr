#' util_task_foodview: Clean and organize Food View task data into BIDS rawdata
#'
#' This function formats and organizes Food View task data from bids/sourcedata into bids/rawdata for a given subject
#'
#'
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @param bids_wd string with full path to bids directory that contains the sourcedata/ and rawdata/ directories
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a list with 1 cleaned dataframe per run
#'
#' @examples
#'
#' \dontrun{
#' # process task data for the Food View Task
#' util_task_foodview_orgraw(sub_id = 'sub-001', ses = 1, bids_wd = bids_wd)
#'
#' }
#'
#'
#' @export

util_task_foodview <- function(sub_str, ses_str = 'ses-1', bids_wd, overwrite = FALSE) {

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

  #### Define sub/ses vars and paths ####

  # get directory paths
  raw_wd <- file.path(bids_wd, 'rawdata', sub_str, ses_str, 'func')
  onset_source_file <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-foodview_onsets.tsv'))
  resp_source_file <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-foodview.tsv'))

  #### Organize Data #####

  # load data, abort processing if file does not exist

  if (file.exists(onset_source_file)) {
    onset_dat <- read.table(onset_source_file, sep = '\t', header = TRUE, colClasses = c("commercial_condfood_cond"="character"))
  } else {
    print(paste(sub_str, "has no Food View task onset data. Aborting task processing for this sub."))
    return()
  }

  if (file.exists(resp_source_file)) {
    resp_dat <- read.table(resp_source_file, header = TRUE, colClasses = c("commercial_cond"="character"))
  } else {
    print(paste(sub_str, "has no Food View task response data. Aborting task processing for this sub."))
    return()
  }

  # update columns names
  names(resp_dat)[names(resp_dat) == "stimName"] <- "stim"
  names(onset_dat)[names(onset_dat) == "commercial_condfood_cond"] <- "commercial_cond"

  # combine onset and response data
  dat <- merge(onset_dat, resp_dat, by=c("run", "set","food_cond","commercial_cond","stim"), all = TRUE)
  dat <- dat[order(dat$onset_time),] #order by onset_time

  # re-label commercial_conditions as food/toy
  dat$commercial_cond <- sub("T", "toy", dat$commercial_cond)
  dat$commercial_cond <- sub("F", "food", dat$commercial_cond)

  # add subject column
  dat$sub <- sub_str

  # separate food_cond into 2 columns
  ## ED column
  dat <- dat %>%
    dplyr::mutate(food_ed = dplyr::case_when(
      grepl("led", food_cond) ~ "low",
      grepl("hed", food_cond) ~ "high",
      TRUE ~ "n/a"  # Default value if none of the conditions are met
    ))

  ## taste column
  dat <- dat %>%
    dplyr::mutate(food_taste = dplyr::case_when(
      grepl("savory", food_cond) ~ "savory",
      grepl("sweet", food_cond) ~ "sweet",
      TRUE ~ "n/a"  # Default value if none of the conditions are met
    ))

  # remove food_cond column
  dat <- dat[,!(names(dat) %in% c("food_cond"))]

  # update names
  names(dat)[names(dat) == "stim"] <- "stim_file_name"
  names(dat)[names(dat) == "rt"] <- "response_time"
  names(dat)[names(dat) == "resp"] <- "response"
  names(dat)[names(dat) == "onset_time"] <- "sys_onset_time"


  # function to clean dat by run
  run_proc <- function(run, dat){
    run_label <- paste0("run", run)
    run_dat <- dat[(dat$run == run),]

    # check for > 1 "wait" stimulus - this indicates the run was restarted but onsets from the original attempt were not overwritten
    if (sum(run_dat$stim_file_name == "wait") > 1) {

      # identify row with the final "wait" stimulus -- indicates the start of the actual (un-aborted) run
      run_start <- max(grep("wait", run_dat$stim_file_name))

      # remove rows prior to run_start (i.e. onsets from aborted runs)
      run_dat <- run_dat[-(1:run_start-1), ]
    }

    # transform sys_onset_time to start at 0 and be in seconds
    run_dat$onset <- (run_dat$sys_onset_time - min(run_dat$sys_onset_time))/1000

    # add duration column -- calculated based on onsets except for final fixation
    for (row in 1:nrow(run_dat)) {
      if (row < nrow(run_dat)) {
        run_dat[row, "duration"] <- round(run_dat[row+1,"onset"] - run_dat[row,"onset"], 2)
      } else if (row == nrow(run_dat) & run_dat[row, "stim_file_name"] == "fix") {
        run_dat[row, "duration"] <- 10 #set to 10 seconds based on task program
      }
    }

    # clean response_time column

    ## make rt = NA when dat$resp = 0 (indicating no response)
    run_dat$response_time[run_dat$response == 0] <- NA

    # re-order columns
    run_dat <- run_dat[c('onset', 'duration', 'sub', 'run', 'commercial_cond', 'stim_file_name', 'response', 'response_time' , 'food_ed', 'food_taste', 'sys_onset_time')]

    # rename run column
    names(run_dat)[names(run_dat) == "run"] <- "run_num"

    return(run_dat)
  }

  # split data by run, process onset and duration, save into run_dfs
  run_dfs <- sapply(unique(dat$run), function(x) run_proc(x, dat), simplify = FALSE)
  names(run_dfs) <- sapply(unique(dat$run), function(x) paste0("run-0", x))

  #### Save in rawdata #####

  # create bids/rawdata directory if it doesn't exist
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }

  # save function
  save_run <- function(run_label, data_list, outfile){
    run_dat <- data_list[[run_label]]
    utils::write.table(run_dat, outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
  }


  # define output file with path
  outfiles <- file.path(raw_wd, paste0(sub_str, '_', ses_str, '_task-foodview_', names(run_dfs), '_events.tsv'))

  if (!file.exists(outfiles[1]) | isTRUE(overwrite)){

    mapply(save_run, run_label = names(run_dfs), outfile = outfiles, MoreArgs = list(run_dfs))

    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }
}

