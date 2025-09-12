#' util_task_foodview: Clean and organize Food View task data into BIDS rawdata
#'
#' This function formats and organizes Food View task data from bids/sourcedata into bids/rawdata for a given subject
#'
#'
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#'
#' @return statement of task completed
#'
#' @examples
#'
#' \dontrun{
#' # process task data for the Food View Task
#' util_task_foodview_orgraw(sub_id = 'sub-001', ses = 'ses-1', base_wd = base_wd)
#'
#' }
#'
#'
#' @export

util_task_foodview <- function(sub_str, ses_str, base_wd, overwrite = FALSE) {

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  #### Define sub/ses vars and paths ####

  # get directory paths
  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'func')
  onset_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-foodview_onsets.tsv'))
  resp_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-foodview.tsv'))

  #### Organize Data #####

  # load data, abort processing if file does not exist

  if (file.exists(onset_source_file)) {
    onset_dat <- read.table(onset_source_file, sep = '\t', header = TRUE)
  } else {
    print(paste(sub_str, 'has no Food View task onset data'))
    return()
  }

  if (file.exists(resp_source_file)) {
    resp_dat <- read.table(resp_source_file, header = TRUE)
  } else {
    print(paste(sub_str, 'has no Food View task response data.'))
    return()
  }

  # function to clean dat by run ####
  run_proc <- function(run, dat){
    run_label <- paste0('run', run)
    run_dat <- dat[(dat['run'] == run),]

    # check for > 1 'wait' stimulus - this indicates the run was restarted but onsets from the original attempt were not overwritten
    if (sum(run_dat[['stim_file_name']] == 'wait') > 1) {

      # identify row with the final 'wait' stimulus -- indicates the start of the actual (un-aborted) run
      run_start <- max(grep('wait', run_dat[['stim_file_name']]))

      # remove rows prior to run_start (i.e. onsets from aborted runs)
      run_dat <- run_dat[-(1:run_start-1), ]
    }

    # transform sys_onset_time to start at 0 and be in seconds
    run_dat['onset'] <- (run_dat[['sys_onset_time']] - min(run_dat[['sys_onset_time']]))/1000

    # add duration column -- calculated based on onsets except for final fixation
    run_dur_function <- function(run_dat, row){
      if (row == nrow(run_dat) & run_dat[row, 'stim_file_name'] == 'fix') {
        duration <- 10 #set to 10 seconds based on task program
      } else {
        duration <- round(run_dat[row+1,'onset'] - run_dat[row,'onset'], 2)
      }
      return(duration)
    }

    run_dat['duration'] <- sapply(seq(1:nrow(run_dat)), function(x) run_dur_function(run_dat, x))


    # clean response_time column

    ## make rt = NA when dat$resp = 0 (indicating no response)
    run_dat[!is.na(run_dat['response']) & run_dat['response'] == 0, 'response_time'] <- NA

    # re-order columns
    run_dat <- run_dat[c('onset', 'duration', 'sub', 'run', 'commercial_cond', 'stim_file_name', 'response', 'response_time' , 'food_ed', 'food_taste', 'sys_onset_time')]

    # rename run column
    names(run_dat)[names(run_dat) == 'run'] <- 'run_num'

    return(run_dat)
  }

  # update columns names
  names(resp_dat)[names(resp_dat) == 'stimName'] <- 'stim'
  names(onset_dat)[names(onset_dat) == 'commercial_condfood_cond'] <- 'commercial_cond'

  # combine onset and response data
  dat <- merge(onset_dat, resp_dat, by=c('run', 'set','food_cond','commercial_cond','stim'), all = TRUE)

  # order by onset_time
  dat <- dat[order(dat[['onset_time']]), ]

  # re-label commercial_conditions as food/toy
  dat[grepl('TRUE', dat[['commercial_cond']]), 'commercial_cond'] <- 'toy'
  dat[grepl('FALSE', dat[['commercial_cond']]), 'commercial_cond'] <- 'food'

  dat[grepl('T', dat[['commercial_cond']]), 'commercial_cond'] <- 'toy'
  dat[grepl('F', dat[['commercial_cond']]), 'commercial_cond'] <- 'food'

  # add subject column
  dat['sub'] <- sub_str

  # separate food_cond into 2 columns
  ## ED column
  dat['food_ed'] <- ifelse(is.na(dat[['food_cond']]), NA, ifelse(grepl('led', dat[['food_cond']]), 'low', 'high'))

  ## taste column
  dat['food_taste'] <- ifelse(is.na(dat[['food_cond']]), NA, ifelse(grepl('savory', dat[['food_cond']]), 'savory', 'sweet'))


  # remove food_cond column
  dat <- dat[,!(names(dat) %in% c('food_cond'))]

  # update names
  names(dat)[names(dat) == 'stim'] <- 'stim_file_name'
  names(dat)[names(dat) == 'rt'] <- 'response_time'
  names(dat)[names(dat) == 'resp'] <- 'response'
  names(dat)[names(dat) == 'onset_time'] <- 'sys_onset_time'

  # split data by run, process onset and duration, save into run_dfs
  run_dfs <- sapply(unique(dat$run), function(x) run_proc(x, dat), simplify = FALSE)
  names(run_dfs) <- sapply(unique(dat$run), function(x) paste0('run-0', x))

  #### Save in rawdata #####

  # create bids/rawdata directory if it doesn't exist
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }

  # save function
  save_run <- function(run_label, data_list, outfile){
    run_dat <- data_list[[run_label]]
    utils::write.table(run_dat, outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )
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

