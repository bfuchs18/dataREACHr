#' util_task_sst: Clean and organize Stop Signal Task data into BIDS rawdata
#'
#' This function formats and organizes Stop Signal Task data from bids/sourcedata into bids/rawdata for a given subject
#' Data from practice and behavioral runs will be organized into bids/rawdata/sub-label/beh, while data from fmri runs will be organized into bids/rawdata/sub-label/func.
#'
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_task_foodview
#' @inheritParams util_copy_to_source
#'
#' @return statement of task completed
#'
#' @examples
#'
#' \dontrun{
#' # process task data for the Food View Task
#' util_task_sst(sub_str = 'sub-001', ses_str = 'ses-1', bids_wd = bids_wd)
#'
#' }
#' @export

util_task_sst <- function(sub_str, ses_str = 'ses-1', bids_wd, overwrite = FALSE) {

  #### Set up/initial checks #####

  # check that bids_wd exist and is a string
  bids_wd_arg <- methods::hasArg(bids_wd)

  if (isTRUE(bids_wd_arg)) {
    if (!is.character(bids_wd)) {
      stop("bids_wd must be entered as a string")
    } else if (!file.exists(bids_wd)) {
      stop("bids_wd entered, but directory does not exist. Check bids_wd string.")
    }
  } else if (isFALSE(bids_wd_arg)) {
    stop("bids_wd must be entered as a string")
  }

  # get directory paths
  raw_func_wd <- file.path(bids_wd, 'rawdata', sub_str, ses_str, 'func')
  raw_beh_wd <- file.path(bids_wd, 'rawdata', sub_str, ses_str, 'beh')
  onset_source_file <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-sst_onsets.tsv'))
  fmri_source_file <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-sst_fmri.tsv'))

  beh_source_file <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'beh', paste0(sub_str, '_', ses_str, '_task-sst_beh.tsv'))
  prac_source_file <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'beh', paste0(sub_str, '_', ses_str, '_task-sst_prac.tsv'))

  #### Load Data #####

  # load data
  ## all subs should have prac_source_file and beh_source_file
  ## subjects that did not do sst in scanner will not have onset_source_file or fmri_source_file

  if (file.exists(prac_source_file)) {
    prac_dat <- read.table(prac_source_file, header = TRUE)
  } else {
    print(paste(sub_str, "has no sst practice data. Aborting task processing for this sub."))
    return()
  }

  if (file.exists(beh_source_file)) {
    beh_dat <- read.table(beh_source_file, header = TRUE)
  } else {
    print(paste(sub_str, "has no sst beh data. Aborting task processing for this sub."))
    return()
  }

  if (file.exists(onset_source_file)) {
    onset_dat <- read.table(onset_source_file, header = TRUE)
    have_onset_dat = 1
  } else {
    have_onset_dat = 0
  }

  if (file.exists(fmri_source_file)) {
    fmri_dat <- read.table(fmri_source_file, header = TRUE)
    have_fmri_dat = 1
  } else {
    have_fmri_dat = 0
  }

  #### Clean and Export Beh Data (prac_dat and beh_dat) #####

  # beh data

  beh_dfs <- list(prac_dat, beh_dat)

  # function to process behavioral data
  beh_dat_proc <- function(dataframe){
    # update column names
    names(dataframe)[names(dataframe) == "stimName"] <- "stim_file_name"
    names(dataframe)[names(dataframe) == "resp1"] <- "response"
    names(dataframe)[names(dataframe) == "rt1"] <- "response_time"
    names(dataframe)[names(dataframe) == "stim"] <- "go_stim"

    # add subject column
    dataframe$sub <- sub_str

    # convert response_time from ms to sec
    dataframe$response_time <- dataframe$response_time / 1000

    # make response_time NA when dataframe$response = 0 (indicating no response)
    dataframe$response_time[dataframe$response == 0] <- NA

    # reorder columns
    dataframe <- dataframe[c('sub', 'type', 'run', 'set', 'run_cond', 'block',
                             'stim_file_name', 'img_cat', 'go_stim', 'signal', 'reqSSD' ,
                             'correct', 'response', 'response_time', 'trueSSD')]

    # rename run column
    names(dataframe)[names(dataframe) == "run"] <- "run_num"

    return(dataframe)
  }

  # split data by run, process onset and duration, save into run_dfs
  beh_dfs <- sapply(seq(1, length(beh_dfs)), function(x) beh_dat_proc(beh_dfs[[x]]), simplify = FALSE)
  names(run_dfs) <- sapply(unique(dat$run), function(x) paste0("run-0", x))

  # make raw beh directory if it doesn't exist
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # export files if don't exist or overwrite = TRUE

  ## practice data
  prac_outfile <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-sst_acq-practice_beh.tsv'))

  if (!file.exists(prac_outfile) | isTRUE(overwrite)) {
    utils::write.table(beh_dfs[[1]], prac_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
  }

  ## task data
  beh_outfile <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-sst_beh.tsv'))

  if (!file.exists(beh_outfile) | isTRUE(overwrite)) {
    utils::write.table(beh_dfs[[2]], beh_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
  }

  #### Clean and Export Func Data (onset_dat and fmri_dat) #####

  # save message if onset_dat or fmri_dat missing, else process
  if (have_fmri_dat == 0 & have_onset_dat == 1) {
    func_run_dfs = paste(sub_str, "has fmri onsets but not response data. sst bold_events data was not processed")
  } else if (have_fmri_dat == 1 & have_onset_dat == 0) {
    func_run_dfs = paste(sub_str, " has fmri response data but onset data. sst bold_events data was not processed")
  } else if (have_fmri_dat == 0 & have_onset_dat == 0) {
    func_run_dfs = paste(sub_str, " does not have fmri response data or onset data. sst bold_events data was not processed")

  } else if (have_fmri_dat == 1 & have_onset_dat == 1) {

    # update columns names
    names(onset_dat)[names(onset_dat) == "stim"] <- "stim_file_name"
    names(fmri_dat)[names(fmri_dat) == "stimName"] <- "stim_file_name"

    # Add column indicating trial number (trial = event when stim_file_name contains ".jpeg")
    ## trial number corresponds to the order within the txt file
    ## adding this column allows for accurate merging of onset_dat and fmri_dat below

    fmri_dat$trial_num <- seq.int(nrow(fmri_dat))
    onset_dat <- onset_dat %>%
      dplyr::mutate(trial_num = ifelse(grepl(".jpeg", .data$stim_file_name),
                                       cumsum(grepl(".jpeg", .data$stim_file_name)),
                                       NA))

    # combine onset_dat (onset data) and fmri_dat (response data) into func_dat
    func_dat <- merge(onset_dat, fmri_dat, by=c("run", "set","run_cond","stim_file_name", "trial_num"), all = TRUE)
    func_dat <- func_dat[order(func_dat$onset_time),] #order by onset_time

    # add subject column
    func_dat$sub <- sub_str

    # assign type
    func_dat$type = "fMRI"

    # update column names
    names(func_dat)[names(func_dat) == "rt1"] <- "response_time"
    names(func_dat)[names(func_dat) == "resp1"] <- "response"
    names(func_dat)[names(func_dat) == "stim"] <- "go_stim"


    # function to process data by run
    run_proc <- function(run, func_dat){

      run_label <- paste0("run", run)
      run_dat <- func_dat[(func_dat$run == run),]

      # check for > 1 "wait" stimulus - this indicates the run was restarted but onsets from the original attempt were not overwritten
      if (sum(run_dat$stim_file_name == "wait") > 1) {

        # identify row with the final "wait" stimulus -- indicates the start of the actual (un-aborted) run
        run_start <- max(grep("wait", run_dat$stim_file_name))

        # remove rows prior to run_start (i.e. onsets from aborted runs)
        run_dat <- run_dat[-(1:run_start-1), ]
      }

      # transform onset so first stimulus in run occurs at 0, convert to seconds
      run_dat$onset <- (run_dat$onset_time - min(run_dat$onset_time))/1000
      run_dat <- run_dat[,!(names(run_dat) %in% c("onset_time"))] # remove original onset_time column

      # add duration column -- calculated based on onsets except for final fixation
      for (row in 1:nrow(run_dat)) {
        if (row < nrow(run_dat)) {
          run_dat[row, "duration"] <- round(run_dat[row+1,"onset"] - run_dat[row,"onset"], 2)
        } else if (row == nrow(run_dat) & run_dat[row, "stim_file_name"] == "fix") {
          run_dat[row, "duration"] <- 10 #set to 10 seconds based on task program
        }
      }

      # convert response_time from ms to sec
      run_dat$response_time <- run_dat$response_time/1000

      # make response_time NA when func_dat$response = 0 (indicating no response)
      run_dat$response_time[run_dat$response == 0] <- NA

      # fill in block category

      # re-order columns
      run_dat <- run_dat[c('onset', 'duration', 'sub', 'run', 'set', 'run_cond',
                           'stim_file_name', 'trial_num', 'type', 'block', 'img_cat',
                           'go_stim', 'signal', 'reqSSD' , 'correct', 'response',
                           'response_time', 'trueSSD')]

      # rename run column
      names(run_dat)[names(run_dat) == "run"] <- "run_num"

      return(run_dat)
    }

    # split data by run, process onset and duration, save into run_dfs
    run_dfs <- sapply(unique(func_dat$run), function(x) run_proc(x, func_dat), simplify = FALSE)
    names(run_dfs) <- sapply(unique(func_dat$run), function(x) paste0("run-0", x))

    #### Save in rawdata #####

    # create bids/rawdata directory if it doesn't exist
    if (!dir.exists(raw_func_wd)) {
      dir.create(raw_func_wd, recursive = TRUE)
    }

    # define output file with path
    outfiles <- file.path(raw_func_wd, paste0(sub_str, '_', ses_str, '_task-sst_', names(run_dfs), '_events.tsv'))

    # save function
    save_run <- function(run_label, data_list, outfile){
      run_dat <- data_list[[run_label]]
      utils::write.table(run_dat, outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
    }

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


}

