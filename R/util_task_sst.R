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
#' util_task_sst(sub_str = 'sub-001', ses_str = 'ses-1', base_wd = base_wd)
#'
#' }
#' @export

util_task_sst <- function(sub_str, ses_str = 'ses-1', base_wd, overwrite = FALSE) {

  #### Set up/initial checks #####

  # check that base_wd exist and is a string
  base_wd_arg <- methods::hasArg(base_wd)

  if (isTRUE(base_wd_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but directory does not exist. Check base_wd string.')
    }
  } else if (isFALSE(base_wd_arg)) {
    stop('base_wd must be entered as a string')
  }

  # get directory paths
  raw_func_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'func')
  raw_beh_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'beh')
  onset_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-sst_onsets.tsv'))
  fmri_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func', paste0(sub_str, '_', ses_str, '_task-sst_fmri.tsv'))

  if (sub_str == 'sub-037'){
    prac1_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh', paste0(sub_str, '_', ses_str, '_task-sst_prac1.tsv'))
    prac2_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh', paste0(sub_str, '_', ses_str, '_task-sst_prac2.tsv'))
  } else {
    prac_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh', paste0(sub_str, '_', ses_str, '_task-sst_prac.tsv'))
  }

  beh_source_file <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh', paste0(sub_str, '_', ses_str, '_task-sst_beh.tsv'))


  #### Load Data #####

  # load data
  ## all subs should have prac_source_file and beh_source_file
  ## subjects that did not do sst in scanner will not have onset_source_file or fmri_source_file
  if (sub_str == 'sub-037') {
      prac1_dat <- read.table(prac1_source_file, header = TRUE)
      prac2_dat <- read.table(prac2_source_file, header = TRUE)
  } else {
    if (file.exists(prac_source_file)) {
      prac_dat <- read.table(prac_source_file, header = TRUE)
    } else {
      print(paste(sub_str, 'has no sst practice data. Aborting task processing for this sub.'))
      return()
    }
  }


  if (file.exists(beh_source_file)) {
    beh_dat <- read.table(beh_source_file, header = TRUE)
  } else {
    print(paste(sub_str, 'has no sst beh data. Aborting task processing for this sub.'))
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

  # function to process behavioral data ####
  beh_dat_proc <- function(dataframe){
    # update column names
    names(dataframe)[names(dataframe) == 'stimName'] <- 'stim_file_name'
    names(dataframe)[names(dataframe) == 'resp1'] <- 'resp'
    names(dataframe)[names(dataframe) == 'rt1'] <- 'rt'
    names(dataframe)[names(dataframe) == 'stim'] <- 'go_stim'

    # add subject column
    dataframe['participant_id'] <- sub_str
    dataframe['session_id'] <- 'ses-1'

    # convert resp_time from ms to sec
    dataframe['rt'] <- dataframe[['rt']] / 1000

    # make resp_time NA when dataframe$resp = 0 (indicating no resp)
    dataframe[dataframe['resp'] == 0, 'rt'] <- NA

    # reorder columns
    dataframe <- dataframe[c('participant_id', 'session_id', 'type', 'run', 'set', 'run_cond', 'block',
                             'stim_file_name', 'img_cat', 'go_stim', 'signal', 'reqSSD' ,
                             'correct', 'resp', 'rt', 'trueSSD')]

    # rename run column
    names(dataframe)[names(dataframe) == 'run'] <- 'run_num'

    return(dataframe)
  }

  # split data by run, process onset and duration, save into run_dfs
  if (sub_str == 'sub-037'){
    beh_raw_dfs <- list(prac1_dat, prac2_dat, beh_dat)

  } else {
    beh_raw_dfs <- list(prac_dat, beh_dat)

  }
  beh_dfs <- sapply(seq(1, length(beh_raw_dfs)), function(x) beh_dat_proc(beh_raw_dfs[[x]]), simplify = FALSE)

  # make raw beh directory if it doesn't exist
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # export files if don't exist or overwrite = TRUE

  if (sub_str == 'sub-037'){
    ## practice data
    prac_outfile1 <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-sst_acq-practice1_events.tsv'))
    prac_outfile2 <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-sst_acq-practice2_events.tsv'))

    if (!file.exists(prac_outfile1) | isTRUE(overwrite)) {
      utils::write.table(beh_dfs[[1]], prac_outfile1, sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )
      utils::write.table(beh_dfs[[2]], prac_outfile2, sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )
    }

    beh_outfile <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-sst_acq-prescan_events.tsv'))

    if (!file.exists(beh_outfile) | isTRUE(overwrite)) {
      utils::write.table(beh_dfs[[3]], beh_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )
    }
  } else {
    ## practice data
    prac_outfile <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-sst_acq-practice_events.tsv'))

    if (!file.exists(prac_outfile) | isTRUE(overwrite)) {
      utils::write.table(beh_dfs[[1]], prac_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    }

    ## task data
    beh_outfile <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-sst_acq-prescan_events.tsv'))

    if (!file.exists(beh_outfile) | isTRUE(overwrite)) {
      utils::write.table(beh_dfs[[2]], beh_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )
    }
  }




  #### Clean and Export Func Data (onset_dat and fmri_dat) #####

  # function to calculate duration ####
  # add duration column -- calculated based on onsets except for final fixation
  calc_dur <- function (row_num, run_dat) {

    if (row_num < nrow(run_dat)) {
      dur <- round(run_dat[row_num+1,'onset'] - run_dat[row_num,'onset'], 2)
    } else if (row_num == nrow(run_dat) & run_dat[row_num, 'stim_file_name'] == 'fix') {
      dur <- 10 #set to 10 seconds based on task program
    } else {
      dur <- NA
    }
    return(dur)
  }

  # function to process data by run ####
  run_proc <- function(run, func_dat){

    run_label <- paste0('run', run)
    run_dat <- func_dat[(func_dat[['run']] == run),]

    # check for > 1 'wait' stimulus - this indicates the run was restarted but onsets from the original attempt were not overwritten
    if (sum(run_dat[['stim_file_name']] == 'wait') > 1) {

      # identify row with the final 'wait' stimulus -- indicates the start of the actual (un-aborted) run
      run_start <- max(grep('wait', run_dat[['stim_file_name']]))

      # remove rows prior to run_start (i.e. onsets from aborted runs)
      run_dat <- run_dat[-(1:run_start-1), ]
    }

    # transform onset so first stimulus in run occurs at 0, convert to seconds
    run_dat['onset'] <- (run_dat[['onset_time']] - min(run_dat[['onset_time']]))/1000
    run_dat <- run_dat[,!(names(run_dat) %in% c('onset_time'))] # remove original onset_time column

    # add duration column -- calculated based on onsets except for final fixation
    run_dat['duration'] <- sapply(seq(1, nrow(run_dat)), function(x) calc_dur(x, run_dat))

    # convert resp_time from ms to sec
    run_dat['rt'] <- run_dat[['rt']]/1000

    # make resp_time NA when func_dat$resp = 0 (indicating no resp)
    run_dat[!is.na(run_dat[['resp']]) & run_dat[['resp']] == 0, 'rt'] <- NA

    # fill in block category

    # re-order columns
    run_dat <- run_dat[c('onset', 'duration', 'participant_id', 'session_id', 'run', 'set', 'run_cond',
                         'stim_file_name', 'trial_num', 'type', 'block', 'img_cat',
                         'go_stim', 'signal', 'reqSSD' , 'correct', 'resp',
                         'rt', 'trueSSD')]

    # rename run column
    names(run_dat)[names(run_dat) == 'run'] <- 'run_num'

    return(run_dat)
  }

  # save message if onset_dat or fmri_dat missing, else process
  if (have_fmri_dat == 0 & have_onset_dat == 1) {
    func_run_dfs = paste(sub_str, 'has fmri onsets but not resp data. sst bold_events data was not processed')
  } else if (have_fmri_dat == 1 & have_onset_dat == 0) {
    func_run_dfs = paste(sub_str, ' has fmri resp data but onset data. sst bold_events data was not processed')
  } else if (have_fmri_dat == 0 & have_onset_dat == 0) {
    func_run_dfs = paste(sub_str, ' does not have fmri resp data or onset data. sst bold_events data was not processed')

  } else if (have_fmri_dat == 1 & have_onset_dat == 1) {

    # update columns names
    names(onset_dat)[names(onset_dat) == 'stim'] <- 'stim_file_name'
    names(fmri_dat)[names(fmri_dat) == 'stimName'] <- 'stim_file_name'

    # Add column indicating trial number (trial = event when stim_file_name contains '.jpeg')
    ## trial number corresponds to the order within the txt file
    ## adding this column allows for accurate merging of onset_dat and fmri_dat below

    fmri_dat['trial_num'] <- seq.int(nrow(fmri_dat))
    onset_dat['trial_num'] <- ifelse(grepl('.jpeg', onset_dat[['stim_file_name']]),
                                     cumsum(grepl('.jpeg', onset_dat[['stim_file_name']])),
                                     NA)

    # combine onset_dat (onset data) and fmri_dat (resp data) into func_dat
    func_dat <- merge(onset_dat, fmri_dat, by=c('run', 'set','run_cond','stim_file_name', 'trial_num'), all = TRUE)
    func_dat <- func_dat[order(func_dat[['onset_time']]),] #order by onset_time

    # add subject column
    func_dat['participant_id'] <- sub_str
    func_dat['session_id'] <- 'ses-1'

    # assign type
    func_dat['type'] = 'fMRI'

    # update column names
    names(func_dat)[names(func_dat) == 'rt1'] <- 'rt'
    names(func_dat)[names(func_dat) == 'resp1'] <- 'resp'
    names(func_dat)[names(func_dat) == 'stim'] <- 'go_stim'


    # split data by run, process onset and duration, save into run_dfs
    run_dfs <- sapply(unique(func_dat$run), function(x) run_proc(x, func_dat), simplify = FALSE)
    names(run_dfs) <- sapply(unique(func_dat$run), function(x) paste0('run-0', x))

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
      utils::write.table(run_dat, outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = 'n/a' )
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

