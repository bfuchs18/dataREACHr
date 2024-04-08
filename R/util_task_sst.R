#' util_task_sst: Clean and organize Stop Signal Task data into BIDS rawdata
#'
#' This function formats and organizes Stop Signal Task data from bids/sourcedata into bids/rawdata for a given subject
#' Data from practice and behavioral runs will be organized into bids/rawdata/sub-label/beh, while data from fmri runs will be organized into bids/rawdata/sub-label/func.
#'
#' @param sub subject label used in sub-label. Leading zeros not required
#' @param ses session label used in ses-label. Default = 1
#' @param bids_wd string with full path to bids directory -- this is the directory that contains sourcedata/ and rawdata/
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = TRUE
#'
#' @return If return_data is set to TRUE, will return a list with 1 cleaned dataframe per run
#'
#' @examples
#'
#' \dontrun{
#' # process task data for the Food View Task
#' list_of_cleaned_data <- util_task_sst(sub = 001, ses = 1, bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids", return = TRUE)
#'
#' }
#' @importFrom utils read.table
#' @importFrom rlang .data
#' @export

util_task_sst <- function(sub, ses = 1, bids_wd, overwrite = FALSE, return_data = TRUE) {

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
    print('util_task_sst.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # Get subject number without leading zeros
  sub_num <- as.numeric(sub)

  # Set sub and ses strings
  sub_str <- sprintf("sub-%03d", sub_num)
  ses_str <- paste0("ses-", ses)

  #### Load Data #####

  # get file paths
  prac_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_prac-', sub_num, '.txt')
  beh_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_beh-', sub_num, '.txt')
  onset_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_onsets-', sub_num, '.txt')
  fmri_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_fmri-', sub_num, '.txt')

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

  for (i in seq_along(beh_dfs)) {

    # Get the data frame from the list
    dataframe <- beh_dfs[[i]]

    # update column names
    names(dataframe)[names(dataframe) == "stimName"] <- "stim_file"
    names(dataframe)[names(dataframe) == "resp1"] <- "response"
    names(dataframe)[names(dataframe) == "rt1"] <- "response_time"
    names(dataframe)[names(dataframe) == "stim"] <- "go_stim"

    # add subject column
    dataframe$sub <- sub_str

    # replace missing values with "n/a" for bids compliance
    dataframe[is.na(dataframe)] <- "n/a"

    # convert response_time from ms to sec
    dataframe$response_time <- dataframe$response_time / 1000

    # make response_time = n/a when dataframe$response = 0 (indicating no response)
    dataframe$response_time[dataframe$response == 0] <- "n/a"

    # reorder columns
    dataframe <- dataframe[c('sub', 'type', 'run', 'set', 'run_cond', 'block',
                         'stim_file', 'img_cat', 'go_stim', 'signal', 'reqSSD' ,
                         'correct', 'response', 'response_time', 'trueSSD')]

    # split beh by run?

    # assign the modified dataframe back to the list
    beh_dfs[[i]] <- dataframe

  }

  # make raw beh directory if it doesn't exist
  raw_beh_wd <- paste0(bids_wd, slash, 'rawdata', slash, sub_str, slash, ses_str, slash, 'beh', slash)
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # export files if don't exist or overwrite = TRUE

  ## practice data
  prac_outfile <- paste0(raw_beh_wd, sub_str, '_ses-', ses, '_task-sst_acq-practice_beh.tsv')
  if (!file.exists(prac_outfile) | isTRUE(overwrite)) {
    utils::write.table(beh_dfs[[1]], prac_outfile, sep = '\t', quote = FALSE, row.names = FALSE )
  }

  ## task data
  beh_outfile <- paste0(raw_beh_wd, sub_str, '_ses-', ses, '_task-sst_beh.tsv')
  if (!file.exists(beh_outfile) | isTRUE(overwrite)) {
    utils::write.table(beh_dfs[[2]], beh_outfile, sep = '\t', quote = FALSE, row.names = FALSE )
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

    # make raw func directory if it doesn't exist
    raw_func_wd <- paste0(bids_wd, slash, 'rawdata', slash, sub_str, slash, ses_str, slash, 'func', slash)
    if (!dir.exists(raw_func_wd)) {
      dir.create(raw_func_wd, recursive = TRUE)
    }

    # update columns names
    names(onset_dat)[names(onset_dat) == "stim"] <- "stim_file"
    names(fmri_dat)[names(fmri_dat) == "stimName"] <- "stim_file"

    # Add column indicating trial number (trial = event when stim_file contains ".jpeg")
    ## trial number corresponds to the order within the txt file
    ## adding this column allows for accurate merging of onset_dat and fmri_dat below

    fmri_dat$trial_num <- seq.int(nrow(fmri_dat))
    onset_dat <- onset_dat %>%
      dplyr::mutate(trial_num = ifelse(grepl(".jpeg", .data$stim_file),
                                       cumsum(grepl(".jpeg", .data$stim_file)),
                                       NA))

    # combine onset_dat (onset data) and fmri_dat (response data) into func_dat
    func_dat <- merge(onset_dat, fmri_dat, by=c("run", "set","run_cond","stim_file", "trial_num"), all = TRUE)
    func_dat <- func_dat[order(func_dat$onset_time),] #order by onset_time

    # add subject column
    func_dat$sub <- sub_str

    # assign type
    func_dat$type = "fMRI"

    # update column names
    names(func_dat)[names(func_dat) == "rt1"] <- "response_time"
    names(func_dat)[names(func_dat) == "resp1"] <- "response"
    names(func_dat)[names(func_dat) == "stim"] <- "go_stim"

    # split data by run, process onset and duration, save into func_run_dfs, and export
    func_run_dfs <- list()
    unique_runs <- unique(func_dat$run)
    for (run in unique_runs) {

      run_label <- paste0("run", run)
      run_dat <- func_dat[(func_dat$run == run),]

      # transform onset so first stimulus in run occurs at 0, convert to seconds
      run_dat$onset <- (run_dat$onset_time - min(run_dat$onset_time))/1000
      run_dat <- run_dat[,!(names(run_dat) %in% c("onset_time"))] # remove original onset_time column

      # add duration column -- calculated based on onsets except for final fixation
      for (row in 1:nrow(run_dat)) {
        if (row < nrow(run_dat)) {
          run_dat[row, "duration"] <- round(run_dat[row+1,"onset"] - run_dat[row,"onset"], 2)
        } else if (row == nrow(run_dat) & run_dat[row, "stim_file"] == "fix") {
          run_dat[row, "duration"] <- 10 #set to 10 seconds based on task program
        }
      }

      # convert response_time from ms to sec
      run_dat$response_time <- run_dat$response_time/1000

      # make response_time = n/a when func_dat$response = 0 (indicating no response)
      run_dat$response_time[run_dat$response == 0] <- "n/a"

      # fill in block category


      # Replace all NA values with "n/a" for BIDS compliance
      run_dat[is.na(run_dat)] <- "n/a"

      # re-order columns
      run_dat <- run_dat[c('onset', 'duration', 'sub', 'run', 'set', 'run_cond',
                           'stim_file', 'trial_num', 'type', 'block', 'img_cat',
                           'go_stim', 'signal', 'reqSSD' , 'correct', 'response',
                           'response_time', 'trueSSD')]

      # append to func_run_dfs
      func_run_dfs[[run_label]] <- run_dat

      # format run_label for output file
      run_label <- paste0("run-0", run)

      # define output file with path
      outfile <- paste0(raw_func_wd, sub_str, '_ses-', ses, '_task-sst_', run_label, '_bold_events.tsv')

      # export file if doesn't exist or overwrite = TRUE
      if (!file.exists(outfile) | isTRUE(overwrite)) {
        utils::write.table(run_dat, outfile, sep = '\t', quote = FALSE, row.names = FALSE )
      }
    }
  }



  #### Return data #####
  if (isTRUE(return_data)) {
    return(list(prac_data = beh_dfs[[1]],
                beh_data = beh_dfs[[2]],
                func_data = func_run_dfs))
  }
}

