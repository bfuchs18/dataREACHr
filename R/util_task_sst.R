#' util_task_sst: Clean and organize Stop Signal Task data into BIDS rawdata
#'
#' This function formats and organizes Stop Signal Task data from bids/sourcedata into bids/rawdata for a given subject
#' Data from practice and behavioral runs will be organized into bids/rawdata/beh, while data from fmri runs will be organized into bids/rawdata/func.
#'
#' @param sub subject label used in sub-label. Leading zeros not required
#' @param ses session label used in ses-label. Default = 1
#' @param bids_wd string with full path to bids directory -- this is the directory that contains sourcedata/ and rawdata/
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = FALSE
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
#'
#'
#' @export

util_task_sst <- function(sub, ses = 1, bids_wd, overwrite = FALSE, return_data = FALSE) {

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

  # get directory paths
  raw_wd <- paste0(bids_wd, slash, 'rawdata', slash, sub_str, slash, ses_str, slash, 'func', slash)
  prac_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_prac-', sub_num, '.txt')
  beh_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_beh-', sub_num, '.txt')
  onset_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_onsets-', sub_num, '.txt')
  fmri_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'stop_fmri-', sub_num, '.txt')

  # load data
  prac_dat <- read.table(prac_source_file, header = TRUE)
  beh_dat <- read.table(beh_source_file, header = TRUE)
  onset_dat <- read.table(onset_source_file, header = TRUE)
  fmri_dat <- read.table(fmri_source_file, header = TRUE)

  # update columns names
  names(prac_dat)[names(prac_dat) == "stimName"] <- "stim_file"
  names(beh_dat)[names(beh_dat) == "stimName"] <- "stim_file"
  names(onset_dat)[names(onset_dat) == "stim"] <- "stim_file"
  names(fmri_dat)[names(fmri_dat) == "stimName"] <- "stim_file"

  #### Clean Beh Data (prac_dat and beh_dat) #####

  # add subject column
  prac_dat$sub <- sub_str
  beh_dat$sub <- sub_str

  #### Clean Func Data (onset_dat and fmri_dat) #####

  # Add column indicating trial number (trial = event when stim_file contains ".jpeg")
  ## trial number corresponds to the order within the txt file
  ## adding this column allows for accurate merging of onset_dat and fmri_dat below

  fmri_dat$trial_num <- seq.int(nrow(fmri_dat))
  onset_dat <- onset_dat %>%
    dplyr::mutate(trial_num = ifelse(grepl(".jpeg", stim_file),
                                     cumsum(grepl(".jpeg", stim_file)),
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

  # split data by run, process onset and duration, save into run_dfs
  run_dfs <- list()
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
    # run_dat[is.na(run_dat)] <- "n/a"

    # re-order columns
    run_dat <- run_dat[c('onset', 'duration', 'sub', 'run', 'set', 'run_cond',
                         'stim_file', 'trial_num', 'type', 'block', 'img_cat',
                         'go_stim', 'signal', 'reqSSD' , 'correct', 'response',
                         'response_time', 'trueSSD')]

    # append to run_dfs
    run_dfs[[run_label]] <- run_dat
  }


  #### Save in rawdata #####

  # create bids/rawdata directory if it doesn't exist
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }

  # for each run in run_dfs, export data
  for (runnum in 1:length(run_dfs)) {

    # extract data for run
    run_dat <- run_dfs[[runnum]]

    # format run_label for output file
    run_label <- gsub('run', 'run-0', names(run_dfs)[runnum])

    # define output file with path
    outfile <- paste0(raw_wd, sub_str, '_ses-', ses, '_task-sst_', run_label, '_bold_events.tsv')

    # export file if doesn't exist or overwrite = TRUE
    if (!file.exists(outfile) | isTRUE(overwrite)) {
      utils::write.table(run_dat, outfile, sep = '\t', quote = FALSE, row.names = FALSE )
    }
  }

  #### Return data #####
  if (isTRUE(return_data)){
    return(run_dfs)
  }
}

