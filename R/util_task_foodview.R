#' util_task_foodview: Clean and organize Food View task data into BIDS rawdata
#'
#' This function formats and organizes Food View task data from bids/sourcedata into bids/rawdata for a given subject
#'
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
#' list_of_cleaned_data <- util_task_foodview_orgraw(sub = 001, ses = 1, bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids", return = TRUE)
#'
#' }
#'
#'
#' @export

util_task_foodview <- function(sub, ses = 1, bids_wd, overwrite = FALSE, return_data = FALSE) {

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
    print('util_task_foodview_orgraw.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # Get subject number without leading zeros
  sub_num <- as.numeric(sub)

  # Set sub and ses strings
  sub_str <- sprintf("sub-%03d", sub_num)
  ses_str <- paste0("ses-", ses)

  # get directory paths
  raw_wd <- paste0(bids_wd, slash, 'rawdata', slash, sub_str, slash, ses_str, slash, 'func', slash)
  onset_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'foodview_onsets-', sub_num, '.txt')
  resp_source_file <- paste0(bids_wd, slash, 'sourcedata', slash, sub_str, slash, ses_str, slash, 'beh', slash, 'foodview-', sub_num, '.txt')

  #### Organize Data #####

  # load data
  onset_dat <- read.delim(onset_source_file)
  resp_dat <- read.delim(resp_source_file)

  # update columns names
  names(resp_dat)[names(resp_dat) == "stimName"] <- "stim"
  names(onset_dat)[names(onset_dat) == "commercial_condfood_cond"] <- "commercial_cond"

  # combine onset and response data
  dat <- merge(onset_dat, resp_dat, by=c("run", "set","food_cond","commercial_cond","stim"), all = TRUE)
  dat <- dat[order(dat$onset_time),] #order by onset_time

  # add subject column
  dat$sub <- sub_str

  # transform onset_time to start at 0 and be in seconds
  dat$onset <- (dat$onset_time - min(dat$onset_time))/1000
  dat <- dat[,!(names(dat) %in% c("onset_time"))] # remove original onset_time column

  # add duration column -- calculated based on onsets except for final fixation
  for (row in 1:nrow(dat)) {
    if (row < nrow(dat)) {
      dat[row, "duration"] <- round(dat[row+1,"onset"] - dat[row,"onset"], 2)
    } else if (row == nrow(dat) & dat[row, "stim"] == "fix") {
      dat[row, "duration"] <- .5 #set to 0.5 seconds based on task program
    }
  }

  # clean rt (response time) time column
  ## convert rt from ms to sec
  dat$rt <- dat$rt/1000

  ## make rt = n/a when dat$resp = 0 (indicating no response)
  dat$rt[dat$resp == 0] <- "n/a"

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

  # Replace all NA values with "n/a" for BIDS compliance
  dat[is.na(dat)] <- "n/a"

  # update names
  names(dat)[names(dat) == "stim"] <- "stim_file"
  names(dat)[names(dat) == "rt"] <- "response_time"

  # re-order columns
  dat <- dat[c('onset', 'duration', 'sub', 'run', 'commercial_cond', 'stim_file', 'resp', 'response_time' , 'food_ed', 'food_taste')]

  # split data by run and save into run_dfs
  run_dfs <- list()
  unique_runs <- unique(dat$run)
  for (run in unique_runs) {

    run_label <- paste0("run", run)

    # append to run_dfs
    run_dfs[[run_label]] <- dat[(dat$run == run),]
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
    outfile <- paste0(raw_wd, sub_str, '_ses-', ses, '_task-foodview_', run_label, '_bold_events.tsv')

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

