#' deriv_foodview_onsets: Generate onset files for foodview task
#'
#' This function generates onset files for the foodview task formatted for analyses in AFNI
#'
#' @param sub subject label used in sub-label. Leading zeros not required
#' @param ses session label used in ses-label. Default = 1
#' @param bids_wd string with full path to bids directory -- this directory contains rawdata/
#' @param overwrite logical indicating if onset files should be overwritten in /derivatives. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = TRUE
#'
#' @return If return_data is set to TRUE, will return XXX
#'
#' @examples
#'
#' \dontrun{
#' # generate onset files
#' sub_001_fv_onsets <- deriv_foodview_onsets(sub = 001, ses = 1, bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids", return = TRUE)
#'
#' }
#' @importFrom utils read.table
#' @importFrom rlang .data
#' @export

deriv_foodview_onsets <- function(sub, ses = 1, bids_wd, overwrite = FALSE, return_data = TRUE) {

  # bids_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids"

  #### Set up/initial checks ####

  # check that bids_wd exist and is a string
  data_arg <- methods::hasArg(bids_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(bids_wd)) {
      stop("bids_wd must be entered as a string")
    } else if (!file.exists(bids_wd)) {
      stop("bids_wd entered, but directory does not exist. Check bids_wd string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("bids_wd must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('deriv_foodview_onsets.R has not been thoroughly tested on Windows systems, may have data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # Get subject number without leading zeros
  sub_num <- as.numeric(sub)

  # Set sub and ses strings
  sub_str <- sprintf("sub-%03d", sub_num)
  ses_str <- paste0("ses-", ses)

  #### Check events files ####

  # define directory with events files
  events_dir <- paste0(bids_wd, slash, 'rawdata', slash, sub_str, slash, ses_str, slash, 'func', slash)

  # get list of rrv files in sourcedata
  events_files <- list.files(events_dir, pattern = "foodview.*bold_events", full.names = TRUE)

  # Abort processing if number of foodview events files is 0 or greater than number of runs
  if (length(events_files) == 0) {
    print(paste(sub_str, "has 0 events files for Food View task. Aborting onset generation for this sub."))
    return()
  } else if (length(events_files) > 4) {
    print(paste(sub_str, "has > 4 events files for Food View task. Aborting onset generation for this sub."))
    return()
  }

  #### Extract onset and duration data for each block ####

  # initialize dataframe to save block onsets and durations to
  onsets <- data.frame(run = integer(),
                                  trial_type = character() ,
                                  onset = double(),
                                  onset_tr = double(),
                                  duration = double(),
                                  stringsAsFactors = FALSE)

  # extract data for each events file (i.e., run)
  for (file_path in events_files) {

    # extract file basename
    file_name = basename(file_path)

    # extract run number (i.e., the 2 characters after "run-")
    run <- stringr::str_extract(file_name, "(?<=run-).{2}")

    # read in file
    events_dat <- read.table(file_path, header = TRUE)

    ##### Get commercial blocks onsets and durations #####

    # extract video rows
    video_rows <- events_dat[grep("mp4", events_dat$stim_file), ]

    # add video block column
    video_rows <- video_rows %>%
      mutate(vid_block = stringr::str_extract(stim_file, "(?<=block)."))


    # extract onsets and durations by ad condition and block
    for (ad_cond in c("toy", "food")) {

      ad_cond_rows <- video_rows[grep(ad_cond, video_rows$stim_file), ]

      for (block in unique(ad_cond_rows$vid_block) ) {

        block_cond_rows <- ad_cond_rows[ad_cond_rows$vid_block == block,]

        commercial_onsets_row <-
          data.frame(
            run = as.integer(run),
            trial_type = paste0("ad_", ad_cond),
            onset = min(block_cond_rows$onset), # onset for first commercial in a block
            onset_tr = min(block_cond_rows$onset) / 2, #divide onset by TR (2)
            duration = sum(block_cond_rows$duration) # sum duration of both commercials
          )

        # add row to dataframe
        onsets <- dplyr::bind_rows(onsets, commercial_onsets_row)
      }
    }

    ##### Get food image block onset times #####

    # extract onset times for food image blocks:

    for (food_cond in c("hed_savory", "hed_sweet", "led_savory", "led_sweet") ) {

      cond_rows <- events_dat[grep(food_cond, events_dat$stim_file), ]

      image_block_onsets_row <-
        data.frame(
          run = as.integer(run),
          trial_type = paste0(food_cond, "_", unique(cond_rows$commercial_cond), "_cond"),
          onset = min(cond_rows$onset), # onset for first image in a block
          onset_tr = min(cond_rows$onset) / 2, #divide onset by TR (2)
          duration = (max(cond_rows$onset) + cond_rows[which.max(cond_rows$onset),]$duration) - min(cond_rows$onset)
        )

      # add row to dataframe
      onsets <- dplyr::bind_rows(onsets, image_block_onsets_row)
    }

  }

  #### Make AFNI onset files for each trial_type ####

  onset_dir <- paste0(bids_wd, slash, 'derivatives', slash, 'afni', slash, 'foodview_onsets', slash)

  # create onset_dir if it doesnt exist
  if (!file.exists(onset_dir)){
    dir.create(onset_dir, recursive = TRUE)
  }

  # for each condition
  for (trial_type in unique(onsets$trial_type)) {

    trial_type_dat <- onsets[onsets$trial_type == trial_type ,]

    # get 1 line of data per run
    for (run in seq(max(onsets$run))) {
      run_dat <- trial_type_dat[trial_type_dat$run == run,]

      if (nrow(run_dat) == 0) {
        line = '*'

      } else if (nrow(run_dat) == 1) {
        line = run_dat$onset

      } else {
        # make onset times tab separated
        line <- paste(run_dat$onset, collapse = "\t")
      }

      onset_file_name = paste0(onset_dir, sub_str, "_onsets_", trial_type, ".txt")
      write(line, file = onset_file_name,
            append = TRUE, sep = "\n")
    }
  }


  #### Return data ####
  if (isTRUE(return_data)) {
    return(onsets)
  }
}

