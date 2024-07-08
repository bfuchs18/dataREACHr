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

  #### Extract onset and duration data ####

  # initialize dataframe to save commerical block onsets and durations to -- extract durations because these can vary by commerical
  commercial_onsets <- data.frame(run = integer(),
                                  commercial_cond = character() ,
                                  vid_block = character(),
                                  onset = double(),
                                  onset_tr = double(),
                                  duration = double(),
                                  stringsAsFactors = FALSE)

  # initialize dataframe to save image block onsets to
  commercial_onsets <- data.frame(run = integer(),
                                  commercial_cond = character() ,
                                  vid_block = character(),
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
            commercial_cond = ad_cond,
            vid_block = block,
            onset = min(block_cond_rows$onset), # onset for first commercial in a block
            onset_tr = min(block_cond_rows$onset) / 2, #divide onset by TR (2)
            duration = sum(block_cond_rows$duration) # sum duration of both commercials
          )

        # add row to dataframe
        commercial_onsets <- dplyr::bind_rows(commercial_onsets, commercial_onsets_row)
      }
    }

    ##### Get food image block onset times #####

    # extract onset times for the following conditions:

    ## post_food_image_blocks =
    ## post_toy_image_blocks =

    ## by image type??
    ## post_food_image_blocks_led_savory =
    ## post_food_image_blocks_led_sweet =
    ## post_food_image_blocks_hed_savory =
    ## post_food_image_blocks_hed_sweet =
    ## post_toy_image_blocks_led_savory =
    ## post_toy_image_blocks_led_sweet =
    ## post_toy_image_blocks_hed_savory =
    ## post_toy_image_blocks_hed_sweet =

  }

  #### Make AFNI onset files ####

  #### Return data ####
  if (isTRUE(return_data)) {
    return(list())
  }
}

