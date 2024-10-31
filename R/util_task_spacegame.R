#' util_task_spacegame: Process raw data from the Space Game (2-stage reinforcement learning task)
#'
#' This function formats, cleans, and organizes spacegame data (.mat) from bids/sourcedata into rawdata for a given subject
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#' Function adapted from Alaina Pearce dataBRAKEr package (https://github.com/alainapearce/dataBRAKEr/tree/main/R)
#' @param sub subject label used in sub-label. Leading zeros not required
#' @param ses session label used in ses-label. Default = 1
#' @param bids_wd string with full path to bids directory -- this is the directory that contains sourcedata/ and rawdata/
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = TRUE
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#' \dontrun{
#' # process task data
#' bids_wd = "/Users/baf44/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/bids"
#' spacegame_task_pardat <- util_task_spacegame(sub = 001, ses = 1, bids_wd = bids_wd, return = TRUE)
#'
#' }
#'
#'
#' @export

util_task_spacegame <- function(sub, ses = 1, bids_wd, overwrite = FALSE, return_data = FALSE) {

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

  # Get subject number without leading zeros
  sub_num <- as.numeric(sub)

  # Set sub and ses strings
  sub_str <- sprintf("sub-%03d", sub_num)
  ses_str <- paste0("ses-", ses)

  # get directory paths
  source_beh_wd <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'beh')
  source_file <- list.files(source_beh_wd, pattern = "mbmfNovelStakes", full.names = TRUE)

  #### Load file #####

  # load matlab data, abort processing no file or >1 file matches pattern

  if (length(source_file) == 1) {
    matdat <- R.matlab::readMat(source_file)
  } else if ( length(source_file) == 0) {
    print(paste(sub_str, "has no spacegame file in sourcedata. Aborting task processing for this sub."))
    return()
  } else if (length(source_file) > 1) {
    print(paste(sub_str, "has more than 1 spacegame file in sourcedata. Should only have 1. Aborting task processing for this sub."))
    return()
  }

  #### Organize Data #####

  # process .mat file
  dat_list <- matdat[['data']]

  dat_columns <- c('rews', 'block', 's', 'stimuli', 'stake', 'choice', 'rt', 'score', 'points', 'timeout')
  dat <- cbind.data.frame(sapply(dat_columns, function(x) t(dat_list[row.names(dat_list) == x]), simplify = TRUE))

  # add in sub/session information
  dat[['sub']] <- as.numeric(unlist(dat_list[row.names(dat_list) == 'id']))
  dat[['date']] <- lubridate::date(lubridate::dmy_hms(unlist(dat_list[row.names(dat_list) == 'date'])))

  # add stakes into points calculation
  dat[['points']] <- dat[['points']]

  # rename
  names(dat) <- c('rewards1', 'rewards2', 'block', 'state_earth', 'state_planet', 'stim_left', 'stim_right', 'stake', 'choice_earth', 'rt_earth', 'rt_planet', 'score', 'points', 'timeout_earth', 'timeout_planet', 'sub', 'date')

  # add trials by block
  # dat[dat[['block']] == 1, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 1, ]))
  # dat[dat[['block']] == 2, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 2, ]))
  # dat[dat[['block']] == 3, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 3, ]))
  # dat[dat[['block']] == 4, 'trial'] <- seq(1, nrow(dat[dat[['block']] == 4, ]))

  # kool didn't consider blocks a reset
  dat[['trial']] <- seq(1, nrow(dat), 1)

  # get key press
  dat[['response']] <- ifelse(dat[['timeout_earth']] == 1, 3, ifelse(dat[['choice_earth']] == dat[['stim_left']], 1, 2))

  # get missed states
  dat[['missed_earth']] <- ifelse(dat[['timeout_earth']] == 1, 1, 0)
  dat[['missed_planet']] <- ifelse(dat[['timeout_earth']] == 1 | dat[['timeout_planet']] == 1, 1, 0)

  # get stay for planet by block
  # dat[dat[['block']] == 1, 'stay_planet'] <- sapply(dat[dat[['block']] == 1, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 1 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 1 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  #
  # dat[dat[['block']] == 2, 'stay_planet'] <- sapply(dat[dat[['block']] == 2, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 2 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 2 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  #
  # dat[dat[['block']] == 3, 'stay_planet'] <- sapply(dat[dat[['block']] == 3, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 3 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 3 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))
  #
  # dat[dat[['block']] == 4, 'stay_planet'] <- sapply(dat[dat[['block']] == 4, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 4 & dat[['trial']] == x, 'state_planet'] == dat[dat[['block']] == 4 & dat[['trial']] == x-1, 'state_planet'], 1, 0)))

  # get earth same/different
  # dat[dat[['block']] == 1, 'earth_same'] <- sapply(dat[dat[['block']] == 1, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 1 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 1 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  #
  # dat[dat[['block']] == 2, 'earth_same'] <- sapply(dat[dat[['block']] == 2, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 2 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 2 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  #
  # dat[dat[['block']] == 3, 'earth_same'] <- sapply(dat[dat[['block']] == 3, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 3 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 3 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))
  #
  # dat[dat[['block']] == 4, 'earth_same'] <- sapply(dat[dat[['block']] == 4, 'trial'], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['block']] == 4 & dat[['trial']] == x, 'state_earth'] == dat[dat[['block']] == 4 & dat[['trial']] == x-1, 'state_earth'], 1, 0)))


  # kool didn't consider blocks a reset
  dat[['stay_planet']] <- sapply(dat[['trial']], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['trial']] == x, 'state_planet'] == dat[dat[['trial']] == x-1, 'state_planet'], 1, 0)))

  dat[['earth_same']] <- sapply(dat[['trial']], function(x) ifelse(x == 1, NA, ifelse(dat[dat[['trial']] == x, 'state_earth'] == dat[dat[['trial']] == x-1, 'state_earth'], 1, 0)))

  # re-order columns
  dat <- dat[c('sub', 'date', 'block', 'trial', 'timeout_earth', 'timeout_planet', 'state_earth', 'state_planet', 'stim_left', 'stim_right', 'rt_earth', 'rt_planet', 'choice_earth', 'response', 'points', 'stake', 'score', 'rewards1', 'rewards2', 'missed_earth', 'missed_planet', 'earth_same', 'stay_planet')]

  #### Save in rawdata #####

  # create bids/rawdata directory if it doesn't exist
  raw_beh_wd <- file.path(bids_wd, 'rawdata', sub_str, ses_str, 'beh')

  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # define output file with path
  outfile <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-spacegame_beh.tsv'))

  # export file if doesn't exist or overwrite = TRUE
  if (!file.exists(outfile) | isTRUE(overwrite)) {
    utils::write.table(
      dat,
      outfile,
      sep = '\t',
      quote = FALSE,
      row.names = FALSE,
      na = "n/a" # use 'n/a' for missing values for BIDS compliance
    )
  }

  if (isTRUE(return_data)){
    return(dat)
  }
}
