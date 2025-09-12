#' util_task_pit: Clean and organize PIT data into BIDS rawdata
#'
#' This function formats and organizes RRV data from bids/sourcedata into bids/rawdata for a given subject
#'
#'@inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_task_foodview
#' @inheritParams util_copy_to_source
#'
#'
#' @return statement of task completed
#'
#' @examples
#'
#' \dontrun{
#' # process task data for the Food View Task
#' pit_data <- util_task_pit(sub_str = 'sub-001', ses_str = 'ses-1', base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids", return = TRUE)
#'
#' }
#' @importFrom utils read.table
#' @importFrom rlang .data
#' @export

util_task_pit <- function(sub_str, ses_str, base_wd, overwrite = FALSE) {

  #### Check args #####

  # check that base_wd_arg exist and is a string
  base_wd_arg <- methods::hasArg(base_wd)

  if (isTRUE(base_wd_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(base_wd)) {
    stop("base_wd must be entered as a string")
  }

  #### Import Data #####
  #print(sub_str)

  # get directory paths
  source_beh_wd <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh', paste0(sub_str, '_', ses_str, '_task-pit.tsv'))

  # load data, abort processing no file or >1 file matches pattern
  if (file.exists(source_beh_wd)) {
    pit_data <- read.table(source_beh_wd, header = TRUE, sep = '\t', na.strings='n/a')
  } else {
    print(paste(sub_str, "has no PIT tsv."))
    return()
  }

  #### Clean data #####
  pit_data['participant'] <- sub_str

  # add sesion column
  pit_data['ses'] <- ses_str

  #rt values
  pit_data['precs_key.rt'] <- ifelse(('precs_key.rt' %in% names(pit_data)), pit_data[['precs_key.rt']], NA)
  pit_data['cs_key.rt'] <- ifelse(('cs_key.rt' %in% names(pit_data)), pit_data[['cs_key.rt']], NA)
  pit_data['iti_key.rt'] <- ifelse(('iti_key.rt' %in% names(pit_data)), pit_data[['iti_key.rt']], NA)


  # reduce columns
  pit_data <- pit_data[c('participant', 'ses', 'date',  'cond', 'cs_img', 'outcome_img', 'side', 'PavBlocks.thisIndex', 'PavTrialLoop.thisIndex', 'PavFixation.started', 'PavFixTrial.started', 'food_img', 'toy_img', 'InstLoop.thisN', 'inst_key.started', 'inst_img.started', 'left', 'right', 'Food', 'Toy', 'PITLoop.thisTrialN', 'pit_prefix.started', 'precs_right', 'precs_left', 'precs_key.rt', 'cs_right.started', 'cs_right', 'cs_left', 'cs_key.rt', 'iti_fix.started', 'iti_right', 'iti_left', 'iti_key.rt', 'rec_q.started', 'cs_rec_img', 'left_rec_img', 'right_rec_img', 'rec_key.rt', 'rec_key.keys', 'cor', 'psychopyVersion', 'frameRate')]

  # rename variables
  names(pit_data) <- gsub('\\.', '_', names(pit_data))
  names(pit_data) <- tolower(names(pit_data))

  names(pit_data)[names(pit_data) == 'participant'] <- 'sub'
  names(pit_data)[names(pit_data) == 'date'] <- 'visit_date'
  names(pit_data)[names(pit_data) == 'outcome_img'] <- 'pav_stim_img'
  names(pit_data)[names(pit_data) == 'side'] <- 'pav_side'
  names(pit_data)[names(pit_data) == 'pavblocks_thisindex'] <- 'pav_block_num'
  names(pit_data)[names(pit_data) == 'pavtrialloop_thisindex'] <- 'pav_trial_num'
  names(pit_data)[names(pit_data) == 'pavfixation_started'] <- 'pav_fix_onset'
  names(pit_data)[names(pit_data) == 'pavfixtrial_started'] <- 'pav_trial_onset'
  names(pit_data)[names(pit_data) == 'food_img'] <- 'inst_food_img'
  names(pit_data)[names(pit_data) == 'toy_img'] <- 'inst_toy_img'
  names(pit_data)[names(pit_data) == 'instloop_thisn'] <- 'inst_trial_num'
  names(pit_data)[names(pit_data) == 'inst_key_started'] <- 'inst_trial_onset'
  names(pit_data)[names(pit_data) == 'inst_img_started'] <- 'inst_outcome_img_onset'
  names(pit_data)[names(pit_data) == 'left'] <- 'inst_left_trial'
  names(pit_data)[names(pit_data) == 'right'] <- 'inst_right_trial'
  names(pit_data)[names(pit_data) == 'food'] <- 'inst_food_earned'
  names(pit_data)[names(pit_data) == 'toy'] <- 'inst_toy_earned'
  names(pit_data)[names(pit_data) == 'pit_loop_thistrialn'] <- 'pit_trial_num'
  names(pit_data)[names(pit_data) == 'pit_prefix_started'] <- 'pit_fix_onset'
  names(pit_data)[names(pit_data) == 'precs_right'] <- 'pit_pre_right_n'
  names(pit_data)[names(pit_data) == 'precs_left'] <- 'pit_pre_left_n'
  names(pit_data)[names(pit_data) == 'cs_right_started'] <- 'pit_cs_onset'
  names(pit_data)[names(pit_data) == 'cs_right'] <- 'pit_cs_right_n'
  names(pit_data)[names(pit_data) == 'cs_left'] <- 'pit_cs_left_n'
  names(pit_data)[names(pit_data) == 'iti_fix_started'] <- 'pit_iti_onset'
  names(pit_data)[names(pit_data) == 'iti_right'] <- 'pit_iti_right_n'
  names(pit_data)[names(pit_data) == 'iti_left'] <- 'pit_iti_left_n'
  names(pit_data)[names(pit_data) == 'rec_q_started'] <- 'rectest_onset'
  names(pit_data)[names(pit_data) == 'cs_rec_img'] <- 'rectest_cs_img'
  names(pit_data)[names(pit_data) == 'left_rec_img'] <- 'rectest_left_img'
  names(pit_data)[names(pit_data) == 'right_rec_img'] <- 'rectest_right_img'
  names(pit_data)[names(pit_data) == 'rec_key_rt'] <- 'rectest_rt'
  names(pit_data)[names(pit_data) == 'rec_key_keys'] <- 'rectest_key'
  names(pit_data)[names(pit_data) == 'cor'] <- 'rectest_correct'
  names(pit_data)[names(pit_data) == 'psychopyversion'] <- 'psychopy_ver'
  names(pit_data)[names(pit_data) == 'framerate'] <- 'frame_rate'



  # update index values
  pit_data['pav_block_num'] <- pit_data[['pav_block_num']] + 1
  pit_data['pav_trial_num'] <- pit_data[['pav_trial_num']] + 1
  pit_data['inst_trial_num'] <- pit_data[['inst_trial_num']] + 1

  # code correct responses
  pit_data['rectest_correct'] <- ifelse(pit_data[['rectest_key']] == pit_data[['rectest_correct']], 1, 0)

  #### Export Data  #####

  # make raw beh directory if it doesn't exist
  raw_beh_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'beh')

  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # export files if don't exist or overwrite = TRUE
  beh_outfile <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-pit_events.tsv'))
  if (!file.exists(beh_outfile) | isTRUE(overwrite)) {
    utils::write.table(pit_data, beh_outfile, sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a")
  }


  #### Return data #####

  if (isTRUE(return_data)) {
    return(pit_data)
  }
}

