#' util_redcap_child_v1: Organize child visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_1_arm_1
#'
#'
#' @param data data from REDCap event child_visit_1_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_child_v1 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_1_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  #reduce columns and update names

  # ## demo ####
  # child_v1demo_data <- data[c('record_id', 'v1_general_notes', 'relationship', 'c_height1_cm', 'c_height2_cm', 'c_weight1_kg', 'c_weight2_kg', 'c_height_avg_cm', 'c_weight_avg_kg', 'c_bmi', 'c_bmi_pcent', 'c_weightstatus', 'p_height1_cm', 'p_height2_cm', 'p_weight1_kg', 'p_weight2_kg', 'p_height_avg_cm', 'p_weight_avg_kg', 'p_bmi', 'p_weightstatus', 'heightweight_notes')]
  #
  # names(child_v1demo_data)[1] <- 'participant_id'
  #
  # ## household ####
  # child_household_data <- data[c('record_id', 'p_height1_cm', 'p_height2_cm', 'p_weight1_kg', 'p_weight2_kg', 'p_height_avg_cm', 'p_weight_avg_kg', 'p_bmi', 'p_weightstatus', 'heightweight_notes')]
  #
  # names(child_household_data)[1] <- 'participant_id'

  ## meal information ####
  meal_info <- data[, grepl('vas', names(data)) |
                         grepl('test_meal', names(data)) |
                         grepl('freddy', names(data)) |
                         grepl('pre_w_o_plate', names(data)) |
                         grepl('pre_w_plate', names(data)) |
                         grepl('post_w_plate', names(data)) |
                         grepl('consumed', names(data)) |
                         names(data) %in% c('participant_id', 'advertisement_condition', 'meal_intake_notes')]

#  names(v1_meal_info)[c(1, 11:15, 17:20, 39:40, 52, 54:55)] <- c('participant_id', 'meal_book', 'meal_start', 'meal_end', 'meal_duration', 'meal_notes', 'nih_listsort_notes', 'pre_want_ff_time', 'pre_want_ff_notes', 'eah_likewant_time', 'eah_likewant_notes', 'eah_game_want_time', 'eah_game_want_notes', 'eah_start', 'eah_end')

  ## kbas ####
  kbas_data <- data[, grepl('participant_id', names(data)) | grepl('toy_', names(data)) | grepl('food_', names(data)) | grepl("^q.*score", names(data))]

  # score this data?
  # return json

  ## stq data ####
  stq_data <- data[, grepl('participant_id', names(data)) | grepl('stq', names(data))]

  #hfi_scored <- dataprepr::score_hfi(hfi_data, id = 'participant_id', score_base = TRUE)
  #hfi_scored <- score_hfi(hfi_data, id = 'participant_id', score_base = TRUE)
  #hfi_json <- json_hfi()

  ## dexa  ####
  #or will this come from data double entry?

  if (isTRUE(return_data)){
    return(list( meal_info = meal_info,
                 stq_data = stq_data))
  }

  # if (isTRUE(return_data)){
  #   return(list(
  #     demo_data = list(child_v1demo_data = child_v1demo_data,
  #                      child_household_data = child_household_data),
  #     otherdata = list(fnirs_cap = fnirs_cap,
  #                      task_info = task_info,
  #                      meal_info = meal_info),
  #     sleep_wk_data = list(data = sleep_wk_scored, meta = child_sleep_json),
  #     hfi_data = list(data = hfi_scored, meta = hfi_json)))
  # }
}

