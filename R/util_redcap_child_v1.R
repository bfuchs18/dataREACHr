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

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v1_post_check_notes', 'v1_date', 'child_assent', 'dxa_notes', 'rrv_task_notes')]
  names(visit_data_child)[names(visit_data_child) == "v1_post_check_notes"] <- "v1_notes"

  ## meal information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data
  meal_info <- data[, grepl('vas|test_meal', names(data)) |
                         names(data) %in% c('participant_id', 'advertisement_condition', 'meal_intake_notes', 'test_meal_notes')]

  meal_info <- meal_info[, !grepl('test_meal_protocol_complete', names(meal_info))]

  ## kbas ####
  kbas_data <-data[, grep("participant_id|toy_|food_|^q.*score", names(data))]
  # score this data?

  ## stq data ####
  stq_data <-data[, grep("participant_id|stq", names(data))]

  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_info = meal_info,
                kbas_data = kbas_data,
                stq_data = stq_data))
  }

}

