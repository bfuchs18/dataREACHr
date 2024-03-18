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

  ## intake information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data

  ## meal data
  meal_data <- data[, grepl('participant_id|meal|advertisement_condition', names(data))]
  meal_data <- meal_data[, !grepl('complete|freddy|consumed', names(meal_data))]
  names(meal_data) <- gsub('intake_notes', 'prep_notes', names(meal_data))

  ## meal vas data
  meal_vas_data <- data[, grepl('participant_id|vas_grilled|vas_chicken|vas_potato|vas_carrot|vas_fruit|vas_water', names(data))]

  ## eah vas data
  eah_vas_data <- data[, grepl('participant_id|brownie|corn_chip|chocolate|icecream|cookie|popcorn|pretzel|skittle|starburst', names(data))]
  names(eah_vas_data) <- gsub('cookie', 'oreo', names(eah_vas_data))

  ## kbas ####
  kbas_data <-data[, grep("participant_id|toy_|food_|^q.*score", names(data))]
  # score this data?

  ## stq data ####
  stq_data <-data[, grep("participant_id|stq", names(data))]

  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_data = meal_data,
                meal_vas_data = meal_vas_data,
                eah_vas_data = eah_vas_data,
                kbas_data = kbas_data,
                stq_data = stq_data))
  }

}

