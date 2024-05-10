#' util_redcap_child_v3: Organize child visit 3 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_3_arm_1
#'
#'
#' @param data data from REDCap event child_visit_3_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_child_v3 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_3_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  # add session column
  data$session_id <- "ses-1"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v3_post_check_notes', 'v3_date', 'space_game_notes', 'pit_task_notes')]
  names(visit_data_child)[names(visit_data_child) == "v3_post_check_notes"] <- "v3_notes"

  ## Intake information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data

  # meal data
  meal_data <- data[, grep("participant_id|session_id|test_meal|advertisement_condition|test_meal_notes|meal_intake_notes", names(data))]
  meal_data <- meal_data[, -grep("complete", names(meal_data))]
  names(meal_data) <- gsub('intake_notes', 'prep_notes', names(meal_data))

  # EAH data
  eah_data <- data[, grep("participant_id|session_id|wanting|advertisement_condition|eah_notes|eah_intake_notes", names(data))]
  eah_data <- eah_data[, -grep("complete|timestamp", names(eah_data))]
  names(eah_data) <- gsub('intake_notes', 'prep_notes', names(eah_data))
  names(eah_data) <- gsub('eah_notes', 'eah_protocol_notes', names(eah_data))

  ## intake_data (meal and eah) -- this data can be used for prelim analyses, but eventually will be replaced with double entry data
  intake_data <- data[, grep("participant_id|session_id|meal|eah_notes|advertisement_condition|bread|butter|cheese|tender|carrot|chips|fruit|water|ranch|ketchup|meal|brownie|corn_chip|kiss|ice_cream|oreo|popcorn|pretzel|skittle|starburst|eah", names(data))]
  intake_data <- intake_data[, -grep("complete|intake_eah_visit_number|check|consumed", names(intake_data))]
  colnames(intake_data) <- gsub("freddy", "fullness", colnames(intake_data)) # Replace "freddy" with "fullness" in colnames
  colnames(intake_data) <- gsub('intake_notes', 'prep_notes', names(intake_data))

  ## sleep log ####
  sleeplog_data <-data[, grep("participant_id|session_id|^date|^bedtime|^asleep|^attempt|^times|^waso|^awake|^out_on|^rating|^comment", names(data))]

  names(sleeplog_data) <- gsub('_mon', '_night1', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_tu|_tues', '_night2', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_wed', '_night3', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_th|_thur', '_night4', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_fri', '_night5', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_sat', '_night6', names(sleeplog_data))
  names(sleeplog_data) <- gsub('_sun', '_night7', names(sleeplog_data))

  sleeplog_data <- sleeplog_data %>% dplyr::relocate("session_id", .after = 1)

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_data = meal_data,
                eah_data = eah_data,
                intake_data = intake_data,
                sleeplog_data = sleeplog_data))
  }
}

