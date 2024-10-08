#' util_redcap_child_v3: Organize child visit 3 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_3_arm_1
#'
#' @param data data from REDCap event child_visit_3_arm_1
#' @param return_data If return_data is set to TRUE, will return a list of dataframes including: visit_data_child, food_paradigm_info, eah_wanting, freddy_data, intake_data, sleeplog_data

util_redcap_child_v3 <- function(data, return_data = TRUE) {

  #### Set up/initial checks #####

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

  ## intake-related data ####

  # food paradigm information (does not include intake and freddy values)
  food_paradigm_info <- data[, grepl('participant_id|session_id|meal|advertisement_condition|eah_notes|eah_intake_notes', names(data))]
  food_paradigm_info <- food_paradigm_info[, !grepl('complete|freddy|consumed', names(food_paradigm_info))]
  names(food_paradigm_info) <- gsub('intake_notes', 'prep_notes', names(food_paradigm_info))

  # eah wanting
  eah_wanting <- data[, grep("participant_id|session_id|wanting", names(data))]
  eah_wanting <- eah_wanting[, -grep("complete|timestamp", names(eah_wanting))]

  ## intake_data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data
  intake_data <- data[, grep("participant_id|session_id|plate", names(data))]

  ## freddy data -- this may or may not be replaced with double entry data
  freddy_data <- data[, grepl('participant_id|session_id|freddy', names(data))]
  freddy_data <- freddy_data[, -grep("complete|check|time|visit_number", names(freddy_data))]
  colnames(freddy_data) <- gsub("freddy", "fullness", colnames(freddy_data)) # Replace "freddy" with "fullness" in colnames

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
                food_paradigm_info = food_paradigm_info,
                eah_wanting = eah_wanting,
                freddy_data = freddy_data,
                intake_data = intake_data,
                sleeplog_data = sleeplog_data))
  }
}

