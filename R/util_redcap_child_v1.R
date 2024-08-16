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

  #### Set up/initial checks #####

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

  # add session column
  data$session_id <- "ses-1"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v1_post_check_notes', 'v1_date', 'child_assent', 'dxa_notes', 'rrv_task_notes')]
  names(visit_data_child)[names(visit_data_child) == "v1_post_check_notes"] <- "v1_notes"

  ## intake-related data ####

  ## food paradigm information (does not include intake and freddy values)
  food_paradigm_info <- data[, grepl('participant_id|session_id|meal|advertisement_condition', names(data))]
  food_paradigm_info <- food_paradigm_info[, !grepl('complete|freddy|consumed', names(food_paradigm_info))]
  names(food_paradigm_info) <- gsub('intake_notes', 'prep_notes', names(food_paradigm_info))

  ## intake_data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data
  intake_data <- data[, grep("participant_id|session_id|plate", names(data))]

  ## freddy data -- this may or may not be replaced with double entry data
  freddy_data <- data[, grepl('participant_id|session_id|freddy', names(data))]
  freddy_data <- freddy_data[, -grep("complete|check|time|visit_number", names(freddy_data))]
  colnames(freddy_data) <- gsub("freddy", "fullness", colnames(freddy_data)) # Replace "freddy" with "fullness" in colnames

  ## vas food liking (eah and meal foods)
  liking_data <- data[, grepl('participant_id|session_id|vas', names(data))]
  names(liking_data) <- gsub('cookie', 'oreo', names(liking_data))
  liking_data <- liking_data[, -grep("pre_vas_freddy", names(liking_data))]
  colnames(liking_data) <- gsub("vas", "liking", colnames(liking_data)) # Replace "vas" with "liking" in colnames

  ## kbas data ####
  kbas_data <-data[, grep("participant_id|session_id|toy_|food_|^q.*score|kids_brand_awareness_survey_version_b_timestamp|kids_brand_awareness_survey_version_a_timestamp", names(data))]
  # process data
  kbas_data <- util_format_kbas_data(kbas_data)

  ## stq data ####
  stq_data <-data[, grep("participant_id|session_id|stq|child_screen_time_questionnaire_timestamp", names(data))]
  stq_data$stq_form_date <- lubridate::as_date(stq_data$child_screen_time_questionnaire_timestamp) # add form date column
  stq_data <- stq_data[, -grep("missingcheck|timestamp", names(stq_data))] # remove extra columns
  #score?

  ## anthro data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data ####
  anthro_data <- data[, grep("participant_id|session|parent_height|child_height|parent_weight|child_weight|child_average_weight", names(data))]
  anthro_data <- anthro_data[, -grep("complete|self_report", names(anthro_data))] #self-report will be merged from household questionnaire -- this is automatically taken from there (not entered data)

  # Update columns names
  colnames(anthro_data) <- gsub("parent_", "parent1_", colnames(anthro_data))

  # rename parent1 sex variable
  colnames(anthro_data) <- gsub("parent1_height_sex", "parent1_sex", colnames(anthro_data))

  # re-label parent1 sex
  anthro_data$parent1_sex <- ifelse(anthro_data$parent1_sex == 0, "female", ifelse(anthro_data$parent1_sex == 1, "male", NA))

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                food_paradigm_info = food_paradigm_info,
                intake_data = intake_data,
                freddy_data = freddy_data,
                liking_data = liking_data,
                kbas_data = kbas_data,
                stq_data = stq_data,
                anthro_data = anthro_data))
  }

}

