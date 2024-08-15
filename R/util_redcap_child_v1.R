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

  ## intake data ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data

  ## meal data
  meal_data <- data[, grepl('participant_id|session_id|meal|advertisement_condition', names(data))]
  meal_data <- meal_data[, !grepl('complete|freddy|consumed', names(meal_data))]
  names(meal_data) <- gsub('intake_notes', 'prep_notes', names(meal_data))

  ## intake_data (meal and eah) -- this data can be used for prelim analyses, but eventually will be replaced with double entry data
  intake_data <- data[, grep("participant_id|session_id|meal|eah_notes|advertisement_condition|bread|butter|cheese|tender|carrot|chips|fruit|water|ranch|ketchup|meal|brownie|corn_chip|kiss|ice_cream|oreo|popcorn|pretzel|skittle|starburst|eah", names(data))]
  intake_data <- intake_data[, -grep("complete|intake_eah_visit_number|check|consumed", names(intake_data))]
  colnames(intake_data) <- gsub("freddy", "fullness", colnames(intake_data)) # Replace "freddy" with "fullness" in colnames
  colnames(intake_data) <- gsub('intake_notes', 'prep_notes', names(intake_data))

  ## meal vas data
  meal_vas_data <- data[, grepl('participant_id|session_id|vas_grilled|vas_chicken|vas_potato|vas_carrot|vas_fruit|vas_water', names(data))]

  ## eah vas data
  eah_vas_data <- data[, grepl('participant_id|session_id|brownie|corn_chip|chocolate|icecream|cookie|popcorn|pretzel|skittle|starburst', names(data))]
  names(eah_vas_data) <- gsub('cookie', 'oreo', names(eah_vas_data))

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

  # calculate parent1 BMI
#  anthro_data$parent1_bmi <- round(anthro_data$parent1_weight_average_kg / ((anthro_data$parent1_height_average_cm / 100) ^ 2), digits = 2)

  # calculate child BMI
#  anthro_data$child_bmi <- round(anthro_data$child_average_weight / ((anthro_data$child_height_average / 100) ^ 2), digits = 2)
#  anthro_data$child_bmi_z <- NA # calculate this
#  anthro_data$child_bmi_p <- NA # calculate this

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_data = meal_data,
                intake_data = intake_data,
                meal_vas_data = meal_vas_data,
                eah_vas_data = eah_vas_data,
                kbas_data = kbas_data,
                stq_data = stq_data,
                anthro_data = anthro_data))
  }

}

