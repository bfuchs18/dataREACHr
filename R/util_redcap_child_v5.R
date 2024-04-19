#' util_redcap_child_v5: Organize child visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_5_arm_1
#'
#'
#' @param data data from REDCap event child_visit_5_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 1 datasets
#'  2) meta-data/.json for each dataset
#' @importFrom rlang .data

util_redcap_child_v5 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_5_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  # add session column
  data$session_id <- "ses-2"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v5_post_check_notes', 'v5_date', 'dxa_notes')]
  names(visit_data_child)[names(visit_data_child) == "v1_post_check_notes"] <- "v1_notes"

  ## intake information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data

  ## meal data
  meal_data <- data[, grepl('participant_id||session_id|meal|advertisement_condition', names(data))]
  meal_data <- meal_data[, !grepl('complete|freddy|consumed|hrt', names(meal_data))]
  names(meal_data) <- gsub('intake_notes', 'prep_notes', names(meal_data))

  ## meal vas data
  meal_vas_data <- data[, grepl('participant_id|session_id|vas_grilled|vas_chicken|vas_potato|vas_carrot|vas_fruit|vas_water', names(data))]

  ## eah data
  eah_data <- data[, grep("participant_id|session_id|wanting|advertisement_condition|eah_notes|eah_intake_notes", names(data))]
  eah_data <- eah_data[, -grep("complete|timestamp", names(eah_data))]
  names(eah_data) <- gsub('intake_notes', 'prep_notes', names(eah_data))
  names(eah_data) <- gsub('eah_notes', 'eah_protocol_notes', names(eah_data))

  ## eah vas data
  eah_vas_data <- data[, grepl('participant_id|session_id|vas_brownie|vas_corn_chip|vas_chocolate|vas_icecream|vas_cookie|vas_popcorn|vas_pretzel|vas_skittle|vas_starburst', names(data))]
  names(eah_vas_data) <- gsub('cookie', 'oreo', names(eah_vas_data))

  ## hrt ####
  hrt_data <- data[, grep("participant_id|session_id|hrt|food_|^q.*score", names(data))]

  ## Puberty ####
  puberty_data <- data[, grep("participant_id|session_id|^tanner|^childrep", names(data))]
  puberty_data_for_scoring <- util_format_puberty_data(puberty_data, respondent = "child")
  # score
  puberty_scored <- dataprepr::score_pds(puberty_data_for_scoring, respondent = "child", score_base = FALSE, male = "male", female = "female", id = "participant_id")

  ## loc ####
  loc_data <-data[, grep("participant_id|session_id|^loc", names(data))]
  loc_data <- loc_data[, -grep("share_info_parent", names(loc_data))] # remove extra columns

  ## kbas data ####
  kbas_data <-data[, grep("participant_id|session_id|toy_|food_|^q.*score|kids_brand_awareness_survey_version_b_timestamp|kids_brand_awareness_survey_version_a_timestamp", names(data))]
  # process data
  kbas_data <- util_format_kbas_data(kbas_data)

  ## stq data ####
  stq_data <-data[, grep("participant_id|session_id|stq|child_screen_time_questionnaire_timestamp", names(data))]
  stq_data$stq_form_date <- lubridate::as_date(stq_data$child_screen_time_questionnaire_timestamp) # add form date column
  stq_data <- stq_data[, -grep("missingcheck|timestamp", names(stq_data))] # remove extra columns
  # score

  ## tictoc data ####
  tictoc_data <-data[, grep("participant_id|tictoc", names(data))]

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_data = meal_data,
                meal_vas_data = meal_vas_data,
                eah_data = eah_data,
                eah_vas_data = eah_vas_data,
                hrt_data = hrt_data,
                puberty_data = puberty_scored,
                loc_data = loc_data,
                kbas_data = kbas_data,
                stq_data = stq_data,
                tictoc_data = tictoc_data))
  }

}

