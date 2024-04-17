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

  ## meal vas data
  meal_vas_data <- data[, grepl('participant_id|session_id|vas_grilled|vas_chicken|vas_potato|vas_carrot|vas_fruit|vas_water', names(data))]

  ## eah vas data
  eah_vas_data <- data[, grepl('participant_id|session_id|brownie|corn_chip|chocolate|icecream|cookie|popcorn|pretzel|skittle|starburst', names(data))]
  names(eah_vas_data) <- gsub('cookie', 'oreo', names(eah_vas_data))

  ## kbas data ####
  kbas_data <-data[, grep("participant_id|session_id|toy_|food_|^q.*score|kids_brand_awareness_survey_version_b_timestamp|kids_brand_awareness_survey_version_a_timestamp", names(data))]

  # relabel version a and b items
  names(kbas_data)[3:105] <- paste0("va_", names(kbas_data)[3:105]) # add va_ to begining of column names
  names(kbas_data)[107:208] <- paste0("vb_", names(kbas_data)[107:208]) # add vb_ to begining of column names
  names(kbas_data) <- gsub('\\_b$|\\_vb$', '', names(kbas_data)) # remove _b and _vb from end of column names

  # add version column
  ## define version items
  va_items <- colnames(kbas_data %>% dplyr::select(dplyr::starts_with("va")) %>% dplyr::select(-dplyr::contains("score")))
  vb_items <- colnames(kbas_data %>% dplyr::select(dplyr::starts_with("vb")) %>% dplyr::select(-dplyr::contains("score")))

  ## assign version -- requires having responses to a given version & no responses to the other version
  kbas_data$kbas_version <- ifelse(rowSums(!is.na(kbas_data[va_items])) > 45 & rowSums(is.na(kbas_data[vb_items])) == 50, "A",
                              ifelse(rowSums(is.na(kbas_data[va_items])) > 45 & rowSums(!is.na(kbas_data[vb_items])) == 50, "B", NA))

  # add form date column -- using dplyr::if_else here because it preserves the type/class of inputs (i.e, dates)
  kbas_data$kbas_form_date <-
    dplyr::if_else(kbas_data$kids_brand_awareness_survey_version_a_timestamp != "",
      lubridate::as_date(kbas_data$kids_brand_awareness_survey_version_a_timestamp),
      lubridate::as_date(kbas_data$kids_brand_awareness_survey_version_b_timestamp)
    )

  # combine va and vb total scored columns
  kbas_data$kbas_food_score <- ifelse(kbas_data$kbas_version == "A", kbas_data$va_food_score_kbas,
                                   ifelse(kbas_data$kbas_version == "B", kbas_data$vb_food_score_kbas, NA))

  kbas_data$kbas_toy_score <- ifelse(kbas_data$kbas_version == "A", kbas_data$va_toy_score_kbas,
                                      ifelse(kbas_data$kbas_version == "B", kbas_data$vb_toy_score_kbas, NA))

  #remove columns
  kbas_data <- kbas_data[, !grepl('timestamp|va_toy_score|vb_toy_score|va_food_score|vb_food_score', names(kbas_data))]

  #reorder columns
  kbas_data <-
    kbas_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) %>% dplyr::relocate(dplyr::contains("kbas_version"), .after = 3) %>% dplyr::relocate(dplyr::contains("kbas_food_score"), .after = 4)  %>% dplyr::relocate(dplyr::contains("kbas_toy_score"), .after = 5)

  ## stq data ####
  stq_data <-data[, grep("participant_id|session_id|stq|child_screen_time_questionnaire_timestamp", names(data))]
  stq_data$stq_form_date <- lubridate::as_date(stq_data$child_screen_time_questionnaire_timestamp) # add form date column
  stq_data <- stq_data[, -grep("missingcheck|timestamp", names(stq_data))] # remove extra columns
  #score?

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_data = meal_data,
                meal_vas_data = meal_vas_data,
                eah_vas_data = eah_vas_data,
                kbas_data = kbas_data,
                stq_data = stq_data))
  }

}

