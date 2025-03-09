#' util_redcap_parent_v5: Organize parent visit 5 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_5_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_5_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_parent_v5 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  # add session column
  data$session_id <- "ses-2"

  #reduce columns and update names

  ## Update form Data ####
  visit_data_parent <- data[, grep("participant_id|session_id|update", names(data))]
  visit_data_parent$update_form_date <- lubridate::as_date(visit_data_parent$participant_update_form_timestamp) # add form date column
  visit_data_parent <- visit_data_parent[, -grep("timestamp|participant_update_form_complete|contact|moving", names(visit_data_parent))]
  visit_data_parent <- visit_data_parent %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  ## househole demographics data ####
  household_data <- data[, grepl('participant_id|session_id|demo', names(data))]
  household_data$household_form_date <- lubridate::as_date(household_data$parent_household_demographics_questionnaire_timestamp)  # add form date column
  household_data <- household_data[, -grep("missingcheck|timestamp|complete", names(household_data))] # remove extra columns

  # process household data
  household_data <- util_format_household_data(household_data)

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('participant_id|session_id|rank', names(data))]
  rank_data$rank_form_date <- lubridate::as_date(rank_data$ranking_food_item_questionnaire_timestamp) # add form date column
  rank_data <- rank_data[, -grep("missingcheck|timestamp|complete", names(rank_data))] # remove extra columns
  rank_data <- rank_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score?

  ## Puberty Data ####
  puberty_data <-data[, grep("participant_id|session_id|^prs|tanner_|parental_rating_scale_for_pubertal_development_timestamp", names(data))]
  puberty_data$puberty_form_date <- lubridate::as_date(puberty_data$parental_rating_scale_for_pubertal_development_timestamp) # add form date column
  puberty_data <- puberty_data[, -grep("missingcheck|timestamp", names(puberty_data))] # remove extra columns
  puberty_data <- puberty_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  puberty_data_for_scoring <- util_format_puberty_data(puberty_data, respondent = "parent")
  puberty_scored <- dataprepr::score_pds(puberty_data_for_scoring, base_zero = FALSE, respondent = 'parent', male = "male", female = "female", id = 'participant_id')

  ## CEBQ Data ####
  cebq_data <- data[, grepl('participant_id|session_id|cebq|child_eating_behavior_questionnaire_timestamp', names(data))]
  cebq_data$cebq_form_date <- lubridate::as_date(cebq_data$child_eating_behavior_questionnaire_timestamp)
  cebq_data <- cebq_data[, -grep("missingcheck|timestamp", names(cebq_data))] # remove extra columns
  cebq_data <- cebq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  cebq_scored <- dataprepr::score_cebq(cebq_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("cebq_form_date"))

  ## CBQ Data ####
  cbq_data <- data[, grepl('participant_id|session_id|cbq|childrens_behavior_questionnaire_timestamp', names(data))]
  cbq_data$cbq_form_date <- lubridate::as_date(cbq_data$childrens_behavior_questionnaire_timestamp) # add form date column
  cbq_data <- cbq_data[, -grep("missingcheck|timestamp", names(cbq_data))] # remove extra columns
  cbq_data <- cbq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns
  cbq_scored <- dataprepr::score_cbq(cbq_data, base_zero = TRUE, id = 'participant_id', does_not_apply_value = 7, extra_scale_cols = c("cbq_form_date"))

  ## CSHQ Data ####
  cshq_data <- data[, grepl('participant_id|session_id|cshq|childs_sleep_habits_questionnaire_timestamp', names(data))]
  cshq_data$cshq_form_date <- lubridate::as_date(cshq_data$childs_sleep_habits_questionnaire_timestamp) # add form date column
  cshq_data <- cshq_data[, -grep("missingcheck|timestamp", names(cshq_data))] # remove extra columns
  cshq_data <- cshq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  cshq_data <- util_format_cshq_data(cshq_data)
  cshq_scored <- dataprepr::score_cshq(cshq_data, base_zero = FALSE, reverse_score = FALSE, id = 'participant_id')


  ## CLASS Data ####
  class_data <- data[, grepl('participant_id|session_id|class|childrens_leisure_activities_study_survey_timestamp', names(data))]
  class_data$class_form_date <- lubridate::as_date(class_data$childrens_leisure_activities_study_survey_timestamp) # add form date column
  class_data <- class_data[, -grep("missingcheck|timestamp", names(class_data))] # remove extra columns
  class_data <- class_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score? -- need to develop score script

  ## PTSCA Data ####
  ptsca_data <- data[, grepl('participant_id|session_id|ptsca|parental_strategies_to_teach_children_about_advert_timestamp', names(data))]
  ptsca_data$ptsca_form_date <- lubridate::as_date(ptsca_data$parental_strategies_to_teach_children_about_advert_timestamp) # add form date column
  ptsca_data <- ptsca_data[, -grep("missingcheck|timestamp", names(ptsca_data))] # remove extra columns
  ptsca_data <- ptsca_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score -- need to develop score script

  ## PMUM Data ####
  pmum_data <- data[, grepl('participant_id|session_id|pmum|problematic_media_use_measure_timestamp', names(data))]
  pmum_data$pmum_form_date <- lubridate::as_date(pmum_data$problematic_media_use_measure_timestamp) # add form date column
  pmum_data <- pmum_data[, -grep("missingcheck|timestamp", names(pmum_data))] # remove extra columns
  pmum_data <- pmum_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score -- need to develop score script

  ## AUDIT Data ####
  audit_data <- data[, grepl('participant_id|session_id|audit|alcohol_use_disorders_identification_test_timestamp', names(data))]
  audit_data$audit_form_date <- lubridate::as_date(audit_data$alcohol_use_disorders_identification_test_timestamp) # add form date column
  audit_data <- audit_data[, -grep("missingcheck|timestamp", names(audit_data))] # remove extra columns
  audit_data <- audit_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  audit_scored <- dataprepr::score_audit(audit_data, id = 'participant_id', extra_scale_cols = c("audit_form_date"), base_zero = TRUE)

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('participant_id|session_id|cfpq|comprehensive_feeding_practices_questionnaire_timestamp', names(data))]
  cfpq_data$cfpq_form_date <- lubridate::as_date(cfpq_data$comprehensive_feeding_practices_questionnaire_timestamp) # add form date column
  cfpq_data <- cfpq_data[, -grep("missingcheck|timestamp", names(cfpq_data))] # remove extra columns
  cfpq_data <- cfpq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  cfpq_scored <- dataprepr::score_cfpq(cfpq_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("cfpq_form_date"))

  ## return data ####
  if (isTRUE(return_data)){
    return(list(
      visit_data_parent = visit_data_parent,
      household_data = household_data,
      rank_data = rank_data,
      puberty_data = puberty_scored,
      cebq_data = cebq_scored,
      cbq_data = cbq_scored,
      cshq_data = cshq_scored,
      class_data = class_data,
      ptsca_data = ptsca_data,
      pmum_data = pmum_data,
      #      pmum_data = pmum_scored, #score script to be developed
      audit_data = audit_scored,
      cfpq_data = cfpq_scored))
  }
}

