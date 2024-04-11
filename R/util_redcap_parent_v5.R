#' util_redcap_parent_v5: Organize parent visit 5 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_5_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_5_arm_1
#' @param v5_date_data dataframe with 2 columns: 'participant_id' and 'v5_date'
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_parent_v5 <- function(data, v5_date_data, return_data = TRUE) {

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

  ## demographics data ####
  # this data will be split into 2 dataframes:
  # (1) demo_data: will include child demographic variables that will be include in participants.tsv
  # (2) household_data: will include variables related to the family and household environemnt

  demo_data_all <- data[, grepl('participant_id', names(data)) | grepl('demo', names(data))]
  household_data <- demo_data_all[, !(names(demo_data_all) %in% c('visit_1_demographics_timestamp',
                                                                  'demo_v1_missingcheck',
                                                                  'visit_1_demographics_complete',
                                                                  'parent_household_demographics_questionnaire_timestamp',
                                                                  'demo_missingcheck', 'demo_missingcheck_2', 'demo_missingcheck_3',
                                                                  'parent_household_demographics_questionnaire_complete'))]

  names(household_data)[names(household_data) == "demo_self_report_feet"] <- "demo_parent2_reported_height_ft_component"
  names(household_data)[names(household_data) == "demo_self_report_inches"] <- "demo_parent2_reported_height_inch_component"
  names(household_data)[names(household_data) == "demo_self_report_weight"] <- "demo_parent2_reported_weight_lbs"

  # combine parent2 feet and inch components into 1 height variable in meters
  household_data$parent2_reported_height_m <- ((household_data$demo_parent2_reported_height_ft_component*12) + household_data$demo_parent2_reported_height_inch_component)*0.0254

  # calculate parent2 BMI (kg/m2)
  household_data$parent2_reported_bmi <- (household_data$demo_parent2_reported_weight_lbs*0.453592) / (household_data$parent2_reported_height_m**2)

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('participant_id', names(data)) | grepl('rank', names(data))]
  rank_data <- rank_data[, !(names(rank_data) %in% c('ranking_food_item_questionnaire_timestamp', 'ranking_food_item_questionnaire_complete'))]
  # score?

  ## Puberty Data ####
  puberty_data <-data[, grep("participant_id|session_id|^prs|tanner_|parental_rating_scale_for_pubertal_development_timestamp", names(data))]
  puberty_data$puberty_form_date <- lubridate::as_date(puberty_data$parental_rating_scale_for_pubertal_development_timestamp) # add form date column
  puberty_data <- puberty_data[, -grep("missingcheck|timestamp", names(puberty_data))] # remove extra columns
  puberty_data_for_scoring <- util_format_puberty_data(puberty_data)
  puberty_scored <- dataprepr::score_pds(puberty_data_for_scoring, score_base = FALSE, respondent = 'parent', male = "1", female = "0", id = 'participant_id')

  ## CEBQ Data ####
  cebq_data <- data[, grepl('participant_id|session_id|cebq|child_eating_behavior_questionnaire_timestamp', names(data))]
  cebq_data$cebq_form_date <- lubridate::as_date(cebq_data$child_eating_behavior_questionnaire_timestamp)
  cebq_data <- cebq_data[, -grep("missingcheck|timestamp", names(cebq_data))] # remove extra columns
  cebq_scored <- dataprepr::score_cebq(cebq_data, score_base = TRUE, id = 'participant_id')

  ## CBQ Data ####
  cbq_data <- data[, grepl('participant_id|session_id|cbq|childrens_behavior_questionnaire_timestamp', names(data))]
  cbq_data$cbq_form_date <- lubridate::as_date(cbq_data$childrens_behavior_questionnaire_timestamp) # add form date column
  cbq_data <- cbq_data[, -grep("missingcheck|timestamp", names(cbq_data))] # remove extra columns
  cbq_scored <- dataprepr::score_cbq(cbq_data, score_base = TRUE, id = 'participant_id')

  ## CSHQ Data ####
  cshq_data <- data[, grepl('participant_id|session_id|cshq|childs_sleep_habits_questionnaire_timestamp', names(data))]
  cshq_data$cshq_form_date <- lubridate::as_date(cshq_data$childs_sleep_habits_questionnaire_timestamp) # add form date column
  cshq_data <- cshq_data[, -grep("missingcheck|timestamp", names(cshq_data))] # remove extra columns
  names(cshq_data) <- gsub('_a', '_prob', names(cshq_data))

  # update values for scoring (3 - Usually, 2 - Sometimes, 1 - Rarely)
  # cshq_scored <- dataprepr::score_cshq(cshq_data, score_base = TRUE, reverse_score = FALSE, id = 'participant_id')


  ## CLASS Data ####
  class_data <- data[, grepl('participant_id|session_id|class|childrens_leisure_activities_study_survey_timestamp', names(data))]
  class_data$class_form_date <- lubridate::as_date(class_data$childrens_leisure_activities_study_survey_timestamp) # add form date column
  class_data <- class_data[, -grep("missingcheck|timestamp", names(class_data))] # remove extra columns
  # score? -- need to develop score script

  ## PTSCA Data ####
  ptsca_data <- data[, grepl('participant_id|session_id|ptsca|parental_strategies_to_teach_children_about_advert_timestamp', names(data))]
  ptsca_data$ptsca_form_date <- lubridate::as_date(ptsca_data$parental_strategies_to_teach_children_about_advert_timestamp) # add form date column
  ptsca_data <- ptsca_data[, -grep("missingcheck|timestamp", names(ptsca_data))] # remove extra columns
  # score -- need to develop score script

  ## PMUM Data ####
  pmum_data <- data[, grepl('participant_id|session_id|pmum|problematic_media_use_measure_timestamp', names(data))]
  pmum_data$pmum_form_date <- lubridate::as_date(pmum_data$problematic_media_use_measure_timestamp) # add form date column
  pmum_data <- pmum_data[, -grep("missingcheck|timestamp", names(pmum_data))] # remove extra columns
  # score -- need to develop score script

  ## AUDIT Data ####
  audit_data <- data[, grepl('participant_id|session_id|audit|alcohol_use_disorders_identification_test_timestamp', names(data))]
  audit_data$audit_form_date <- lubridate::as_date(audit_data$alcohol_use_disorders_identification_test_timestamp) # add form date column
  audit_data <- audit_data[, -grep("missingcheck|timestamp", names(audit_data))] # remove extra columns
  audit_scored <- dataprepr::score_audit(audit_data, id = 'participant_id')

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('participant_id|session_id|cfpq|comprehensive_feeding_practices_questionnaire_timestamp', names(data))]
  cfpq_data$cfpq_form_date <- lubridate::as_date(cfpq_data$comprehensive_feeding_practices_questionnaire_timestamp) # add form date column
  cfpq_data <- cfpq_data[, -grep("missingcheck|timestamp", names(cfpq_data))] # remove extra columns
  cfpq_scored <- dataprepr::score_cfpq(cfpq_data, score_base = TRUE, id = 'participant_id')

  ## return data ####
  if (isTRUE(return_data)){
    return(list(
      visit_data_parent = visit_data_parent,
      household_data = household_data,
      rank_data = rank_data,
      puberty_data = puberty_scored,
      cebq_data = cebq_scored,
      cbq_data = cbq_scored,
      cshq_data = cshq_data,
      # cshq_data = cshq_scored,
      class_data = class_data,
      ptsca_data = ptsca_data,
      pmum_data = pmum_data,
      #      pmum_data = pmum_scored, #score script to be developed
      audit_data = audit_scored,
      cfpq_data = cfpq_scored))
  }
}

