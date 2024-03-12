#' util_redcap_parent_v5: Organize parent visit 5 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_5_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_5_arm_1
#' @param v1_date_data dataframe with 2 columns: 'participant_id' and 'v5_date'
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

  #reduce columns and update names

  ## demographics data ####
  # this data will be split into 2 dataframes:
  # (1) demo_data: will include child demographic variables that will be include in participants.tsv
  # (2) household_data: will include variables related to the family and household environemnt

  child_demo_vars <- c('demo_income', 'demo_education_mom')

  demo_data_all <- data[, grepl('participant_id', names(data)) | grepl('demo', names(data))]
  household_data <- demo_data_all[, !(names(demo_data_all) %in% c('visit_1_demographics_timestamp',
                                                                  'demo_v1_missingcheck',
                                                                  'visit_1_demographics_complete',
                                                                  'parent_household_demographics_questionnaire_timestamp',
                                                                  'demo_missingcheck', 'demo_missingcheck_2', 'demo_missingcheck_3',
                                                                  'parent_household_demographics_questionnaire_complete',
                                                                  child_demo_vars))]

  names(household_data)[names(household_data) == "demo_self_report_feet"] <- "demo_parent2_reported_height_ft_component"
  names(household_data)[names(household_data) == "demo_self_report_inches"] <- "demo_parent2_reported_height_inch_component"
  names(household_data)[names(household_data) == "demo_self_report_weight"] <- "demo_parent2_reported_weight_lbs"

  # combine parent2 feet and inch components into 1 height variable in meters
  household_data$parent2_reported_height_m <- ((household_data$demo_parent2_reported_height_ft_component*12) + household_data$demo_parent2_reported_height_inch_component)*0.0254

  # calculate parent2 BMI (kg/m2)
  household_data$parent2_reported_bmi <- (household_data$demo_parent2_reported_weight_lbs*0.453592) / (household_data$parent2_reported_height_m**2)

  # subset demo_data
  demo_data <- demo_data_all[, c('participant_id', child_demo_vars)]

  # # add age to demo_data ??
  demo_data <- merge(demo_data, v5_date_data, by = 'participant_id',  all=T)
  demo_data[['v5_date']] <- lubridate::as_date(demo_data[['v5_date']])
  demo_data[['age']] <- lubridate::interval(demo_data[['demo_child_birthdate']], demo_data[['v5_date']])/lubridate::years(1)

  # # remove dob and v1 date from demo_data
  demo_data <- demo_data[, !(names(demo_data) %in% c('demo_child_birthdate','v5_date'))]

  # add sex to demo_data
  demo_data <- merge(demo_data, data[, c("participant_id", "prs_sex")], by="participant_id")
  names(demo_data)[names(demo_data) == "prs_sex"] <- "sex"

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('participant_id', names(data)) | grepl('rank', names(data))]
  rank_data <- rank_data[, !(names(rank_data) %in% c('ranking_food_item_questionnaire_timestamp', 'ranking_food_item_questionnaire_complete'))]
  # score?

  ## Puberty Data ####
  puberty_data <-data[, grep("participant_id|^prs|tanner_", names(data))]
  puberty_data_for_scoring <-util_format_puberty_data(puberty_data)
  puberty_scored <- dataprepr::score_pds(puberty_data_for_scoring, score_base = FALSE, respondent = 'parent', male = "1", female = "0", id = 'participant_id')

  ## CEBQ Data ####
  cebq_data <- data[, grepl('participant_id', names(data)) | grepl('cebq', names(data))]
  cebq_scored <- dataprepr::score_cebq(cebq_data, score_base = TRUE, id = 'participant_id')

  ## CBQ Data ####
  cbq_data <- data[, grepl('participant_id', names(data)) | grepl('cbq', names(data))]
  cbq_data <- cbq_data[, !(names(cbq_data) %in% c('cbq_missingcheck'))]
  cbq_scored <- dataprepr::score_cbq(cbq_data, score_base = TRUE, id = 'participant_id')

  ## CHSQ Data ####
  cshq_data <- data[, grepl('participant_id', names(data)) | grepl('cshq', names(data))]
  cshq_data <- cshq_data[, !(names(cshq_data) %in% c('cshq_missingcheck'))]

  ## CLASS Data ####
  class_data <- data[, grepl('participant_id', names(data)) | grepl('class', names(data))]
  # score? -- need to develop score script

  ## PSTCA Data  ####
  ptsca_data <- data[, grepl('participant_id', names(data)) | grepl('ptsca', names(data))]
  ptsca_data <- ptsca_data[, !(names(ptsca_data) %in% c('ptsca_missingcheck'))]
  # score -- need to develop score script

  ## PMUM Data  ####
  pmum_data <- data[, grepl('participant_id', names(data)) | grepl('pmum', names(data))]
  # score -- need to develop score script

  ## AUDIT Data  ####
  audit_data <- data[, grepl('participant_id', names(data)) | grepl('audit', names(data))]
  audit_data <- audit_data[, !(names(audit_data) %in% c('audit_missingcheck'))]
  audit_scored <- dataprepr::score_audit(audit_data, id = 'participant_id')

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('participant_id', names(data)) | grepl('cfpq', names(data))]
  cfpq_scored <- dataprepr::score_cfpq(cfpq_data, score_base = TRUE, id = 'participant_id')


  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      demo_data = demo_data,
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

