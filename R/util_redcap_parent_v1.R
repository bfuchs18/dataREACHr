#' util_redcap_parent_v1: Organize parent visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_parent_v1 <- function(data, return_data = TRUE) {

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
  data$session_id <- "ses-1"

  #reduce columns and update names

  ## demographics data ####
  # this data will be split into 3 dataframes:
    # (1) participants_data: data collected as part of the "Visit 1 Demographics" qualtrics form that will go into participants.tsv file
    # (2) infancy_data: data collected as part of the "Visit 1 Demographics" qualtrics form that will not go into participants.tsv file
    # (3) household_data: data collected as part of the "Parent Household Demographics" qualtrics form

  # select all demo variables
  demo_data_all <- data[, grepl('participant_id|session_id|demo', names(data))]

  # add date columns
  demo_data_all$demo_form_date <- lubridate::as_date(demo_data_all$visit_1_demographics_timestamp)
  demo_data_all$household_form_date <- lubridate::as_date(demo_data_all$parent_household_demographics_questionnaire_timestamp)

  # select columns for participants_data
  participants_data <- demo_data_all[c("participant_id", "demo_ethnicity", "demo_race")]

  # select columns for infancy_data
  infancy_data <- demo_data_all[c("participant_id","session_id", "demo_form_date", "demo_birth_length", "demo_birthweight_pounds", "demo_birthweight_ounces", "demo_premature", "demo_premature_weeks", "demo_feeding", "demo_exclusive_feeding", "demo_tot_breastfeeding", "demo_solid_food")]

  # derive total birthweight in ounces from lb and oz components
  infancy_data$birthweight_ounces_total <- (infancy_data$demo_birthweight_pounds)*16 + infancy_data$demo_birthweight_ounces

  # select columns for household_data
  household_data <- demo_data_all[, !(names(demo_data_all) %in% c('demo_v1_missingcheck',
                                                                    'visit_1_demographics_complete',
                                                                    'parent_household_demographics_questionnaire_timestamp', 'demo_child_birthdate',
                                                                    'demo_missingcheck', 'demo_missingcheck_2', 'demo_missingcheck_3',
                                                                    'parent_household_demographics_questionnaire_complete',
                                                                  names(participants_data[3:ncol(participants_data)]), names(infancy_data[3:ncol(infancy_data)])))]

  # relocate household_data columns
  household_data <- household_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("household_form_date", .after = 2) # relocate columns

  # rename columns
  names(household_data)[names(household_data) == "demo_self_report_feet"] <- "demo_parent2_reported_height_ft_component"
  names(household_data)[names(household_data) == "demo_self_report_inches"] <- "demo_parent2_reported_height_inch_component"
  names(household_data)[names(household_data) == "demo_self_report_weight"] <- "demo_parent2_reported_weight_lbs"

  # calculate parent age
  household_data[['visit_1_demographics_timestamp']] <- lubridate::as_date(household_data[['visit_1_demographics_timestamp']])
  household_data[['demo_parent_birthdate']] <- lubridate::as_date(household_data[['demo_parent_birthdate']])
  household_data[['demo_parent_age']] <- lubridate::interval(household_data[['demo_parent_birthdate']], household_data[['visit_1_demographics_timestamp']])/lubridate::years(1)

  # remove birthdate and timestamp variables
  household_data <- household_data[, -grep("birthdate|timestamp", names(household_data))]

  # combine parent2 feet and inch components into 1 height variable in meters
  household_data$parent2_reported_height_m <- ((household_data$demo_parent2_reported_height_ft_component*12) + household_data$demo_parent2_reported_height_inch_component)*0.0254

  # calculate parent2 BMI (kg/m2)
  household_data$parent2_reported_bmi <- (household_data$demo_parent2_reported_weight_lbs*0.453592) / (household_data$parent2_reported_height_m**2)

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('participant_id|session_id|rank', names(data))]
  rank_data$rank_form_date <- lubridate::as_date(rank_data$ranking_food_item_questionnaire_timestamp) # add form date column
  rank_data <- rank_data[, -grep("missingcheck|timestamp|complete", names(rank_data))] # remove extra columns
  rank_data <- rank_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("rank_form_date", .after = 2) # relocate columns

  # score?

  ## Puberty Data ####
  puberty_data <-data[, grep("participant_id|session_id|^prs|tanner_|parental_rating_scale_for_pubertal_development_timestamp", names(data))]
  puberty_data$puberty_form_date <- lubridate::as_date(puberty_data$parental_rating_scale_for_pubertal_development_timestamp) # add form date column
  puberty_data <- puberty_data[, -grep("missingcheck|timestamp", names(puberty_data))] # remove extra columns
  puberty_data <- puberty_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  puberty_data_for_scoring <- util_format_puberty_data(puberty_data)
  puberty_scored <- dataprepr::score_pds(puberty_data_for_scoring, score_base = FALSE, respondent = 'parent', male = "1", female = "0", id = 'participant_id')

  ## CFQ Data ####
  cfq_data <- data[, grepl('participant_id|session_id|cfq|child_feeding_questionnaire_timestamp', names(data))]
  cfq_data$cfq_form_date <- lubridate::as_date(cfq_data$child_feeding_questionnaire_timestamp)
  cfq_data <- cfq_data[, -grep("missingcheck|timestamp", names(cfq_data))] # remove extra columns
  cfq_data <- cfq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  cfq_scored <- dataprepr::score_cfq(cfq_data, score_base = TRUE, restriction_split = FALSE, id = 'participant_id', extra_scale_cols = c("cfq_form_date") )

  ## CEBQ Data ####
  cebq_data <- data[, grepl('participant_id|session_id|cebq|child_eating_behavior_questionnaire_timestamp', names(data))]
  cebq_data$cebq_form_date <- lubridate::as_date(cebq_data$child_eating_behavior_questionnaire_timestamp)
  cebq_data <- cebq_data[, -grep("missingcheck|timestamp", names(cebq_data))] # remove extra columns
  cebq_data <- cebq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  cebq_scored <- dataprepr::score_cebq(cebq_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("cebq_form_date"))

  ## EFCR Data ####
  efcr_data <- data[, grepl('participant_id|session_id|efcr|external_food_cues_responsiveness_timestamp', names(data))]
  efcr_data$efcr_form_date <- lubridate::as_date(efcr_data$external_food_cues_responsiveness_timestamp)
  efcr_data <- efcr_data[, -grep("missingcheck|timestamp", names(efcr_data))] # remove extra columns
  efcr_data <- efcr_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  efcr_scored <- dataprepr::score_efcr(efcr_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("efcr_form_date"))

  ## CHAOS Data  ####
  chaos_data <- data[, grepl('participant_id|session_id|chaos', names(data))]
  chaos_data$chaos_form_date <- lubridate::as_date(chaos_data$confusion_hubbub_and_order_scale_chaos_timestamp)
  chaos_data <- chaos_data[, -grep("missingcheck|timestamp|complete", names(chaos_data))] # remove extra columns
  chaos_data <- chaos_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # need to develop score script

  ## PSS Data  (percieved stress scale) ####
  pss_data <- data[, grepl('participant_id|session_id|pss|perceived_stress_scale_timestamp', names(data))]
  pss_data$pss_form_date <- lubridate::as_date(pss_data$perceived_stress_scale_timestamp)
  pss_data <- pss_data[, -grep("missingcheck|timestamp", names(pss_data))] # remove extra columns
  pss_data <- pss_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  pss_scored <- dataprepr::score_pss(pss_data, score_base = TRUE, id = "participant_id", extra_scale_cols = c("pss_form_date"))

  ## LBC Data  ####
  lbc_data <- data[, grepl('participant_id|session_id|lbc|lifestyle_behavior_checklist_timestamp', names(data))]
  lbc_data$lbc_form_date <- lubridate::as_date(lbc_data$lifestyle_behavior_checklist_timestamp)
  lbc_data <- lbc_data[, -grep("missingcheck|timestamp", names(lbc_data))] # remove extra columns
  lbc_data <- lbc_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  names(lbc_data) <- gsub('_a', '_conf', names(lbc_data))
  # lbc_scored <- dataprepr::score_lbc(lbc_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("lbc_form_date")) # need to debug

  ## return data ####
  if (isTRUE(return_data)){
    return(list(
      participants_data = participants_data,
      infancy_data = infancy_data,
      household_data = household_data,
      rank_data = rank_data,
      puberty_data = puberty_scored,
      cfq_data = cfq_scored,
      cebq_data = cebq_scored,
      efcr_data = efcr_scored,
#      chaos_data = chaos_scored, #score script to be developed
      chaos_data = chaos_data,
      pss_data = pss_scored,
      lbc_data = lbc_scored))
  }
}

