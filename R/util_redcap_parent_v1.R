#' util_redcap_parent_v1: Organize parent visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @param v1_date_data dataframe with columns: 'participant_id' and 'v1_date' -- can have additional columns as long as these are in there
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_parent_v1 <- function(data, v1_date_data, return_data = TRUE) {

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
    # (1) demo_data: data collected as part of the "Visit 1 Demographics" qualtrics form -- this will be the base for participants.tsv file
    # (2) household_data: data collected as part of the "Parent Household Demographics" qualtrics form

  demo_data_all <- data[, grepl('participant_id', names(data)) | grepl('demo', names(data))]

  demo_data <- data[c("participant_id", "demo_child_birthdate", "demo_birth_length", "demo_birthweight_pounds", "demo_birthweight_ounces",
                      "demo_ethnicity", "demo_race", "demo_premature", "demo_premature_weeks", "demo_feeding", "demo_exclusive_feeding", "demo_tot_breastfeeding", "demo_solid_food")]

  household_data <- demo_data_all[, !(names(demo_data_all) %in% c('demo_v1_missingcheck',
                                                                    'visit_1_demographics_complete',
                                                                    'parent_household_demographics_questionnaire_timestamp',
                                                                    'demo_missingcheck', 'demo_missingcheck_2', 'demo_missingcheck_3',
                                                                    'parent_household_demographics_questionnaire_complete',
                                                                  names(demo_data[2:ncol(demo_data)])))]

  names(household_data)[names(household_data) == "demo_self_report_feet"] <- "demo_parent2_reported_height_ft_component"
  names(household_data)[names(household_data) == "demo_self_report_inches"] <- "demo_parent2_reported_height_inch_component"
  names(household_data)[names(household_data) == "demo_self_report_weight"] <- "demo_parent2_reported_weight_lbs"

  # calculate parent age
  household_data[['visit_1_demographics_timestamp']] <- lubridate::as_date(household_data[['visit_1_demographics_timestamp']])
  household_data[['demo_parent_birthdate']] <- lubridate::as_date(household_data[['demo_parent_birthdate']])
  household_data[['demo_parent_age']] <- lubridate::interval(household_data[['demo_parent_birthdate']], household_data[['visit_1_demographics_timestamp']])/lubridate::years(1)

  # remove birthdate and timestamp variables
  household_data <- household_data[, !(names(household_data) %in% c('visit_1_demographics_timestamp','demo_parent_birthdate'))]

  # combine parent2 feet and inch components into 1 height variable in meters
  household_data$parent2_reported_height_m <- ((household_data$demo_parent2_reported_height_ft_component*12) + household_data$demo_parent2_reported_height_inch_component)*0.0254

  # calculate parent2 BMI (kg/m2)
  household_data$parent2_reported_bmi <- (household_data$demo_parent2_reported_weight_lbs*0.453592) / (household_data$parent2_reported_height_m**2)

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('participant_id', names(data)) | grepl('rank', names(data))]
  rank_data <- rank_data[, !(names(rank_data) %in% c('ranking_food_item_questionnaire_timestamp', 'ranking_food_item_questionnaire_complete'))]
  # score?

  ## Puberty Data ####
  puberty_data <-data[, grep("participant_id|^prs|tanner_", names(data))]
  puberty_data_for_scoring <- util_format_puberty_data(puberty_data)
  puberty_scored <- dataprepr::score_pds(puberty_data_for_scoring, score_base = FALSE, respondent = 'parent', male = "1", female = "0", id = 'participant_id')

  ## CFQ Data ####
  cfq_data <- data[, grepl('participant_id', names(data)) | grepl('cfq', names(data))]
  cfq_data <- cfq_data[, !(names(cfq_data) %in% c('cfq_missingcheck'))]
  cfq_scored <- dataprepr::score_cfq(cfq_data, score_base = TRUE, restriction_split = FALSE, id = 'participant_id')

  ## CEBQ Data ####
  cebq_data <- data[, grepl('participant_id', names(data)) | grepl('cebq', names(data))]
  cebq_scored <- dataprepr::score_cebq(cebq_data, score_base = TRUE, id = 'participant_id')

  ## EFCR Data ####
  efcr_data <- data[, grepl('participant_id', names(data)) | grepl('efcr', names(data))]
  efcr_scored <- dataprepr::score_efcr(efcr_data, score_base = TRUE, id = 'participant_id')

  ## CHAOS Data  ####
  chaos_data <- data[, grepl('participant_id', names(data)) | grepl('chaos', names(data))]
  chaos_data <- chaos_data[, !(names(chaos_data) %in% c('confusion_hubbub_and_order_scale_chaos_timestamp', 'confusion_hubbub_and_order_scale_chaos_complete'))]
  # need to develop score script

  ## PSS Data  (percieved stress scale) ####
  pss_data <- data[, grepl('participant_id', names(data)) | grepl('pss', names(data))]
  pss_scored <- dataprepr::score_pss(pss_data, score_base = TRUE, id = "participant_id")

  ## LBC Data  ####
  lbc_data <- data[, grepl('participant_id', names(data)) | grepl('lbc', names(data))]
  lbc_data <- lbc_data[, !(names(lbc_data) %in% c('lbc_missingcheck'))]
  names(lbc_data) <- gsub('_a', '_conf', names(lbc_data))
  lbc_scored <- dataprepr::score_lbc(lbc_data, score_base = TRUE, id = 'participant_id')

  ## return data ####
  if (isTRUE(return_data)){
    return(list(
      demo_data = demo_data,
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

