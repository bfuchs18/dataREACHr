#' util_redcap_parent_v1: Organize parent visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @param v1_date_data dataframe with 2 columns: 'participant_id' and 'v1_date'
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
    # (1) demo_data: will include child demographic variables that will be include in participants.tsv
    # (2) household_data: will include variables related to the family and household environemnt

  child_demo_vars <- c('demo_race', 'demo_ethnicity', 'demo_income', 'demo_education_mom', 'demo_child_birthdate')

  demo_data_all <- data[, grepl('participant_id', names(data)) | grepl('demo', names(data))]
  household_data <- demo_data_all[, !(names(demo_data_all) %in% c('visit_1_demographics_timestamp',
                                                                    'demo_v1_missingcheck',
                                                                    'visit_1_demographics_complete',
                                                                    'parent_household_demographics_questionnaire_timestamp',
                                                                    'demo_missingcheck', 'demo_missingcheck_2', 'demo_missingcheck_3',
                                                                    'parent_household_demographics_questionnaire_complete',
                                                                    child_demo_vars))]

  names(data)[names(data) == "demo_self_report_feet"] <- "demo_parent2_reported_height_ft_component"
  names(data)[names(data) == "demo_self_report_inches"] <- "demo_parent2_reported_height_inch_component"
  names(data)[names(data) == "demo_self_report_weight"] <- "demo_parent2_reported_weight_lbs"

  # subset demo_data
  demo_data <- demo_data_all[, c('participant_id', child_demo_vars)]

  # add age to demo_data
  demo_data <- merge(demo_data, v1_date_data, by = 'participant_id')
  demo_data[['v1_date']] <- lubridate::as_date(demo_data[['v1_date']])
  demo_data[['age']] <- lubridate::interval(demo_data[['demo_child_birthdate']], demo_data[['v1_date']])/lubridate::years(1)

  # remove dob and v1 date from demo_data
  demo_data <- demo_data[, !(names(demo_data) %in% c('demo_child_birthdate','v1_date'))]

  # add sex to demo_data
  demo_data <- merge(demo_data, data[, c("participant_id", "prs_sex")], by="participant_id")
  names(demo_data)[names(demo_data) == "prs_sex"] <- "sex"

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('participant_id', names(data)) | grepl('rank', names(data))]
  rank_data <- rank_data[, !(names(rank_data) %in% c('ranking_food_item_questionnaire_timestamp', 'ranking_food_item_questionnaire_complete'))]
  # score?

  ## Puberty Data ####
  puberty_data <-data[, grep("participant_id|^prs|tanner_", names(data))]
  names(puberty_data) <- gsub('prs_sex', 'sex', names(puberty_data))

  # scoring prep: update puberty_data to adhere to dataprepr::score_pds requirements:
    # The data set columns/variables must match the following naming convention: 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa'.
    # Values should be: 1 = not started yet, 2 = barely started, 3 = definitely started, 4 = seems complete, and 99 = I Don't Know. For Female ('pds_5fa') question on menarche, the response can be coded as Yes = 1, No = 0.

  # remap scale values except prs_girls_5
  value_mapping <- c("0" = 1, "1" = 2, "2" = 3, "3" = 4, "4" = 99)
  columns_to_remap <- c("prs_boys_1", "prs_boys_2", "prs_boys_3", "prs_boys_4", "prs_boys_5", "prs_boys_6", "prs_girls_1", "prs_girls_2", "prs_girls_3", "prs_girls_4", "prs_girls_6")
  puberty_data[, columns_to_remap] <- lapply(puberty_data[, columns_to_remap], function(x) value_mapping[as.character(x)])

  # remap prs_girls_5
  value_mapping_g5 <- c("4" = 99)
  puberty_data[, "prs_girls_5"] <- ifelse(puberty_data[, "prs_girls_5"] == 4, value_mapping_g5[as.character(4)], puberty_data[, "prs_girls_5"])

  # subset and rename girls variables
  puberty_data_girls <- subset(puberty_data, sex == 0)
  puberty_data_girls <- puberty_data_girls[, -grep("boys|tanner_male_choice", names(puberty_data_girls))]
  puberty_data_girls <- puberty_data_girls %>% dplyr::rename(
                                                pds_1 = prs_girls_1,
                                                pds_2 = prs_girls_2,
                                                pds_3 = prs_girls_3,
                                                pds_4f = prs_girls_4,
                                                pds_5fa = prs_girls_5,
                                                pds_6 = prs_girls_6,
                                                tanner_choice = tanner_female_choice
                                              )

  # subset and rename boys variables
  puberty_data_boys <- subset(puberty_data, sex == 1)
  puberty_data_boys <- puberty_data_boys[, -grep("girls|tanner_female_choice", names(puberty_data_boys))]
  puberty_data_boys <- puberty_data_boys %>% dplyr::rename(
                                                pds_1 = prs_boys_1,
                                                pds_2 = prs_boys_2,
                                                pds_3 = prs_boys_3,
                                                pds_4m = prs_boys_4,
                                                pds_5m = prs_boys_5,
                                                pds_6 = prs_boys_6,
                                                tanner_choice = tanner_male_choice
                                              )

  # bind girls and boys dfs -- dplyr::bind_rows fills missing values with NA where columns don't match.
  puberty_data_for_scoring <- dplyr::bind_rows(puberty_data_girls, puberty_data_boys)

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
  # need to develop score script
  #pss_scored <-

  ## LBC Data  ####
  lbc_data <- data[, grepl('participant_id', names(data)) | grepl('lbc', names(data))]
  lbc_data <- lbc_data[, !(names(lbc_data) %in% c('lbc_missingcheck'))]
  lbc_scored <- dataprepr::score_lbc(lbc_data, score_base = TRUE, id = 'participant_id')


  ## compile and return data ####
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
#      pss_data = pss_scored, # score script to be developed
      pss_data = pss_data,
      lbc_data = lbc_scored))
  }
}

