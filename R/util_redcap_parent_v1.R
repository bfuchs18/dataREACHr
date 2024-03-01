#' util_redcap_parent_v1: Organize parent visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_1_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_1_arm_1
#' @param v1_date_data dataset that includes ID variable and date of first visit
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
  household_data <- data[, grepl('participant_id', names(data)) | grepl('demo', names(data))]
  household_data <- household_data[, !(names(household_data) %in% c('visit_1_demographics_timestamp',
                                                                    'demo_v1_missingcheck',
                                                                    'visit_1_demographics_complete',
                                                                    'parent_household_demographics_questionnaire_timestamp',
                                                                    'demo_missingcheck', 'demo_missingcheck_2', 'demo_missingcheck_3',
                                                                    'parent_household_demographics_questionnaire_complete'))]
  # names(household_data)[??] <- 'demo_mom_ed'
  # determine demo_mom_ed
  # determine demo_dad_ed
  # compute age instead of birthday
  # remove items in demo_data from household_data

  # demo_data <- household_data[c('participant_id', 'demo_c_dob', 'demo_c_sex', 'demo_race', 'demo_ethnicity', 'demo_child_other_race', 'demo_income', 'demo_mod_ed', 'demo_dad_ed')]

  ## RANK Data (ranking food item questionnaire) ####
  rank_data <- data[, grepl('participant_id', names(data)) | grepl('rank', names(data))]
  rank_data <- rank_data[, !(names(rank_data) %in% c('ranking_food_item_questionnaire_timestamp', 'ranking_food_item_questionnaire_complete'))]
  # score

  ## Puberty Data ####
  puberty_data <- data[, grepl('participant_id', names(data)) | grepl('prs', names(data))]
  names(puberty_data) <- gsub('prs_sex', 'sex', names(puberty_data))

  #re-code sex to match demo_c_sex
  #  puberty_data[['sex']] <- ifelse(puberty_data[['sex']] == 0, 1, 0)

  #  puberty_scored <- dataprepr::score_pds(puberty_data, score_base = FALSE, respondent = 'parent', male = 0, female = 1, id = 'participant_id')
  #  puberty_json <- json_pds()

  ## CFQ Data ####
  cfq_data <- data[, grepl('participant_id', names(data)) | grepl('cfq', names(data))]
  cfq_data <- cfq_data[, !(names(cfq_data) %in% c('cfq_missingcheck'))]
  cfq_data$visit <- 1
  cfq_scored <- dataprepr::score_cfq(cfq_data, score_base = TRUE, restriction_split = FALSE, id = 'participant_id')
  #  cfq_json <- json_cfq() -- this will be needed for v1 and v5. run function in proc_redcap?

  ## CEBQ Data ####
  cebq_data <- data[, grepl('participant_id', names(data)) | grepl('cebq', names(data))]
  cebq_data$visit <- 1
  cebq_scored <- dataprepr::score_cebq(cebq_data, score_base = TRUE, id = 'participant_id')
  #  cebq_json <- json_cebq() -- this will be needed for v1 and v5. run function in proc_redcap?

  ## EFCR Data ####
  efcr_data <- data[, grepl('participant_id', names(data)) | grepl('efcr', names(data))]

  #  efcr_scored <- dataprepr::score_efcr(efcr_data, score_base = FALSE, id = 'participant_id')
  #  efcr_json <- json_efcr()

  ## CHAOS Data  ####
  chaos_data <- data[, grepl('participant_id', names(data)) | grepl('chaos', names(data))]
  chaos_data <- chaos_data[, !(names(chaos_data) %in% c('confusion_hubbub_and_order_scale_chaos_timestamp', 'confusion_hubbub_and_order_scale_chaos_complete'))]

  ## PSS Data  ####
  pss_data <- data[, grepl('participant_id', names(data)) | grepl('pss', names(data))]


  ## LBC Data  ####
  lbc_data <- data[, grepl('participant_id', names(data)) | grepl('lbc', names(data))]
  lbc_data <- lbc_data[, !(names(lbc_data) %in% c('lbc_missingcheck'))]

  # lbc_scored <- dataprepr::score_lbc(lbc_data, score_base = FALSE, id = 'participant_id')
  # lbc_json <- json_lbc()


  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      # for now, until scored and meta data exists
      #demo_data = list(data = demo_data),
      household_data = list(data = household_data),
      rank_data = list(data = rank_data),
      puberty_data = list(data = puberty_data),
      cfq_data = list(data = cfq_data),
      cebq_data = list(data = cebq_data),
      efcr_data = list(data = efcr_data),
      chaos_data = list(data = chaos_data),
      pss_data = list(data = pss_data),
      lbc_data = list(data = lbc_data)))
      # demo_data = list(data = demo_data),
      # household_data = list(data = household_data),
      # rank_data = list(data = rank_scored, meta = rank_json),
      # puberty_data = list(data = puberty_scored, meta = puberty_json),
      # cfq_data = list(data = cfq_scored, meta = cfq_json),
      # cebq_data = list(data = cebq_scored, meta = cebq_json),
      # efcr_data = list(data = efcr_scored, meta = efcr_json),
      # chaos_data = list(data = chaos_scored, meta = choas_json),
      # pss_data = list(data = pss_scored, meta = pss_json),
      # lbc_data = list(data = lbc_scored, meta = lbc_json)))
  }
}

