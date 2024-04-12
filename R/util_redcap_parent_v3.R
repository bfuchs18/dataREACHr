#' util_redcap_parent_v3: Organize parent visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_3_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_3_arm_1'
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'
util_redcap_parent_v3 <- function(data, return_data = TRUE) {

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

  ## Update form Data ####
  visit_data_parent <- data[, grep("participant_id|session_id|update", names(data))]
  visit_data_parent$update_form_date <- lubridate::as_date(visit_data_parent$participant_update_form_timestamp) # add form date column
  visit_data_parent <- visit_data_parent[, -grep("timestamp|participant_update_form_complete|contact|moving", names(visit_data_parent))]
  visit_data_parent <- visit_data_parent %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  ## SPSRQ Data ####
  spsrq_data <- data[, grepl('participant_id|session_id|spsrq|sensitivity_to_punishment_and_reward_questio_833adf_timestamp', names(data))]
  spsrq_data$spsrq_form_date <- lubridate::as_date(spsrq_data$sensitivity_to_punishment_and_reward_questio_833adf_timestamp) # add form date column
  spsrq_data <- spsrq_data[, -grep("missingcheck|timestamp", names(spsrq_data))] # remove extra columns
  spsrq_data <- spsrq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  spsrq_scored <- dataprepr::score_spsrq(spsrq_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("spsrq_form_date"))

  ## PWLB Data ####
  pwlb_data <- data[, grepl('participant_id|session_id|pwlb|parent_weight_loss_behavior_questionnaire_timestamp', names(data))]
  pwlb_data$pwlb_form_date <- lubridate::as_date(pwlb_data$parent_weight_loss_behavior_questionnaire_timestamp) # add form date column
  pwlb_data <- pwlb_data[, -grep("missingcheck|timestamp", names(pwlb_data))] # remove extra columns
  pwlb_data <- pwlb_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("pwlb_form_date", "pwlb_24a"))

  ## TFEQ Data ####
  #note: REACH used tfeq-r18 (revised scale)
  tfeq_data <- data[, grepl('participant_id|session_id|tfeq|three_factor_eating_questionnaire_revised_timestamp', names(data))]
  tfeq_data$tfeq_form_date <- lubridate::as_date(tfeq_data$three_factor_eating_questionnaire_revised_timestamp) # add form date column
  tfeq_data <- tfeq_data[, -grep("missingcheck|timestamp", names(tfeq_data))] # remove extra columns
  tfeq_data <- tfeq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  tfeq_scored <- dataprepr::score_tfeq18(tfeq_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("tfeq_form_date"))

  ## CLASS Data ####
  class_data <- data[, grepl('participant_id|session_id|class|childrens_leisure_activities_study_survey_timestamp', names(data))]
  class_data$class_form_date <- lubridate::as_date(class_data$childrens_leisure_activities_study_survey_timestamp) # add form date column
  class_data <- class_data[, -grep("missingcheck|timestamp", names(class_data))] # remove extra columns
  class_data <- class_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score? -- need to develop score script

  ## BISBAS Data ####
  bisbas_data <- data[, grepl('participant_id|session_id|bisbas|behavioral_approachinhibition_scale_questionnaire_timestamp', names(data))]
  bisbas_data$bisbas_form_date <- lubridate::as_date(bisbas_data$behavioral_approachinhibition_scale_questionnaire_timestamp) # add form date column
  bisbas_data <- bisbas_data[, -grep("missingcheck|timestamp", names(bisbas_data))] # remove extra columns
  bisbas_data <- bisbas_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  bisbas_scored <- dataprepr::score_bisbas(bisbas_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("bisbas_form_date"))

  ## PTSCA Data ####
  ptsca_data <- data[, grepl('participant_id|session_id|ptsca|parental_strategies_to_teach_children_about_advert_timestamp', names(data))]
  ptsca_data$ptsca_form_date <- lubridate::as_date(ptsca_data$parental_strategies_to_teach_children_about_advert_timestamp) # add form date column
  ptsca_data <- ptsca_data[, -grep("missingcheck|timestamp", names(ptsca_data))] # remove extra columns
  ptsca_data <- ptsca_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score -- need to develop score script

  ## DEBQ Data ####
  debq_data <- data[, grepl('participant_id|session_id|debq|dutch_eating_behavior_questionnaire_timestamp', names(data))]
  debq_data$debq_form_date <- lubridate::as_date(debq_data$dutch_eating_behavior_questionnaire_timestamp) # add form date column
  debq_data <- debq_data[, -grep("missingcheck|timestamp", names(debq_data))] # remove extra columns
  debq_data <- debq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  debq_data$debq_20 <- NA # mark item 20 data as missing. debq question 20 was not administered in REACH -- debq question 23 was repeated in its place
  debq_scored <- dataprepr::score_debq(debq_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("debq_form_date"))

  ## SCPF Data ####
  scpf_data <- data[, grepl('participant_id|session_id|scpf|structure_and_control_in_parent_feeding_timestamp', names(data))]
  scpf_data$scpf_form_date <- lubridate::as_date(scpf_data$structure_and_control_in_parent_feeding_timestamp) # add form date column
  scpf_data <- scpf_data[, -grep("missingcheck|timestamp", names(scpf_data))] # remove extra columns
  scpf_data <- scpf_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  scpf_data$scpf_17 <- NA # mark item 17 data as missing -- the wrong question was administered here.
  scpf_scored <- dataprepr::score_scpf(scpf_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("scpf_form_date"))

  ## return data ####
  if (isTRUE(return_data)){
    return(list(
      visit_data_parent = visit_data_parent,
      spsrq_data = spsrq_scored,
      pwlb_data = pwlb_scored,
      tfeq_data = tfeq_scored,
      class_data = class_data,
      bisbas_data = bisbas_scored,
      ptsca_data = ptsca_data,
      #      ptsca_data = ptsca_scored,
      debq_data = debq_scored,
      scpf_data = scpf_scored
    ))
  }
}

