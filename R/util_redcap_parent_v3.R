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

  #reduce columns and update names

  ## V3 Data ####
  v3_data <- data[, grepl('participant_id', names(data)) | grepl('update', names(data))]
  v3_data <- v3_data[, !(names(v3_data) %in% c('participant_update_form_timestamp', 'participant_update_form_complete', 'update_form_contact'))]
  # notes: update_form_visit_number -- looks to be the session number attended, not protocol visit number
  # add column to specify v3 data?


  ## SPSRQ Data ####
  spsrq_data <- data[, grepl('participant_id', names(data)) | grepl('spsrq', names(data))]
  spsrq_scored <- dataprepr::score_spsrq(spsrq_data, score_base = TRUE, id = 'participant_id')

  ## PWLB Data ####
  pwlb_data <- data[, grepl('participant_id', names(data)) | grepl('pwlb', names(data))]
  pwlb_data <- pwlb_data[, !(names(pwlb_data) %in% c('pwlb_missingcheck'))]
  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, score_base = TRUE, id = 'participant_id')

  ## TFEQ Data ####
  #note: REACH used tfeq-r18 (revised scale)
  tfeq_data <- data[, grepl('participant_id', names(data)) | grepl('tfeq', names(data))]
  tfeq_scored <- dataprepr::score_tfeq18(tfeq_data, score_base = TRUE, id = 'participant_id')

  ## CLASS Data ####
  class_data <- data[, grepl('participant_id', names(data)) | grepl('class', names(data))]
  # score? -- need to develop score script

  ## BISBAS Data ####
  bisbas_data <- data[, grepl('participant_id', names(data)) | grepl('bisbas', names(data))]
  bisbas_scored <- dataprepr::score_bisbas(bisbas_data, score_base = TRUE, id = 'participant_id')

  ## PTSCA Data ####
  ptsca_data <- data[, grepl('participant_id', names(data)) | grepl('ptsca', names(data))]
  ptsca_data <- ptsca_data[, !(names(ptsca_data) %in% c('ptsca_missingcheck'))]
  # score -- need to develop score script

  ## DEBQ Data ####
  debq_data <- data[, grepl('participant_id', names(data)) | grepl('debq', names(data))]
  debq_data$debq_20 <- NA # debq question 20 was not administered in REACH -- debq question 23 was repeated in its place
  debq_scored <- dataprepr::score_debq(debq_data, score_base = TRUE, id = 'participant_id')

  ## SCPF Data ####
  scpf_data <- data[, grepl('participant_id', names(data)) | grepl('scpf', names(data))]
  # score -- need to develop score script

  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      v3_data = v3_data,
      spsrq_data = spsrq_scored,
      pwlb_data = pwlb_scored,
      tfeq_data = tfeq_scored,
      class_data = class_data,
      bisbas_data = bisbas_scored,
      ptsca_data = ptsca_data,
      #      ptsca_data = ptsca_scored,
      debq_data = debq_scored,
      #     scpf_data = scpf_scored, #need to develop score script
      scpf_data = scpf_data
    ))
  }
}

