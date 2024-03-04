#' util_redcap_parent_v4: Organize parent visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_4_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_4_arm_1'
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'
util_redcap_parent_v4 <- function(data, return_data = TRUE) {

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

  ## Visit Data ####
  v4_data <- data[, grepl('participant_id', names(data)) | grepl('update', names(data))]
  v4_data <- v4_data[, !(names(v4_data) %in% c('participant_update_form_timestamp', 'participant_update_form_complete', 'update_form_contact'))]
  # notes: update_form_visit_number -- looks to be the session number attended, not protocol visit number
  # add column to specify v4 data?


  ## HFSSM Data ####
  hfssm_data <- data[, grepl('participant_id', names(data)) | grepl('hfssm', names(data))]
  # score -- need to develop score script

  ## HFI Data ####
  hfi_data <- data[, grepl('participant_id', names(data)) | grepl('^hfi', names(data))]
  hfi_scored <- dataprepr::score_hfi(hfi_data, score_base = TRUE, id = 'participant_id')

  ## PMUM Data ####
  pmum_data <- data[, grepl('participant_id', names(data)) | grepl('pmum', names(data))]
  # score -- need to develop score script

  ## CCHIP Data ####
  cchip_data <- data[, grepl('participant_id', names(data)) | grepl('cchip', names(data))]
  # score -- need to develop score script

  ## AUDIT Data ####
  audit_data <- data[, grepl('participant_id', names(data)) | grepl('audit', names(data))]
  audit_data <- audit_data[, !(names(audit_data) %in% c('audit_missingcheck'))]
  # score -- need to develop score script

  ## FHFI Data ####
  fhfi_data <- data[, grepl('participant_id', names(data)) | grepl('fhfi', names(data))]
  # score -- need to develop score script

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('participant_id', names(data)) | grepl('cfpq', names(data))]
  # score -- need to develop score script

  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      v4_data = v4_data,
      hfi_data = hfi_scored,
      hfssm_data = hfssm_data,
      # hfssm_data = hfssm_scored,
      pmum_data = pmum_data,
      # pmum_data = pmum_scored,
      cchip_data = cchip_data,
      # cchip_data = pmum_scored,
      audit_data = audit_data,
      #      audit_data = audit_scored,
      fhfi_data = fhfi_data,
      # fhfi_data = fhfi_scored,
      # cfpq_data = cfpq_scored,
      cfpq_data = cfpq_data
    ))
  }
}

