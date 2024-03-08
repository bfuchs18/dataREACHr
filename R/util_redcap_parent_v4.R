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
  visit_data_parent <- data[, grepl('participant_id', names(data)) | grepl('update', names(data))]
  visit_data_parent <- visit_data_parent[, !(names(visit_data_parent) %in% c('participant_update_form_timestamp', 'participant_update_form_complete', 'update_form_contact'))]
  # notes: update_form_visit_number -- looks to be the session number attended, not protocol visit number
  # add column to specify v4 data?


  ## HFSSM Data ####
  hfssm_data <- data[, grepl('participant_id', names(data)) | grepl('hfssm', names(data))]
  hfssm_scored <- dataprepr::score_hfssm(hfssm_data, score_base = TRUE, id = "participant_id")

  ## HFIAS Data ####
  # this refers to the household_food_insecurity_access_scale (HFIAS)
  #hfias_data <-
  hfias_data <- data[, grepl('participant_id', names(data)) | grepl('^hfi', names(data))]
  names(hfias_data) <- gsub('hfi', 'hfias', names(hfias_data))

  #hfias_scored <- dataprepr::score_hfi(hfi_data, score_base = TRUE, id = 'participant_id')

  ## PMUM Data ####
  pmum_data <- data[, grepl('participant_id', names(data)) | grepl('pmum', names(data))]
  # score -- need to develop score script

  ## CCHIP Data ####
  cchip_data <- data[, grepl('participant_id', names(data)) | grepl('cchip', names(data))]
  cchip_scored <- dataprepr::score_cchip(cchip_data, id = 'participant_id')

  ## AUDIT Data ####
  audit_data <- data[, grepl('participant_id', names(data)) | grepl('audit', names(data))]
  audit_data <- audit_data[, !(names(audit_data) %in% c('audit_missingcheck'))]
  audit_scored <- dataprepr::score_audit(audit_data, id = 'participant_id')

  ## Fulkerson HFI Data ####
  # this refers to the fulkerson_home_food_inventory
  fhfi_data <- data[, grepl('participant_id', names(data)) | grepl('fhfi', names(data))]
  names(fhfi_data) <- gsub('fhfi', 'hfi', names(fhfi_data))

  # fhfi_data <- util_format_fhfi_data(fhfi_data) # move everything into a separate function

  #fhfi_scored <- dataprepr::score_hfi(hfi_data, score_base = TRUE, id = 'participant_id') # need to make sure data is ready to go into this

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('participant_id', names(data)) | grepl('cfpq', names(data))]
  cfpq_scored <- dataprepr::score_cfpq(cfpq_data, score_base = TRUE, id = 'participant_id')

  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      visit_data_parent = visit_data_parent,
      # hfias_data = hfias_scored,
      hfssm_data = hfssm_scored,
      pmum_data = pmum_data,
      # pmum_data = pmum_scored,
      cchip_data = cchip_scored,
      audit_data = audit_scored,
      fhfi_data = fhfi_data,
      # fhfi_data = fhfi_scored,
      cfpq_data = cfpq_scored
    ))
  }
}

