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

  # add session column
  data$session_id <- "ses-1"

  #reduce columns and update names

  ## Update form Data ####
  visit_data_parent <- data[, grep("participant_id|session_id|update", names(data))]
  visit_data_parent$update_form_date <- lubridate::as_date(visit_data_parent$participant_update_form_timestamp) # add form date column
  visit_data_parent <- visit_data_parent[, -grep("timestamp|participant_update_form_complete|contact|moving", names(visit_data_parent))]
  visit_data_parent <- visit_data_parent %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  ## HFSSM Data ####
  hfssm_data <- data[, grepl('participant_id|session_id|hfssm|household_food_security_survey_timestamp', names(data))]
  hfssm_data$hfssm_form_date <- lubridate::as_date(hfssm_data$household_food_security_survey_timestamp) # add form date column
  hfssm_data <- hfssm_data[, -grep("missingcheck|timestamp", names(hfssm_data))] # remove extra columns
  hfssm_data <- hfssm_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  hfssm_scored <- dataprepr::score_hfssm(hfssm_data, base_zero = TRUE, id = "participant_id")

  ## HFIAS Data ####
  # this refers to the household_food_insecurity_access_scale (HFIAS)
  hfias_data <- data[, grepl('participant_id|session_id|^hfi|household_food_insecurity_access_scale_timestamp', names(data))]
  hfias_data$hfias_form_date <- lubridate::as_date(hfias_data$household_food_insecurity_access_scale_timestamp) # add form date column
  hfias_data <- hfias_data[, -grep("missingcheck|timestamp", names(hfias_data))] # remove extra columns
  hfias_data <- hfias_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns
  names(hfias_data) <- gsub('hfi', 'hfias', names(hfias_data))

  hfias_scored <- dataprepr::score_hfias(hfias_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("hfias_form_date"))

  ## PMUM Data ####
  pmum_data <- data[, grepl('participant_id|session_id|pmum|problematic_media_use_measure_timestamp', names(data))]
  pmum_data$pmum_form_date <- lubridate::as_date(pmum_data$problematic_media_use_measure_timestamp) # add form date column
  pmum_data <- pmum_data[, -grep("missingcheck|timestamp", names(pmum_data))] # remove extra columns
  pmum_data <- pmum_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score -- need to develop score script

  ## CCHIP Data ####
  cchip_data <- data[, grepl('participant_id|session_id|cchip|community_childhood_hunger_id_project_timestamp', names(data))]
  cchip_data$cchip_form_date <- lubridate::as_date(cchip_data$community_childhood_hunger_id_project_timestamp) # add form date column
  cchip_data <- cchip_data[, -grep("missingcheck|timestamp", names(cchip_data))] # remove extra columns
  cchip_data <- cchip_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  cchip_scored <- dataprepr::score_cchip(cchip_data, id = 'participant_id')

  ## AUDIT Data ####
  audit_data <- data[, grepl('participant_id|session_id|audit|alcohol_use_disorders_identification_test_timestamp', names(data))]
  audit_data$audit_form_date <- lubridate::as_date(audit_data$alcohol_use_disorders_identification_test_timestamp) # add form date column
  audit_data <- audit_data[, -grep("missingcheck|timestamp", names(audit_data))] # remove extra columns
  audit_data <- audit_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  audit_scored <- dataprepr::score_audit(audit_data, id = 'participant_id')

  ## Fulkerson HFI Data ####
  # this refers to the fulkerson_home_food_inventory
  fhfi_data <- data[, grepl('participant_id|session_id|fhfi|fulkerson_home_food_inventory_timestamp', names(data))]
  fhfi_data$fhfi_form_date <- lubridate::as_date(fhfi_data$fulkerson_home_food_inventory_timestamp) # add form date column
  fhfi_data <- fhfi_data[, -grep("missingcheck|timestamp", names(fhfi_data))] # remove extra columns
  fhfi_data <- fhfi_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns


  # fhfi_data <- util_format_fhfi_data(fhfi_data) # need to deal with visible_27 and visible_28 before this function is ready

  # fhfi_scored <- dataprepr::score_hfi(hfi_data, base_zero = TRUE, id = 'participant_id') # need to make sure data is ready to go into this

  ## CFPQ Data ####
  cfpq_data <- data[, grepl('participant_id|session_id|cfpq|comprehensive_feeding_practices_questionnaire_timestamp', names(data))]
  cfpq_data$cfpq_form_date <- lubridate::as_date(cfpq_data$comprehensive_feeding_practices_questionnaire_timestamp) # add form date column
  cfpq_data <- cfpq_data[, -grep("missingcheck|timestamp", names(cfpq_data))] # remove extra columns
  cfpq_data <- cfpq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  cfpq_scored <- dataprepr::score_cfpq(cfpq_data, base_zero = TRUE, id = 'participant_id')

  ## return data ####

  if (isTRUE(return_data)){
    return(list(
      visit_data_parent = visit_data_parent,
      hfias_data = hfias_scored,
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

