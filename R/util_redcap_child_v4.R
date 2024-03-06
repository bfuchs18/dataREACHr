#' util_redcap_child_v4: Organize child visit 4 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_4_arm_1
#'
#'
#' @param data data from REDCap event child_visit_4_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_child_v4 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_4_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v4_post_check_notes', 'v4_date', 'wasi_notes', 'pit_task_notes')]
  names(visit_data_child)[names(visit_data_child) == "v4_post_check_notes"] <- "v4_notes"

  ## meal information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data
  meal_info <- data[, grep('participant_id|wanting|test_meal||advertisement_condition|test_meal_notes|meal_intake_notes|eah_notes|eah_intake_notes', names(data))]

  ## loc ####
  loc_data <-data[, grep("participant_id|^loc", names(data))]

  ## pptq ####
  pptq_data <-data[, grep("participant_id|^pptq", names(data))]
  # score?

  ## sic ####
  sic_data <-data[, grep("participant_id|^sic", names(data))]
  # score?

  ## wasi ####
  # get wasi scores through double-entry?
  wasi_data <-data[, grep("participant_id|^wasi", names(data))]
  wasi_data <- wasi_score[, !(names(wasi_score) %in% c('wasi_age_y', 'wasi_age_m','wasi_iq_scoring_form_complete'))]

  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_info = meal_info,
                loc_data = loc_data,
                pptq_data = pptq_data,
                sic_data = sic_data,
                wasi_data = wasi_data))
  }
}

