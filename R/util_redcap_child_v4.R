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

  # add session column
  data$session_id <- "ses-1"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v4_post_check_notes', 'v4_date', 'wasi_notes', 'pit_task_notes')]
  names(visit_data_child)[names(visit_data_child) == "v4_post_check_notes"] <- "v4_notes"

  ## Intake information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data

  ## meal data
  meal_data <- data[, grep("participant_id|session_id|test_meal|advertisement_condition|test_meal_notes|meal_intake_notes", names(data))]
  meal_data <- meal_data[, -grep("complete", names(meal_data))]
  names(meal_data) <- gsub('intake_notes', 'prep_notes', names(meal_data))

  ## EAH data
  eah_data <- data[, grep("participant_id|session_id|wanting|advertisement_condition|eah_notes|eah_intake_notes", names(data))]
  eah_data <- eah_data[, -grep("complete|timestamp", names(eah_data))]
  names(eah_data) <- gsub('intake_notes', 'prep_notes', names(eah_data))
  names(eah_data) <- gsub('eah_notes', 'eah_protocol_notes', names(eah_data))

  ## loc ####
  loc_data <-data[, grep("participant_id|session_id|^loc", names(data))]
  loc_data <- loc_data[, -grep("share_info_parent", names(loc_data))] # remove extra columns

  ## pptq ####
  pptq_data <-data[, grep("participant_id|session_id|^pptq|pictorial_personality_traits_questionnaire_timestamp", names(data))]
  pptq_data$pptq_form_date <- lubridate::as_date(pptq_data$pictorial_personality_traits_questionnaire_timestamp) # add form date column
  pptq_data <- pptq_data[, -grep("missingcheck|timestamp", names(pptq_data))] # remove extra columns
  pptq_data_for_scoring <- util_format_pptq_data(pptq_data)
  pptq_scored <- dataprepr::score_pptq(pptq_data_for_scoring, pptq_scale = 3, score_base = FALSE, id = "participant_id", extra_scale_cols = c("pptq_form_date"))

  ## sic ####
  sic_data <-data[, grep("participant_id|session_id|^sic|stress_in_children_questionnaire_timestamp", names(data))]
  sic_data$sic_form_date <- lubridate::as_date(sic_data$stress_in_children_questionnaire_timestamp) # add form date column
  sic_data <- sic_data[, -grep("missingcheck|timestamp", names(sic_data))] # remove extra columns
  # score?

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_data = meal_data,
                eah_data = eah_data,
                loc_data = loc_data,
                pptq_data = pptq_scored,
                sic_data = sic_data))
  }
}

