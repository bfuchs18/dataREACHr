#' util_redcap_child_v2: Organize child visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_2_arm_1
#'
#' @param data data from REDCap event child_visit_2_arm_1
#' @param return_data If return_data is set to TRUE, returns a list of the following dataframes: visit_data_child, mri_notes, mri_assessment_data, mri_cams_ff
#'

util_redcap_child_v2 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_2_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  # add session column
  data$session_id <- "ses-1"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v2_post_check_notes', 'v2_date')]
  names(visit_data_child)[names(visit_data_child) == "v2_post_check_notes"] <- "v2_notes"

  ## MRI notes ####
  mri_notes <- data[, grepl('^mri_', names(data)) |
                      names(data) %in% c('participant_id', "session_id", 'v2_date', 'mock_fmri_complete_check', 'mock_fmri_notes')]
  mri_notes <- mri_notes[, !(names(mri_notes) %in% c('mri_resting_complete_check', 'mri_resting_state_notes'))]
  mri_notes <- mri_notes %>% dplyr::relocate("session_id", .after = 1) # relocate columns


  ## MRI behavioral assessment ####
  mri_assessment_data <- data[, grepl('participant_id|session_id|_familiarity|_recall|_liking|fmri_behavioral_assessment_timestamp', names(data))]
  mri_assessment_data$fmri_behavioral_assessment_timestamp <- ifelse(mri_assessment_data$fmri_behavioral_assessment_timestamp == "[not completed]", "", mri_assessment_data$fmri_behavioral_assessment_timestamp)
  mri_assessment_data$mri_assessment_form_date <- lubridate::as_date(mri_assessment_data$fmri_behavioral_assessment_timestamp) # add form date column
  mri_assessment_data <- mri_assessment_data[, -grep("missingcheck|timestamp", names(mri_assessment_data))] # remove extra columns
  mri_assessment_data <- mri_assessment_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score this data?

  # CAMS and Freddy Fullness data
  mri_cams_ff <- data[, grepl('participant_id|session_id|pre_cams_score|post_cams_score|freddy_score', names(data))]
  mri_cams_ff <- mri_cams_ff %>% dplyr::relocate("session_id", .after = 1) # relocate columns

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                mri_notes = mri_notes,
                mri_assessment_data = mri_assessment_data,
                mri_cams_ff = mri_cams_ff))
  }
}

