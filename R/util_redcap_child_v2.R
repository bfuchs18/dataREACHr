#' util_redcap_child_v2: Organize child visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_2_arm_1
#'
#'
#' @param data data from REDCap event child_visit_2_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 1 datasets
#'  2) meta-data/.json for each dataset
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

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v2_post_check_notes', 'v2_date')]
  names(visit_data_child)[names(visit_data_child) == "v2_post_check_notes"] <- "v2_notes"

  ## MRI notes ####
  mri_notes <- data[, grepl('^mri_', names(data)) |
                      names(data) %in% c('participant_id', 'mock_fmri_complete_check', 'mock_fmri_notes')]
  mri_notes <- mri_notes[, !(names(mri_notes) %in% c('mri_resting_complete_check', 'mri_resting_state_notes'))]


  ## MRI behavioral assessment ####
  mri_assessment_data <- data[, grepl('participant_id', names(data)) | grepl('_familiarity', names(data)) | grepl('_recall', names(data)) | grepl("_liking", names(data))]
  # score this data?

  # CAMS and Freddy Fullness data will come from double-data entry forms


  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                mri_notes = mri_notes,
                mri_assessment_data = mri_assessment_data))
  }
}

