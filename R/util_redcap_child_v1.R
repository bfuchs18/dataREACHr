#' util_redcap_child_v1: Organize child visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_1_arm_1
#'
#'
#' @param data data from REDCap event child_visit_1_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 1 datasets
#'  2) meta-data/.json for each dataset
#'

util_redcap_child_v1 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_1_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v1_post_check_notes', 'v1_date', 'child_assent', 'dxa_notes', 'rrv_task_notes')]
  names(visit_data_child)[names(visit_data_child) == "v1_post_check_notes"] <- "v1_notes"

  # ## anthropometrics ####
  anthro_data <- data[c('participant_id', 'child_height_1_cm', 'child_height_2_cm', 'child_weight_1_kg', 'child_weight_2_kg', 'child_height_average', 'child_average_weight', 'child_bmi_v1', 'child_bmi_percentile', 'child_bmi_screenout',
                        'parent_height_sex', 'parent_height_1_cm', 'parent_height_2_cm', 'parent_weight_1_kg', 'parent_weight_2_kg', 'parent_height_average_cm', 'parent_weight_average_kg', 'parent_bmi_redcap_calc', 'parent_bmi_v1_self_report',
                        'parent_bmi_screenout', 'heightweight_notes')]

  # child bmi and percentile values were determined using the online form and entered into redcap -- recalculate?

  names(anthro_data)[names(anthro_data) == "child_bmi_v1"] <- "child_bmi"
  colnames(anthro_data)[11:19] <- c('parent1_sex', 'parent1_height_1_cm', 'parent1_height_2_cm', 'parent1_weight_1_kg', 'parent1_weight_2_kg', 'parent1_height_average_cm', 'parent1_weight_average_kg', 'parent1_bmi_measured', 'parent2_bmi_partner_report')


  ## meal information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data
  meal_info <- data[, grepl('vas', names(data)) |
                         grepl('test_meal', names(data)) |
                         names(data) %in% c('participant_id', 'advertisement_condition', 'meal_intake_notes')]

  ## kbas ####
  kbas_data <- data[, grepl('participant_id', names(data)) | grepl('toy_', names(data)) | grepl('food_', names(data)) | grepl("^q.*score", names(data))]
  # score this data?

  ## stq data ####
  stq_data <- data[, grepl('participant_id', names(data)) | grepl('stq', names(data))]

  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                anthro_data = anthro_data,
                meal_info = meal_info,
                 kbas_data = kbas_data,
                 stq_data = stq_data))
  }

}

