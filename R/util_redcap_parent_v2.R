#' util_redcap_parent_v2: Organize parent visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_2_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_2_arm_1e'
#' @param agesex_data a dataframe with child age and sex -- required for scoring the BRIEF2
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#'
util_redcap_parent_v2 <- function(data, agesex_data, return_data = TRUE) {

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

  ## V2 Data ####
  visit_data_parent <- data[, grepl('participant_id', names(data)) | grepl('update', names(data))]
  visit_data_parent <- visit_data_parent[, !(names(visit_data_parent) %in% c('participant_update_form_timestamp', 'participant_update_form_complete', 'update_form_contact'))]
  # notes: update_form_visit_number -- looks to be the session number attended, not protocol visit number
  # add column to specify v2 data?


  ## CBQ Data ####
  cbq_data <- data[, grepl('participant_id', names(data)) | grepl('cbq', names(data))]
  cbq_data <- cbq_data[, !(names(cbq_data) %in% c('cbq_missingcheck'))]
  cbq_scored <- dataprepr::score_cbq(cbq_data, score_base = TRUE, id = 'participant_id')

  ## BRIEF Data ####
  brief_data <- data[, grepl('participant_id', names(data)) | grepl('brief', names(data))]
  brief_data <- brief_data[, !(names(brief_data) %in% c('brief_missing_check'))]
  # add age and sex to brief_data
  brief_data <- merge(brief_data, agesex_data, by = "participant_id")

  # brief_scored <- dataprepr::score_brief2(brief_data, age_var = "age", sex_var = "sex", score_base = TRUE, male = 1, female = 0, id = "participant_id") # need to debug


  ## CSHQ Data ####
  cshq_data <- data[, grepl('participant_id', names(data)) | grepl('cshq', names(data))]
  cshq_data <- cshq_data[, !(names(cshq_data) %in% c('cshq_missingcheck'))]

  # update values for scoring (3 - Usually, 2 - Sometimes, 1 - Rarely)
  cshq_data

  # cshq_scored <- dataprepr::score_cshq(cshq_data, score_base = TRUE, reverse_score = FALSE, id = 'participant_id')


  ## BES Data ####
  bes_data <- data[, grepl('participant_id', names(data)) | grepl('bes', names(data))]
  bes_data <- bes_data[, !(names(bes_data) %in% c('bes_missingcheck'))]
  bes_scored <- dataprepr::score_bes(bes_data, score_base = TRUE, pna = 4, id = 'participant_id')

  ## FFBS Data ####
  ffbs_data <- data[, grepl('participant_id', names(data)) | grepl('ffbs', names(data))]
  ffbs_scored <- dataprepr::score_ffbs(ffbs_data, score_base = TRUE, id = 'participant_id')

  ## FSQ Data (feeding strategies questionnaire) ####
  fsq_data <- data[, grepl('participant_id', names(data)) | grepl('fsq', names(data))]
  fsq_data <- fsq_data[, !(names(fsq_data) %in% c('fsq_missingcheck'))]
  #score -- need to develop score script

  ## compile and return data ####
  if (isTRUE(return_data)){
    return(list(
      visit_data_parent = visit_data_parent,
      cbq_data = cbq_scored,
      brief_data = brief_data,
#      brief_data = brief_scored,
      cshq_data = cshq_data,
#      cshq_data = cshq_scored,
      bes_data = bes_scored,
      ffbs_data = ffbs_scored,
#     fsq_data = fsq_scored, #need to develop score script
      fsq_data = fsq_data
      ))
  }
}

