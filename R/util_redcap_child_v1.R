#' util_redcap_child_v1: Organize child visit 1 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_1_arm_1
#'
#' @param data data from REDCap event child_visit_1_arm_1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw child visit 1 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_child}
#'    \item{food_paradigm_info}
#'    \item{freddy_data}
#'    \item{intake_data}
#'    \item{liking_data}
#'    \item{kbas_data}
#'    \item{stq_data}
#'    \item{anthro_data}
#'  }
#' @examples
#'
#' # process REDCap data
#' child_visit1_list <- util_redcap_child_v1(data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]
#'
#' @export

util_redcap_child_v1 <- function(data) {

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop('data must be a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('child data for REDCap event child_visit_1_arm_1 must be entered as a data.frame')
  }

  # update name of participant ID column
  names(data)[names(data) == 'record_id'] <- 'participant_id'

  # add session column
  data['session_id'] <- 'ses-1'

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'session_id', 'v1_post_check_notes', 'v1_date', 'child_assent', 'dxa_notes', 'rrv_task_notes')]
  names(visit_data_child)[names(visit_data_child) == 'v1_post_check_notes'] <- 'v1_notes'
  visit_data_json <- json_v1_data()

  ## food paradigm information (does not include intake and freddy values) ####
  food_paradigm_info <- data[, grepl('participant_id|session_id|meal|advertisement_condition', names(data))]
  food_paradigm_info <- food_paradigm_info[, !grepl('complete|freddy|consumed', names(food_paradigm_info))]
  names(food_paradigm_info) <- gsub('intake_notes', 'prep_notes', names(food_paradigm_info))

  food_paradigm_info <- food_paradigm_info[c('participant_id', 'session_id', names(food_paradigm_info)[grepl('meal', names(food_paradigm_info))])]

  food_paradigm_json <- json_v1_food_paradigm()

  ## intake_data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data
  intake_data_notde <- data[, grep("participant_id|session_id|plate", names(data))]
  intake_data_notde <- intake_data_notde[c('participant_id', 'session_id', names(intake_data_notde)[grepl('plate', names(intake_data_notde))])]

  v1_intake_notde_json <- json_v1_intake_notde()

  ## freddy data (NO double entry data) ####
  freddy_data <- data[, grepl('participant_id|session_id|freddy', names(data))]
  freddy_data <- freddy_data[, -grep('complete|check|visit_number', names(freddy_data))]

  # Replace 'freddy' with 'fullness' in colnames
  names(freddy_data) <- gsub('freddy', 'fullness', names(freddy_data))
  freddy_data <- freddy_data[c('participant_id', 'session_id', names(freddy_data)[grepl('fullness', names(freddy_data))])]

  freddy_json <- json_v1_freddy()


  ## vas food liking (eah and meal foods) ####
  liking_data <- data[, grepl('participant_id|session_id|vas', names(data))]
  liking_data <- liking_data[, -grep('pre_vas_freddy', names(liking_data))]

  # Update names
  names(liking_data) <- gsub('vas', 'liking', names(liking_data))
  names(liking_data) <- gsub('cookie', 'oreo', names(liking_data))
  liking_data <- liking_data[c('participant_id', 'session_id', names(liking_data)[grepl('liking', names(liking_data))])]

  liking_json <- json_v1_liking()

  ## kbas data ####
  kbas_data <- data[, grep('participant_id|session_id|toy_|food_|^q.*score|kids_brand_awareness_survey_version_b_timestamp|kids_brand_awareness_survey_version_a_timestamp', names(data))]

  # process data
  kbas_data <- util_format_kbas_data(kbas_data)

  kbas_json <- json_kbas()

  ## stq data ####
  stq_data <-data[, grep('participant_id|session_id|stq', names(data))]
  stq_data <- stq_data[c('participant_id', 'session_id', names(stq_data)[grepl('stq', names(stq_data))])]

  #score?

  ## anthro data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data ####
  anthro_data <- data[, grep('participant_id|session|parent_height|child_height|parent_weight|child_weight|child_average_weight', names(data))]
  anthro_data <- anthro_data[, -grep('complete|self_report|check|notes', names(anthro_data))]

  anthro_data <- anthro_data[c('participant_id', 'session_id', names(anthro_data)[grepl('height|weight', names(anthro_data))])]

  # rename columns
  names(anthro_data) <- gsub('parent_', 'parent1_', names(anthro_data))
  names(anthro_data) <- gsub('parent1_height_sex', 'parent1_sex', names(anthro_data))
  names(anthro_data) <- gsub('child_average_weight', 'child_weight_average', names(anthro_data))

  # re-label parent1 sex
  anthro_data$parent1_sex <- ifelse(anthro_data$parent1_sex == 0, 'female', ifelse(anthro_data$parent1_sex == 1, 'male', NA))

  anthro_json <- json_v1_anthro()

  ## return data ####
    return(list(visit_data_child = list(data = visit_data_child, meta = visit_data_json),
                food_paradigm_info = list(food_paradigm_info, meta = food_paradigm_json),
                intake_data = list(intake_data_notde, meta = v1_intake_notde_json),
                freddy_data = list(freddy_data, meta = freddy_json),
                liking_data = list(liking_data, meta = liking_json),
                kbas_data = list(kbas_data, meta = kbas_json),
                stq_data = list(stq_data),
                anthro_data = list(anthro_data, meta = anthro_json)))

}

