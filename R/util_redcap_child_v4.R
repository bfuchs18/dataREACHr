#' util_redcap_child_v4: Organize child visit 4 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_4_arm_1
#'
#' @param data data from REDCap event child_visit_4_arm_1
#' @param return_data If return_data is set to TRUE, will return a list of dataframes including: visit_data_child, food_paradigm_info, eah_wanting, freddy_data, intake_data, loc_data, pptq_data, sic_data

util_redcap_child_v4 <- function(data, return_data = TRUE) {

  #### Set up/initial checks #####

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

  ## intake-related data ####

  # food paradigm information (does not include fullness, intake)
  food_paradigm_info <- data[, grepl('participant_id|session_id|meal|advertisement_condition', names(data))]
  food_paradigm_info <- food_paradigm_info[, !grepl('complete|freddy|consumed', names(food_paradigm_info))]
  names(food_paradigm_info) <- gsub('intake_notes', 'prep_notes', names(food_paradigm_info))

  # eah wanting
  eah_wanting <- data[, grep("participant_id|session_id|wanting", names(data))]
  eah_wanting <- eah_wanting[, -grep("complete|timestamp", names(eah_wanting))]

  ## freddy data -- this may or may not be replaced with double entry data
  freddy_data <- data[, grepl('participant_id|session_id|freddy', names(data))]
  freddy_data <- freddy_data[, -grep("complete|check|time|visit_number", names(freddy_data))]
  colnames(freddy_data) <- gsub("freddy", "fullness", colnames(freddy_data)) # Replace "freddy" with "fullness" in colnames

  ## intake_data -- this data can be used for prelim analyses, but eventually will be replaced with double entry data
  intake_data <- data[, grep("participant_id|session_id|plate", names(data))]

  ## loc ####
  loc_data <-data[, grep("participant_id|session_id|^loc", names(data))]
  loc_data <- loc_data[, -grep("share_info_parent", names(loc_data))] # remove extra columns
  loc_data <- loc_data %>% dplyr::relocate("session_id", .after = 1)

  ## pptq ####
  pptq_data <-data[, grep("participant_id|session_id|^pptq|pictorial_personality_traits_questionnaire_timestamp", names(data))]
  pptq_data$pptq_form_date <- lubridate::as_date(pptq_data$pictorial_personality_traits_questionnaire_timestamp) # add form date column
  pptq_data <- pptq_data[, -grep("missingcheck|timestamp", names(pptq_data))] # remove extra columns
  pptq_data_for_scoring <- util_format_pptq_data(pptq_data)
  pptq_scored <- dataprepr::score_pptq(pptq_data_for_scoring, pptq_scale = 3, base_zero = FALSE, id = "participant_id", extra_scale_cols = c("pptq_form_date"))

  ## sic ####
  sic_data <-data[, grep("participant_id|session_id|^sic|stress_in_children_questionnaire_timestamp", names(data))]
  sic_data$sic_form_date <- lubridate::as_date(sic_data$stress_in_children_questionnaire_timestamp) # add form date column
  sic_data <- sic_data[, -grep("missingcheck|timestamp", names(sic_data))] # remove extra columns
  sic_data <- sic_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("sic_form_date", .after = 2) # relocate columns

  # score?

  ## return data ####
  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                food_paradigm_info = food_paradigm_info,
                eah_wanting = eah_wanting,
                freddy_data = freddy_data,
                intake_data = intake_data,
                loc_data = loc_data,
                pptq_data = pptq_scored,
                sic_data = sic_data))
  }
}

