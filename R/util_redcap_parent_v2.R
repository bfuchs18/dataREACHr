#' util_redcap_parent_v2: Organize parent visit 2 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_2_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_2_arm_1e'
#' @param agesex_data dataframe with columns: 'participant_id', 'v2_age', 'sex' -- can have additional columns as long as these are in there; rerequired for scoring the BRIEF2
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw parent 1 datasets
#'  2) meta-data/.json for each dataset
#' @importFrom rlang .data
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

  # add session column
  data$session_id <- "ses-1"

  #reduce columns and update names

  ## Update form Data ####
  visit_data_parent <- data[, grep("participant_id|session_id|update", names(data))]
  visit_data_parent$update_form_date <- lubridate::as_date(visit_data_parent$participant_update_form_timestamp) # add form date column
  visit_data_parent <- visit_data_parent[, -grep("timestamp|participant_update_form_complete|contact|moving", names(visit_data_parent))]
  visit_data_parent <- visit_data_parent %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  ## CBQ Data ####
  cbq_data <- data[, grepl('participant_id|session_id|cbq|childrens_behavior_questionnaire_timestamp', names(data))]
  cbq_data$cbq_form_date <- lubridate::as_date(cbq_data$childrens_behavior_questionnaire_timestamp) # add form date column
  cbq_data <- cbq_data[, -grep("missingcheck|timestamp", names(cbq_data))] # remove extra columns
  cbq_data <- cbq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns
  cbq_scored <- dataprepr::score_cbq(cbq_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("cbq_form_date"))

  ## BRIEF Data ####
  brief_data <- data[, grepl('participant_id|session_id|brief|behavior_rating_inventory_of_executive_function_timestamp', names(data))]
  brief_data$brief_form_date <- lubridate::as_date(brief_data$behavior_rating_inventory_of_executive_function_timestamp) # add form date column
  brief_data <- brief_data[, -grep("missing_check|timestamp", names(brief_data))] # remove extra columns
  brief_data <- brief_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # add age and sex to brief_data
  brief_data <- merge(brief_data, agesex_data[c("participant_id", "brief_age", "sex")], by = "participant_id")
  brief_scored <- dataprepr::score_brief2(brief_data, age_var = "brief_age", sex_var = "sex", score_base = TRUE, male = "male", female = "female", id = "participant_id", extra_scale_cols = c("brief_age", "brief_form_date"))

  ## CSHQ Data ####
  cshq_data <- data[, grepl('participant_id|session_id|cshq|childs_sleep_habits_questionnaire_timestamp', names(data))]
  cshq_data$cshq_form_date <- lubridate::as_date(cshq_data$childs_sleep_habits_questionnaire_timestamp) # add form date column
  cshq_data <- cshq_data[, -grep("missingcheck|timestamp", names(cshq_data))] # remove extra columns
  cshq_data <- cshq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  names(cshq_data) <- gsub('_a', '_prob', names(cshq_data))

  # update values for scoring (3 - Usually, 2 - Sometimes, 1 - Rarely)

  # cshq_scored <- dataprepr::score_cshq(cshq_data, score_base = TRUE, reverse_score = FALSE, id = 'participant_id', extra_scale_cols = c("cshq_form_date"))


  ## BES Data ####
  bes_data <- data[, grepl('participant_id|session_id|bes|binge_eating_scale_timestamp', names(data))]
  bes_data$bes_form_date <- lubridate::as_date(bes_data$binge_eating_scale_timestamp) # add form date column
  bes_data <- bes_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns
  bes_data <- bes_data[, -grep("missingcheck|timestamp", names(bes_data))] # remove extra columns

  # change pna ('Don't want to answer') responses to 999
  bes_data_for_scoring <- bes_data %>%
    dplyr::mutate(
      # 4 indicates pna
      bes_1 = ifelse(.data$bes_1 == 4, 999, .data$bes_1),
      bes_2 = ifelse(.data$bes_2 == 4, 999, .data$bes_2),
      bes_3 = ifelse(.data$bes_3 == 4, 999, .data$bes_3),
      bes_4 = ifelse(.data$bes_4 == 4, 999, .data$bes_4),
      bes_5 = ifelse(.data$bes_5 == 4, 999, .data$bes_5),
      bes_7 = ifelse(.data$bes_7 == 4, 999, .data$bes_7),
      bes_8 = ifelse(.data$bes_8 == 4, 999, .data$bes_8),
      bes_9 = ifelse(.data$bes_9 == 4, 999, .data$bes_9),
      bes_10 = ifelse(.data$bes_10 == 4, 999, .data$bes_10),
      bes_11 = ifelse(.data$bes_11 == 4, 999, .data$bes_11),
      bes_12 = ifelse(.data$bes_12 == 4, 999, .data$bes_12),
      # 3 indicates pna
      bes_6 = ifelse(.data$bes_6 == 3, 999, .data$bes_6),
      bes_13 = ifelse(.data$bes_13 == 3, 999, .data$bes_13),
      bes_14 = ifelse(.data$bes_14 == 3, 999, .data$bes_14),
      bes_15 = ifelse(.data$bes_15 == 3, 999, .data$bes_15),
      bes_16 = ifelse(.data$bes_16 == 3, 999, .data$bes_16)
    )

  bes_scored <- dataprepr::score_bes(bes_data_for_scoring, score_base = TRUE, pna = 999, id = 'participant_id', extra_scale_cols = c("bes_form_date"))

  ## FFBS Data ####
  ffbs_data <- data[, grepl('participant_id|session_id|ffbs|family_food_behavior_survey_timestamp', names(data))]
  ffbs_data$ffbs_form_date <- lubridate::as_date(ffbs_data$family_food_behavior_survey_timestamp) # add form date column
  ffbs_data <- ffbs_data[, -grep("missingcheck|timestamp", names(ffbs_data))] # remove extra columns
  ffbs_data <- ffbs_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns


  ffbs_scored <- dataprepr::score_ffbs(ffbs_data, score_base = TRUE, id = 'participant_id', extra_scale_cols = c("ffbs_form_date"))

  ## FSQ Data (feeding strategies questionnaire) ####
  fsq_data <- data[, grepl('participant_id|session_id|fsq|feeding_strategies_questionnaire_timestamp', names(data))]
  fsq_data$fsq_form_date <- lubridate::as_date(fsq_data$feeding_strategies_questionnaire_timestamp) # add form date column
  fsq_data <- fsq_data[, -grep("missingcheck|timestamp", names(fsq_data))] # remove extra columns
  fsq_data <- fsq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  #score -- need to develop score script

  ## return data ####
  if (isTRUE(return_data)){
    return(list(
      visit_data_parent = visit_data_parent,
      cbq_data = cbq_scored,
      brief_data = brief_scored,
      cshq_data = cshq_data,
#      cshq_data = cshq_scored,
      bes_data = bes_scored,
      ffbs_data = ffbs_scored,
#     fsq_data = fsq_scored, #need to develop score script
      fsq_data = fsq_data
      ))
  }
}

