#' util_redcap_de: Organize double-entry data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap double entry data data
#'
#'
#' @param data double-entry data
#' @param return_data If return_data is set to TRUE, will return a list including: mri_visit_data, tictoc_data, wasi_data, dexa_data, anthro_data (wide and long), intake_data (wide and long)
#'

util_redcap_de <- function(data, agesex_data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("REDCap double entry data must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  # all validated so can just take reviewer 1 data
  data <- data[grepl('--1', data[['participant_id']]), ]
  data$participant_id <- gsub("--1", "", data$participant_id)


  #reduce columns and update names

  ## MRI visit data: CAMS and Freddy Fullness ####
  mri_visit_data <- data[, grep("participant_id|cams_score|freddy_score_v2", names(data))]
  names(mri_visit_data)[names(mri_visit_data) == "pre_cams_score"] <- "cams_pre_mri"
  names(mri_visit_data)[names(mri_visit_data) == "post_cams_score"] <- "cams_post_mri"
  names(mri_visit_data)[names(mri_visit_data) == "pre_snack_freddy_score_v2"] <- "freddy_pre_snack"
  names(mri_visit_data)[names(mri_visit_data) == "post_snack_freddy_score_v2"] <- "freddy_post_snack"
  names(mri_visit_data)[names(mri_visit_data) == "post_snack_2_freddy_score_v2"] <- "freddy_post_snack2"
  names(mri_visit_data)[names(mri_visit_data) == "pre_mri_freddy_score_v2"] <- "freddy_pre_mri"

  ## TICTOC ####
  tictoc_data <- data[, grep("participant_id|tictoc_", names(data))]
  tictoc_data <- tictoc_data[, !(names(tictoc_data) %in% c('tictoc_task_input_sheet_complete'))]

  ## WASI data ####
  wasi_data <- data[, grep("participant_id|wasi.*score$|wasi_percentile", names(data))]

  ## Anthropometrics data ####
  anthro_data <- data[, grep("participant_id|parent_height|child_height|parent_weight|child_weight|child_average_weight|bmi", names(data))]
  anthro_data <- anthro_data[, -grep("complete|self_report", names(anthro_data))] #self-report will be merged from household questionnaire -- this is automatically taken from there (not entered data)

  # rename parent variables with parent1
  colnames(anthro_data) <- gsub("parent_", "parent1_", colnames(anthro_data))

  # remove child bmi values that were entered into redcao
  anthro_data <- anthro_data[, -grep("child_bmi", names(anthro_data))]

  # make all vales numeric except column 1 (participant_id)
  anthro_data <- dplyr::mutate_at(anthro_data, -1, function(x) as.numeric(as.character(x)))

  # add sex and age to calculate BMI derivatives
  anthro_data <- merge(anthro_data, agesex_data[c("participant_id", "sex", "v1_age", "v5_age")], by = "participant_id", all = TRUE)

  # calculate child bmi values -- need to debug once there is data entered

  # ## visit 1 BMI
  # anthro_data$child_bmi_v1 <-
  #   ifelse(is.na(anthro_data[["child_height_average_v1"]]) | is.na(anthro_data[["child_average_weight_v1"]]), NA,
  #          round(anthro_data[["child_average_weight_v1"]] / ((
  #            anthro_data[["child_height_average_v1"]] / 100
  #          ) ^ 2),
  #          digits = 2))
  #
  # anthro_data$child_bmi_z_v1 <- round(childsds::sds(value = anthro_data[["child_bmi_v1"]], age = anthro_data[["v1_age"]], sex = anthro_data[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = 1, female = 0), digits = 2)
  # anthro_data$child_bmi_p_v1 <- round((childsds::sds(value = anthro_data[["child_bmi_v1"]], age = anthro_data[["v1_age"]], sex = anthro_data[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "perc", male = 1, female = 0)) * 100, digits = 2)
  #
  # ## visit 5 BMI
  # anthro_data$child_bmi_v5 <-
  #   ifelse(is.na(anthro_data[["child_height_average_v5"]]) | is.na(anthro_data[["child_average_weight_v5"]]), NA,
  #          round(anthro_data[["child_average_weight_v5"]] / ((
  #            anthro_data[["child_height_average_v5"]] / 100
  #          ) ^ 2),
  #          digits = 2))
  #
  # anthro_data$child_bmi_z_v5 <- round(childsds::sds(value = anthro_data[["child_bmi_v5"]], age = anthro_data[["v5_age"]], sex = anthro_data[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = 1, female = 0), digits = 2)
  # anthro_data$child_bmi_p_v5 <- round((childsds::sds(value = anthro_data[["child_bmi_v5"]], age = anthro_data[["v5_age"]], sex = anthro_data[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "perc", male = 1, female = 0)) * 100, digits = 2)

  # separate visit data
  anthro_v1_data <- anthro_data[, grep("participant_id|v1", names(anthro_data))]
  colnames(anthro_v1_data) <- gsub("_v1|v1_", "", colnames(anthro_v1_data)) # Remove "v1_" or "_v1" from column names

  anthro_v5_data <- anthro_data[, grep("participant_id|v5", names(anthro_data))]
  colnames(anthro_v5_data) <- gsub("_v5|v5_", "", colnames(anthro_v5_data)) # Remove "v5_" or "_v5" from column names

  # make all vales numeric
  anthro_v1_data <- dplyr::mutate_all(anthro_v1_data, function(x) as.numeric(as.character(x)))
  anthro_v5_data <- dplyr::mutate_all(anthro_v5_data, function(x) as.numeric(as.character(x)))

  # stack anthro data
  stacked_anthro <- dplyr::bind_rows(
    transform(anthro_v1_data, visit = "1", session_id = "ses-1"),
    transform(anthro_v5_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)


  ## DEXA data ####

  # visit 1 data
  dexa_v1_data <- data[, grep("participant_id|^v1.*v1$", names(data))] # Subset columns that start with "v1" and end with "v1"
  colnames(dexa_v1_data) <- gsub("^v1_|_v1$", "", colnames(dexa_v1_data)) # Remove "v1_" and "_v1" from column names

  # visit 5 data
  dexa_v5_data <- data[, grep("participant_id|^v1.*v5$", names(data))] # Subset columns that start with "v1" and end with "v5"
  colnames(dexa_v5_data) <- gsub("^v1_|_v5$", "", colnames(dexa_v5_data)) # Remove "v1_" and "_v5" from column names

  # make all vales numeric
  dexa_v1_data <- dplyr::mutate_all(dexa_v1_data, function(x) as.numeric(as.character(x)))
  dexa_v5_data <- dplyr::mutate_all(dexa_v5_data, function(x) as.numeric(as.character(x)))

  # stack visit 1 and visit 5 data, add "visit" column, move "visit" to column 2
  stacked_dexa <- dplyr::bind_rows(
    transform(dexa_v1_data, visit = "1", session_id = "ses-1"),
    transform(dexa_v5_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)


  ## meal data ####
  meal_data <- data[, grep("participant_id|bread|butter|cheese|tender|carrot|chips|fruit|water|ranch|meal", names(data))]
  meal_data <- meal_data[, -grep("water_eah|intake_notes", names(meal_data))]

  # recalculate kcal consumed? this was calculated in redcap
  ## at the very least, re-calculate grilled cheese based on the amounts of the raw ingredients

  # visit 1 data
  v1_meal_data <- meal_data[, grep("participant_id|_v1$", names(meal_data))]
  names(v1_meal_data) <- gsub('_v1', '', names(v1_meal_data))

  # visit 3 data
  v3_meal_data <- meal_data[, grep("participant_id|_v3$", names(meal_data))]
  names(v3_meal_data) <- gsub('_v3', '', names(v3_meal_data))

  # visit 4 data
  v4_meal_data <- meal_data[, grep("participant_id|_v4$", names(meal_data))]
  names(v4_meal_data) <- gsub('_v4', '', names(v4_meal_data))

  # visit 5 data
  v5_meal_data <- meal_data[, grep("participant_id|_v5$", names(meal_data))]
  names(v5_meal_data) <- gsub('_v5', '', names(v5_meal_data))

  # make all vales numeric
  v1_meal_data <- dplyr::mutate_all(v1_meal_data, function(x) as.numeric(as.character(x)))
  v3_meal_data <- dplyr::mutate_all(v3_meal_data, function(x) as.numeric(as.character(x)))
  v4_meal_data <- dplyr::mutate_all(v4_meal_data, function(x) as.numeric(as.character(x)))
  v5_meal_data <- dplyr::mutate_all(v5_meal_data, function(x) as.numeric(as.character(x)))

  # stack meal data
  stacked_meal <- dplyr::bind_rows(
    transform(v1_meal_data, visit = "1", session_id = "ses-1"),
    transform(v3_meal_data, visit = "3", session_id = "ses-1"),
    transform(v4_meal_data, visit = "4", session_id = "ses-1"),
    transform(v5_meal_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  ## EAH data ####
  eah_data <- data[, grep("participant_id|brownie|corn_chip|kiss|ice_cream|oreo|popcorn|pretzel|skittle|starburst|eah", names(data))]
  eah_data <- eah_data[, -grep("complete|notes|intake_eah_visit_number", names(eah_data))] # do we want ad condition (ad_cond_eah) from the de data?

  # recalculate kcal consumed? this was calculated in redcap

  # add pre-eah meal total intake (kcal, g) for use as covariates

  # visit 3 data
  v3_eah_data <- eah_data[, grep("participant_id|_v3$", names(eah_data))]
  names(v3_eah_data) <- gsub('_v3', '', names(v3_eah_data))

  # visit 4 data
  v4_eah_data <- eah_data[, grep("participant_id|_v4$", names(eah_data))]
  names(v4_eah_data) <- gsub('_v4', '', names(v4_eah_data))

  # visit 5 data
  v5_eah_data <- eah_data[, grep("participant_id|_v5$", names(eah_data))]
  names(v5_eah_data) <- gsub('_v5', '', names(v5_eah_data))

  # make all vales numeric
  v3_eah_data <- dplyr::mutate_all(v3_eah_data, function(x) as.numeric(as.character(x)))
  v4_eah_data <- dplyr::mutate_all(v4_eah_data, function(x) as.numeric(as.character(x)))
  v5_eah_data <- dplyr::mutate_all(v5_eah_data, function(x) as.numeric(as.character(x)))

  # stack EAH data
  stacked_eah <- dplyr::bind_rows(
    transform(v3_eah_data, visit = "3", session_id = "ses-1"),
    transform(v4_eah_data, visit = "4", session_id = "ses-1"),
    transform(v5_eah_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  if (isTRUE(return_data)) {
    return(
      list(
        mri_visit_data = mri_visit_data,
        tictoc_data = tictoc_data,
        wasi_data = wasi_data,
        dexa_data = stacked_dexa,
        anthro_data = list(anthro_long = stacked_anthro,
                           anthro_wide = anthro_data),
        intake_data = list(meal_long = stacked_meal,
                           meal_wide = meal_data,
                           eah_long = stacked_eah,
                           eah_wide = eah_data)
      )
    )
  }

}

