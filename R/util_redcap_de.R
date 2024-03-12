#' util_redcap_de: Organize double-entry data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap double entry data data
#'
#'
#' @param data double-entry data
#' @param return_data If return_data is set to TRUE, will return a list including: mri_visit_data, tictoc_data, wasi_data, dexa_data, anthro_data (wide and long), intake_data (wide and long)
#'

util_redcap_de <- function(data, return_data = TRUE) {

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

  ## TICTOC ####
  tictoc_data <- data[, grep("participant_id|tictoc_", names(data))]
  tictoc_data <- tictoc_data[, !(names(tictoc_data) %in% c('tictoc_task_input_sheet_complete'))]

  ## WASI data ####
  wasi_data <- data[, grep("participant_id|wasi.*score$|wasi_percentile", names(data))]

  ## Anthropometrics data ####
  anthro_data <- data[, grep("participant_id|parent_height|child_height|parent_weight|child_weight|bmi", names(data))]
  anthro_data <- anthro_data[, -grep("complete|self_report", names(anthro_data))] #self-report will be merged from household questionnaire -- this is automatically taken from there (not entered data)

  # child bmi and percentile values were determined using the online form and entered into redcap -- recalculate?

  colnames(anthro_data) <- gsub("child_bmi_v1", "child_bmi", colnames(anthro_data))
  # names(anthro_data)[names(anthro_data) == "parent_height_sex"] <- "parent1_sex"
  # names(anthro_data)[names(anthro_data) == "parent_height_1_cm"] <- "parent1_height_1_cm"
  # names(anthro_data)[names(anthro_data) == "parent_height_2_cm"] <- "parent1_height_2_cm"
  # names(anthro_data)[names(anthro_data) == "parent_weight_1_kg"] <- "parent1_weight_1_kg"
  # names(anthro_data)[names(anthro_data) == "parent_weight_2_kg"] <- "parent1_weight_2_kg"
  # names(anthro_data)[names(anthro_data) == "parent_height_average_cm"] <- "parent1_height_average_cm"
  # names(anthro_data)[names(anthro_data) == "parent_weight_average_kg"] <- "parent1_weight_average_kg"
  # names(anthro_data)[names(anthro_data) == "parent_bmi_redcap_calc"] <- "parent1_bmi_measured"
  #

  # separate visit data
  anthro_v1_data <- anthro_data[, grep("participant_id|v1", names(anthro_data))]
  colnames(anthro_v1_data) <- gsub("_v1", "", colnames(anthro_v1_data)) # Remove "v1_" from column names

  anthro_v5_data <- anthro_data[, grep("participant_id|v5", names(anthro_data))]
  colnames(anthro_v5_data) <- gsub("_v5", "", colnames(anthro_v5_data)) # Remove "v5_" from column names


  # make all vales numeric
  anthro_v1_data <- dplyr::mutate_all(anthro_v1_data, function(x) as.numeric(as.character(x)))
  anthro_v5_data <- dplyr::mutate_all(anthro_v5_data, function(x) as.numeric(as.character(x)))

  # stack anthro data
  stacked_anthro <- dplyr::bind_rows(
    transform(anthro_v1_data, visit = "1"),
    transform(anthro_v5_data, visit = "5")
  ) %>% dplyr::relocate(visit, .after = 1)



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
    transform(dexa_v1_data, visit = "1"),
    transform(dexa_v5_data, visit = "5")
  ) %>% dplyr::relocate(visit, .after = 1)


  ## meal data ####

  # subset intake and freddies
  intake_data <- data[, grep("participant_id|pre_w_o_plate|pre_w_plate|post_w_plate|consumed|freddy_v1|freddy_v3|freddy_v4|freddy_v5", names(data))]
  intake_data <- intake_data[, -grep("eah.*v1$", names(intake_data))] # remove EAH v1 FFs -- there was no EAH on visit 1

  # visit 1 data
  v1_intake_data <- intake_data[, grep("participant_id|_v1$", names(intake_data))]
  names(v1_intake_data) <- gsub('_v1', '', names(v1_intake_data))

  # visit 3 data
  v3_intake_data <- intake_data[, grep("participant_id|_v3$", names(intake_data))]
  names(v3_intake_data) <- gsub('_v3', '', names(v3_intake_data))

  # visit 4 data
  v4_intake_data <- intake_data[, grep("participant_id|_v4$", names(intake_data))]
  names(v4_intake_data) <- gsub('_v4', '', names(v4_intake_data))

  # visit 5 data
  v5_intake_data <- intake_data[, grep("participant_id|_v5$", names(intake_data))]
  names(v5_intake_data) <- gsub('_v5', '', names(v5_intake_data))

  # make all vales numeric
  v1_intake_data <- dplyr::mutate_all(v1_intake_data, function(x) as.numeric(as.character(x)))
  v3_intake_data <- dplyr::mutate_all(v3_intake_data, function(x) as.numeric(as.character(x)))
  v4_intake_data <- dplyr::mutate_all(v4_intake_data, function(x) as.numeric(as.character(x)))
  v5_intake_data <- dplyr::mutate_all(v5_intake_data, function(x) as.numeric(as.character(x)))

  # stack intake data
  stacked_intake <- dplyr::bind_rows(
    transform(v1_intake_data, visit = "1"),
    transform(v3_intake_data, visit = "3"),
    transform(v4_intake_data, visit = "4"),
    transform(v5_intake_data, visit = "5")
  ) %>% dplyr::relocate(visit, .after = 1)


  if (isTRUE(return_data)) {
    return(
      list(
        mri_visit_data = mri_visit_data,
        tictoc_data = tictoc_data,
        wasi_data = wasi_data,
        dexa_data = stacked_dexa,
        anthro_data = list(anthro_long = stacked_anthro,
                           anthro_wide = anthro_data),
        intake_data = list(intake_long = stacked_intake,
                           intake_wide = intake_data)
      )
    )
  }

}

