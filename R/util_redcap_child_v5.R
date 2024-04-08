#' util_redcap_child_v5: Organize child visit 1 data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap data from REDCap visit data, event child_visit_5_arm_1
#'
#'
#' @param data data from REDCap event child_visit_5_arm_1
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) clean raw child visit 1 datasets
#'  2) meta-data/.json for each dataset
#' @importFrom rlang .data

util_redcap_child_v5 <- function(data, return_data = TRUE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
    stop("child data for REDCap event child_visit_5_arm_1 must be entered as a data.frame")
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  #reduce columns and update names

  ## visit data ####
  visit_data_child <- data[c('participant_id', 'v5_post_check_notes', 'v5_date', 'dxa_notes')]
  names(visit_data_child)[names(visit_data_child) == "v1_post_check_notes"] <- "v1_notes"

  ## intake information ####
  # note: this does not include intake or freddy fullness values, which will come from redcap double-entry data

  ## meal data
  meal_data <- data[, grepl('participant_id|meal|advertisement_condition', names(data))]
  meal_data <- meal_data[, !grepl('complete|freddy|consumed|hrt', names(meal_data))]
  names(meal_data) <- gsub('intake_notes', 'prep_notes', names(meal_data))

  ## meal vas data
  meal_vas_data <- data[, grepl('participant_id|vas_grilled|vas_chicken|vas_potato|vas_carrot|vas_fruit|vas_water', names(data))]

  ## eah data
  eah_data <- data[, grep("participant_id|wanting|advertisement_condition|eah_notes|eah_intake_notes", names(data))]
  eah_data <- eah_data[, -grep("complete|timestamp", names(eah_data))]
  names(eah_data) <- gsub('intake_notes', 'prep_notes', names(eah_data))
  names(eah_data) <- gsub('eah_notes', 'eah_protocol_notes', names(eah_data))

  ## eah vas data
  eah_vas_data <- data[, grepl('participant_id|vas_brownie|vas_corn_chip|vas_chocolate|vas_icecream|vas_cookie|vas_popcorn|vas_pretzel|vas_skittle|vas_starburst', names(data))]
  names(eah_vas_data) <- gsub('cookie', 'oreo', names(eah_vas_data))

  ## hrt ####
  hrt_data <- data[, grep("participant_id|hrt|food_|^q.*score", names(data))]

  ## Puberty ####
  puberty_data <- data[, grep("participant_id|^tanner|^childrep", names(data))]
  names(puberty_data)[names(puberty_data) == "tanner_sex_v5"] <- "sex"

  # in all columns except 1 (participant_id) replace 4s (I don't know) with 99
  puberty_data[,-c(1)][puberty_data[,-c(1)]==4]<-99

  # is p6 part of scoring for children? -- not in Table1 A Self-Administered sting Scale for Development Carskadon and Acebo

  # subset and rename girls variables
  puberty_data_girls <- subset(puberty_data, puberty_data$sex == 0)
  puberty_data_girls <- puberty_data_girls[, -grep("puberty_voice|puberty_facialhair|boycomp|tanner_male", names(puberty_data_girls))]
  puberty_data_girls <- puberty_data_girls %>% dplyr::rename(
    pds_1 = .data$childreport_puberty_height,
    pds_2 = .data$childreport_puberty_hair,
    pds_3 = .data$childreport_puberty_acne,
    pds_4f = .data$childrep_puberty_breast,
    pds_5fa = .data$childrep_puberty_menses,
    pds_6 = .data$childrep_puberty_girlcomp,
    tanner_choice = .data$tanner_female_choice_v5
  )

  # subset and rename boys variables
  puberty_data_boys <- subset(puberty_data, puberty_data$sex == 1)
  puberty_data_boys <- puberty_data_boys[, -grep("puberty_breast|puberty_menses|tanner_female|girlcomp", names(puberty_data_boys))]
  puberty_data_boys <- puberty_data_boys %>% dplyr::rename(
    pds_1 = .data$childreport_puberty_height,
    pds_2 = .data$childreport_puberty_hair,
    pds_3 = .data$childreport_puberty_acne,
    pds_4m = .data$childreport_puberty_voice,
    pds_5m = .data$childreport_puberty_facialhair,
    pds_6 = .data$childrep_puberty_boycomp,
    tanner_choice = .data$tanner_male_choice_v5
  )

  # bind girls and boys dfs -- dplyr::bind_rows fills missing values with NA where columns don't match.
  puberty_data_for_scoring <- dplyr::bind_rows(puberty_data_girls, puberty_data_boys)

  # score
  puberty_scored <- dataprepr::score_pds(puberty_data_for_scoring, respondent = "child", score_base = FALSE, male = 1, female = 0, id = "participant_id")

  ## loc ####
  loc_data <-data[, grep("participant_id|^loc", names(data))]
  loc_data <-loc_data[, -grep("share_info_parent", names(loc_data))]

  ## kbas ####
  kbas_data <-data[, grep("participant_id|toy_|food_|^q.*score", names(data))]
  # score this data?

  ## stq ####
  stq_data <-data[, grep("participant_id|stq", names(data))]
  # score this data?

  ## tictoc data ####
  tictoc_data <-data[, grep("participant_id|tictoc", names(data))]

  if (isTRUE(return_data)){
    return(list(visit_data_child = visit_data_child,
                meal_data = meal_data,
                meal_vas_data = meal_vas_data,
                eah_data = eah_data,
                eah_vas_data = eah_vas_data,
                hrt_data = hrt_data,
                puberty_data = puberty_scored,
                loc_data = loc_data,
                kbas_data = kbas_data,
                stq_data = stq_data,
                tictoc_data = tictoc_data))
  }

}

