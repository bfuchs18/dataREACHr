#' proc_redcap: Process raw data downloaded from Study REACH REDCap
#'
#' This function:
#' 1) reads REDCap data from sourcedata
#' 2) cleans data to save in BIDS format in phenotype. Produces the following files:
#'    *
#' 3) calls functions to create .json files for each phenotype/x.tsv file
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @param visit_data_path full path to the redcap visit data in bids/sourcedata/phenotype directory
#' @param data_de_path full path to the redcap double entry data in bids/sourcedata/phenotype directory
#' @param overwrite overwrite existing files (default = FALSE)
#' @param return_data return phenotype to console (default = FLASE)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw phenotype datasets for each task
#'  2) meta-data/.json inforamtion for each task
#'
#' @examples
#'
#' data_de_path = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids/sourcedata/phenotype/REACHDataDoubleEntry_DATA_2024-03-12_1045.csv"
#' visit_data_path = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids/sourcedata/phenotype/FoodMarketingResilie_DATA_2024-03-22_1446.csv"
#'
#' phenotype_data <- proc_redcap(visit_data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#' @importFrom utils tail write.csv read.csv
#' @export

proc_redcap <- function(visit_data_path, data_de_path, overwrite = FALSE, return_data = FALSE) {

  # For testing
  # visit_data_path = "/Users/baf44/Keller_Marketing/ParticipantData/bids/sourcedata/phenotype/FoodMarketingResilie_DATA_2024-02-16_1544.csv"

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(visit_data_path)

  if (isTRUE(data_arg)) {
    if (!is.character(visit_data_path)) {
      stop("visit_data_path must be entered as a string")
    } else if (!file.exists(visit_data_path)) {
      stop("visit_data_path entered, but file does not exist. Check visit_data_path string.")
    }
  } else if (isFALSE(data_arg)) {
    stop("visit_data_path must be entered as a string")
  }


  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The proc_redcap.R has not been thoroughly tested on Windows systems, may have visit_data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # find location of slashes so can decompose filepaths
  slash_loc <- unlist(gregexpr(slash, visit_data_path))

  # set paths for other directories
  base_wd <- substr(visit_data_path, 1, tail(slash_loc, 4))
  bids_wd <- substr(visit_data_path, 1, tail(slash_loc, 3))
  phenotype_wd <- paste0(bids_wd, slash, 'phenotype', slash)

  # add file ending if it is missing
  if (!grep('.csv', visit_data_path)) {
    visit_data_file <- paste0(visit_data_path, '.csv')
  } else {
    visit_data_file <- visit_data_path
  }

  if (!grep('.csv', data_de_path)) {
    data_de_file <- paste0(data_de_path, '.csv')
  } else {
    data_de_file <- data_de_path
  }

  # check file existis
  if (!file.exists(visit_data_file)) {
    stop ('entered visit_data_path is not an existing file - be sure it is entered as a string and contains the full data path and file name')
  }

  if (!file.exists(data_de_file)) {
    stop ('entered data_de_path is not an existing file - be sure it is entered as a string and contains the full data path and file name')
  }


  #### Load and organize visit data ####
  redcap_visit_data <- read.csv(visit_data_path, header = TRUE)

  # # subset events and remove unnecessary columns
  redcap_long_wide <- function(event_name, data){

    #subset
    sub_dat <- data[data[['redcap_event_name']] == event_name, ]

    #remove empty columns
    sub_dat <- sub_dat[, !colSums(is.na(sub_dat) | sub_dat == "") == nrow(sub_dat)]
    #return
    return(sub_dat)
  }

  # Extract visit data ####
  child_visit_1_arm_1 <- redcap_long_wide('child_visit_1_arm_1', redcap_visit_data)
  parent_visit_1_arm_1 <- redcap_long_wide('parent_visit_1_arm_1', redcap_visit_data)
  child_visit_2_arm_1 <- redcap_long_wide('child_visit_2_arm_1', redcap_visit_data)
  parent_visit_2_arm_1 <- redcap_long_wide('parent_visit_2_arm_1', redcap_visit_data)
  child_visit_3_arm_1 <- redcap_long_wide('child_visit_3_arm_1', redcap_visit_data)
  parent_visit_3_arm_1 <- redcap_long_wide('parent_visit_3_arm_1', redcap_visit_data)
  child_visit_4_arm_1 <- redcap_long_wide('child_visit_4_arm_1', redcap_visit_data)
  parent_visit_4_arm_1 <- redcap_long_wide('parent_visit_4_arm_1', redcap_visit_data)
  child_visit_5_arm_1 <- redcap_long_wide('child_visit_5_arm_1', redcap_visit_data)
  parent_visit_5_arm_1 <- redcap_long_wide('parent_visit_5_arm_1', redcap_visit_data)

  # Make dataframe of visit dates and ages
  date_data <- merge(child_visit_1_arm_1[, c("record_id", "v1_date")], child_visit_2_arm_1[, c("record_id", "v2_date")], by = "record_id", all = TRUE)
  date_data <- merge(date_data, child_visit_3_arm_1[, c("record_id", "v3_date")], by = "record_id", all = TRUE)
  date_data <- merge(date_data, child_visit_4_arm_1[, c("record_id", "v4_date")], by = "record_id", all = TRUE)
  date_data <- merge(date_data, child_visit_5_arm_1[, c("record_id", "v5_date")], by = "record_id", all = TRUE)
  date_data <- merge(date_data, parent_visit_2_arm_1[, c("record_id", "behavior_rating_inventory_of_executive_function_timestamp")], by = "record_id", all = TRUE)

  # add child sex and dob to date_data
  date_data <- merge(date_data, parent_visit_1_arm_1[, c("record_id", "prs_sex", "demo_child_birthdate")], by = "record_id", all = TRUE)

  # add ages to date_data
  date_data[['v1_date']] <- lubridate::as_date(date_data[['v1_date']])
  date_data[['v2_date']] <- lubridate::as_date(date_data[['v2_date']])
  date_data[['v3_date']] <- lubridate::as_date(date_data[['v3_date']])
  date_data[['v4_date']] <- lubridate::as_date(date_data[['v4_date']])
  date_data[['v5_date']] <- lubridate::as_date(date_data[['v5_date']])
  date_data[['brief_date']] <- lubridate::as_date(date_data[['behavior_rating_inventory_of_executive_function_timestamp']])
  date_data[['demo_child_birthdate']] <- lubridate::as_date(date_data[['demo_child_birthdate']])
  date_data[['v1_age']] <- lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v1_date']])/lubridate::years(1)
  date_data[['v2_age']] <- lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v2_date']])/lubridate::years(1)
  date_data[['v3_age']] <- lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v3_date']])/lubridate::years(1)
  date_data[['v4_age']] <- lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v4_date']])/lubridate::years(1)
  date_data[['v5_age']] <- lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v5_date']])/lubridate::years(1)
  date_data[['brief_age']] <- lubridate::interval(date_data[['demo_child_birthdate']], date_data[['brief_date']])/lubridate::years(1)

  #update column names in date_data
  names(date_data)[names(date_data) == "record_id"] <- "participant_id"
  names(date_data)[names(date_data) == "prs_sex"] <- "sex"

  # # organize event data
  child_v1_data <- util_redcap_child_v1(child_visit_1_arm_1)
  parent_v1_data <- util_redcap_parent_v1(parent_visit_1_arm_1, v1_date_data = date_data)
  child_v2_data <- util_redcap_child_v2(child_visit_2_arm_1)
  parent_v2_data <- util_redcap_parent_v2(parent_visit_2_arm_1, agesex_data = date_data)
  child_v3_data <- util_redcap_child_v3(child_visit_3_arm_1)
  parent_v3_data <- util_redcap_parent_v3(parent_visit_3_arm_1)
  child_v4_data <- util_redcap_child_v4(child_visit_4_arm_1)
  parent_v4_data <- util_redcap_parent_v4(parent_visit_4_arm_1)
  child_v5_data <- util_redcap_child_v5(child_visit_5_arm_1)
  parent_v5_data <- util_redcap_parent_v5(parent_visit_5_arm_1, v5_date_data= date_data)

  #### Load and organize double-entry data ####
  redcap_de_data <- read.csv(data_de_path, header = TRUE)
  processed_de_data <- util_redcap_de(redcap_de_data, agesex_data = date_data)

  #### Stack visit data collected on 2 visits ####
  # Note: double entry data collected on multiple visits is stacked by util_redcap_de()

  # Dataframes will have a "visit" and "session_id" columns added before stacking, and these will be moved to columns 2 and 3

  stacked_stq <- dplyr::bind_rows(
    transform(child_v1_data$stq_data, visit = "1", session_id = "ses-1"),
    transform(child_v5_data$stq_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_kbas <- dplyr::bind_rows(
    transform(child_v1_data$kbas_data, visit = "1", session_id = "ses-1"),
    transform(child_v5_data$kbas_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_loc <- dplyr::bind_rows(
    transform(child_v4_data$loc_data, visit = "4", session_id = "ses-1"),
    transform(child_v5_data$loc_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_demo <- dplyr::bind_rows(
    transform(parent_v1_data$demo_data, visit = "1", session_id = "ses-1"),
    transform(parent_v5_data$demo_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_household <- dplyr::bind_rows(
    transform(parent_v1_data$household_data, visit = "1", session_id = "ses-1"),
    transform(parent_v5_data$household_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_cebq <- dplyr::bind_rows(
    transform(parent_v1_data$cebq_data$bids_phenotype, visit = "1", session_id = "ses-1"),
    transform(parent_v5_data$cebq_data$bids_phenotype, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_cbq <- dplyr::bind_rows(
    transform(parent_v2_data$cbq_data$bids_phenotype, visit = "2", session_id = "ses-1"),
    transform(parent_v5_data$cbq_data$bids_phenotype, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  # not scored/in bids_phenotype yet
  # stacked_cshq <- dplyr::bind_rows(
  #   transform(parent_v2_data$cshq_data$bids_phenotype, visit = "2", session_id = "ses-1"),
  #   transform(parent_v5_data$cshq_data$bids_phenotype, visit = "5", session_id = "ses-2")
  # ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  # not scored/in bids_phenotype yet
  # stacked_pstca <- dplyr::bind_rows(
  #   transform(parent_v3_data$pstca_data$bids_phenotype, visit = "3", session_id = "ses-1"),
  #   transform(parent_v5_data$pstca_data$bids_phenotype, visit = "5", session_id = "ses-2")
  # ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_audit <- dplyr::bind_rows(
    transform(parent_v4_data$audit_data$bids_phenotype, visit = "4", session_id = "ses-1"),
    transform(parent_v5_data$audit_data$bids_phenotype, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  # not scored/in bids_phenotype yet
  # stacked_pmum <- dplyr::bind_rows(
  #   transform(parent_v4_data$pmum_data$bids_phenotype, visit = "4", session_id = "ses-1"),
  #   transform(parent_v5_data$pmum_data$bids_phenotype, visit = "5", session_id = "ses-2")
#  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_cfpq <- dplyr::bind_rows(
    transform(parent_v4_data$cfpq_data$bids_phenotype, visit = "4", session_id = "ses-1"),
    transform(parent_v5_data$cfpq_data$bids_phenotype, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  # not scored/in bids_phenotype yet - will it be?
  # stacked_rank <- dplyr::bind_rows(
  #   transform(parent_v1_data$rank_data$bids_phenotype, visit = "3", session_id = "ses-1"),
  #   transform(parent_v5_data$rank_data$bids_phenotype, visit = "5", session_id = "ses-2")
  # ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  # not scored/in bids_phenotype yet - will it be?
  # stacked_class <- dplyr::bind_rows(
  #   transform(parent_v3_data$class_data$bids_phenotype, visit = "3", session_id = "ses-1"),
  #   transform(parent_v5_data$class_data$bids_phenotype, visit = "5", session_id = "ses-2")
  # ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  # stack child (V5) and parent (V1, V5) puberty data
  stacked_puberty <- dplyr::bind_rows(
    transform(parent_v1_data$puberty_data$bids_phenotype, visit = "1", session_id = "ses-1", respondent = "parent"),
    transform(parent_v5_data$puberty_data$bids_phenotype, visit = "5", session_id = "ses-2", respondent = "parent"),
    transform(child_v5_data$puberty_data$bids_phenotype, visit = "5", session_id = "ses-2", respondent = "child"),
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2) %>% dplyr::relocate(respondent, .after = 3)

  # note: visit column is vas_visit -- this data will merged with intake data by session_id only
  stacked_meal_vas_data <- dplyr::bind_rows(
    transform(child_v1_data$meal_vas_data, vas_visit = "1", session_id = "ses-1"),
    transform(child_v5_data$meal_vas_data, vas_visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(vas_visit, .after = 2)

  # note: visit column is vas_visit -- this data will merged with intake data by session_id only
  stacked_eah_vas_data <- dplyr::bind_rows(
    transform(child_v1_data$eah_vas_data, vas_visit = "1", session_id = "ses-1"),
    transform(child_v5_data$eah_vas_data, vas_visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(vas_visit, .after = 2)

  stacked_meal_data <- dplyr::bind_rows(
    transform(child_v1_data$meal_data, visit = "1", session_id = "ses-1"),
    transform(child_v3_data$meal_data, visit = "3", session_id = "ses-1"),
    transform(child_v4_data$meal_data, visit = "4", session_id = "ses-1"),
    transform(child_v5_data$meal_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_eah_data <- dplyr::bind_rows(
    transform(child_v3_data$eah_data, visit = "3", session_id = "ses-1"),
    transform(child_v4_data$eah_data, visit = "4", session_id = "ses-1"),
    transform(child_v5_data$eah_data, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_parent2_anthro <- dplyr::bind_rows(
    transform(parent_v1_data$household_data[, c("participant_id", "parent2_reported_bmi")], visit = "1", session_id = "ses-1"),
    transform(parent_v5_data$household_data[, c("participant_id", "parent2_reported_bmi")], visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  stacked_updates <- dplyr::bind_rows(
    transform(parent_v2_data$visit_data_parent, visit = "2", session_id = "ses-1"),
    transform(parent_v3_data$visit_data_parent, visit = "3", session_id = "ses-1"),
    transform(parent_v4_data$visit_data_parent, visit = "4", session_id = "ses-1"),
    transform(parent_v5_data$visit_data_parent, visit = "5", session_id = "ses-2")
  ) %>% dplyr::relocate(session_id, .after = 1) %>% dplyr::relocate(visit, .after = 2)

  #### Merge visit intake (meal, EAH, vas) data ####
  merged_vas_data <- merge(stacked_meal_vas_data, stacked_eah_vas_data, by=c("participant_id","session_id", "vas_visit"), all = TRUE)
  merged_intake_data <- merge(stacked_meal_data, stacked_eah_data, by=c("participant_id","visit", "session_id", "advertisement_condition"), all = TRUE)
  merged_intake_data <- merge(merged_intake_data, merged_vas_data, by=c("participant_id", "session_id"), all = TRUE)

  #### Merge visit data and double entry (de) data ####

  # de anthro_data with stacked_parent2_anthro (data from household_data)
  merged_anthro <- merge(processed_de_data$anthro_data$anthro_long, stacked_parent2_anthro, by=c("participant_id","visit", "session_id"), all = TRUE)

  # merge intake data

  # merge notes/visit data? update data?

  # Merge data that belongs in participant.tsv

  # merge MRI visit data double entry CAMS / MRI freddies
  merged_mri <- merge(child_v2_data$mri_notes, processed_de_data$mri_visit_data, by = "participant_id", all = TRUE)

  # merge intake_data from visits and double-entry



  #### Export Phenotype Data ####

  # Update to only export if overwrite == TRUE

  # generate phenotype_wd if it doesnt exist
  if (!file.exists(phenotype_wd)){
    dir.create(file.path(phenotype_wd))
  }

  # export forms collected at 1 visit only (i.e., not stacked data) -- can this be done using a list of lists and a loop?
  write.csv(parent_v1_data$cfq_data$bids_phenotype, paste0(phenotype_wd, slash, 'cfq.tsv'), row.names = FALSE) # cfq
  write.csv(parent_v1_data$efcr_data$bids_phenotype, paste0(phenotype_wd, slash, 'efcr.tsv'), row.names = FALSE) #efcr
  write.csv(parent_v1_data$lbc_data$bids_phenotype, paste0(phenotype_wd, slash, 'lbc.tsv'), row.names = FALSE) #lbc
  write.csv(parent_v1_data$pss_data$bids_phenotype, paste0(phenotype_wd, slash, 'pss.tsv'), row.names = FALSE) # pss
  # write.csv(parent_v1_data$chaos_data$bids_phenotype, paste0(phenotype_wd, slash, 'chaos.tsv'), row.names = FALSE) # chaos -- need to develop score script


  write.csv(parent_v2_data$brief_data$bids_phenotype, paste0(phenotype_wd, slash, 'brief2.tsv'), row.names = FALSE) #brief
  write.csv(parent_v2_data$bes_data$bids_phenotype, paste0(phenotype_wd, slash, 'bes.tsv'), row.names = FALSE) #bes
  write.csv(parent_v2_data$ffbs_data$bids_phenotype, paste0(phenotype_wd, slash, 'ffbs.tsv'), row.names = FALSE) #ffbs
  # write.csv(parent_v2_data$ffq_data$bids_phenotype, paste0(phenotype_wd, slash, 'ffq.tsv'), row.names = FALSE) #ffq -- will this be bids_phenotype

  write.csv(parent_v3_data$spsrq_data$bids_phenotype, paste0(phenotype_wd, slash, 'spsrq.tsv'), row.names = FALSE) # spsrq
  write.csv(parent_v3_data$pwlb_data$bids_phenotype, paste0(phenotype_wd, slash, 'pwlb.tsv'), row.names = FALSE) # pwlb
  write.csv(parent_v3_data$tfeq_data$bids_phenotype, paste0(phenotype_wd, slash, 'tfeq.tsv'), row.names = FALSE) # tfeq
  write.csv(parent_v3_data$bisbas_data$bids_phenotype, paste0(phenotype_wd, slash, 'bisbas.tsv'), row.names = FALSE) # bisbas
  write.csv(parent_v3_data$debq_data$bids_phenotype, paste0(phenotype_wd, slash, 'debq.tsv'), row.names = FALSE) # debq
  # write.csv(parent_v3_data$scpf_data$bids_phenotype, paste0(phenotype_wd, slash, 'scpf.tsv'), row.names = FALSE) # scpf -- will this be bids_phenotype

  write.csv(child_v4_data$pptq_data$bids_phenotype, paste0(phenotype_wd, slash, 'pptq.tsv'), row.names = FALSE) # debq

  # export stacked dataframes

  # write.csv(stacked_demo, paste0(phenotype_wd, slash, 'demo.tsv'), row.names = FALSE) # should this be the participants.tsv?
  write.csv(merged_anthro, paste0(phenotype_wd, slash, 'anthropometrics.tsv'), row.names = FALSE)

  write.csv(stacked_stq, paste0(phenotype_wd, slash, 'stq.tsv'), row.names = FALSE)
  write.csv(stacked_kbas, paste0(phenotype_wd, slash, 'kbas.tsv'), row.names = FALSE)
  write.csv(stacked_household, paste0(phenotype_wd, slash, 'household.tsv'), row.names = FALSE) # should this be the participants.tsv?
  write.csv(stacked_cebq, paste0(phenotype_wd, slash, 'cebq.tsv'), row.names = FALSE)
  write.csv(stacked_cbq, paste0(phenotype_wd, slash, 'cbq.tsv'), row.names = FALSE)

  # write.csv(stacked_cshq, paste0(phenotype_wd, slash, 'cshq.tsv'), row.names = FALSE)
  # write.csv(stacked_pstca, paste0(phenotype_wd, slash, 'pstca.tsv'), row.names = FALSE)
  write.csv(stacked_audit, paste0(phenotype_wd, slash, 'audit.tsv'), row.names = FALSE)
  # write.csv(stacked_pmum, paste0(phenotype_wd, slash, 'pmum.tsv'), row.names = FALSE)
  write.csv(stacked_cfpq, paste0(phenotype_wd, slash, 'cfpq.tsv'), row.names = FALSE)
  # write.csv(stacked_rank, paste0(phenotype_wd, slash, 'rank.tsv'), row.names = FALSE)
  write.csv(stacked_puberty, paste0(phenotype_wd, slash, 'puberty.tsv'), row.names = FALSE)
  write.csv(stacked_loc, paste0(phenotype_wd, slash, 'loc.tsv'), row.names = FALSE)


  # export double entry dexa data
  write.csv(processed_de_data$dexa_data, paste0(phenotype_wd, slash, 'dexa.tsv'), row.names = FALSE)


  # Export meta-data
  # make separate overwrite args -- 1 for dataframes and 1 for jsons?
  write_jsons(export_dir = phenotype_wd, overwrite = overwrite)

  # return meta-data with this? if so, need to update write_jsons() to return meta-data
  if (isTRUE(return_data)){
    return(list( child_v1_data = child_v1_data,
                 child_v2_data = child_v2_data,
                 child_v3_data = child_v3_data,
                 child_v4_data = child_v4_data,
                 child_v5_data = child_v5_data,
                 parent_v1_data = parent_v1_data,
                 parent_v2_data = parent_v2_data,
                 parent_v3_data = parent_v3_data,
                 parent_v4_data = parent_v4_data,
                 parent_v5_data = parent_v5_data,
                 processed_de_data = processed_de_data
                 # meta_data = meta_data
                 ))
  }
}

