#' proc_redcap: Process raw data downloaded from Study REACH REDCap
#'
#' This function:
#' 1) Reads REDCap data (visit and double-entry) from bids/sourcedata
#' 2) Calls util_ functions to clean and compile data in dataframes
#' 3) Calls json_ functions to create strings with meta-data stored in JSON format for each dataframe
#' 4) Exports the following BIDS-compliant .tsv files into bids/phenotype:
#'    * questionnaire data - raw and scores: efcr, lbc, pss, brief2, bes, ffbs, spsrq, pwlb, tfeq, bisbas, debq, scpf, hfssm, cchip, pptq, stq, kbas, cebq, audit, cfpq, puberty, cshq, chaos, cfq
#'    * questionnaire data - raw only: infancy, household, fsq, hfias, sic, fhfi, pstca, pmum, rank, loc, cbq
#'    * compiled and researcher-entered data: demographics, intake, anthropometrics, dexa, mri_visit, sleeplog, (wasi?, notes?, updates?)
#' 5) Exports bids/participants.tsv
#' 6) Exports a .json file with meta-data for each .tsv
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @param visit_data_path full path to the redcap visit data in bids/sourcedata/phenotype directory
#' @param data_de_path full path to the redcap double entry data in bids/sourcedata/phenotype directory
#' @param overwrite overwrite existing files (default = FALSE)
#' @param return_data return raw and processed data to console (default = FALSE)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) input_data: list with 2 dataframes with raw data (visit_data, de_data (double_entered data))
#'  2) visit_data: list of 10 dataframes with intermediate-processed visit data (child_v1_data, child_v2_data, child_v3_data, child_v4_data, child_v5_data, parent_v1_data, parent_v2_data, parent_v3_data, parent_v4_data, parent_v5_data)
#'  3) double_entry_data: list of dataframes with intermediate-processed double entry data
#'  4) phenotype_data: list of dataframes with processed data that gets exported into bids/phenotype
#'
#' @examples
#'
#' \dontrun{
#' data_de_path = "/Users/baf44/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/bids/sourcedata/phenotype/REACHDataDoubleEntry_DATA_2024-08-14_1521.csv"
#' visit_data_path = "/Users/baf44/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/bids/sourcedata/phenotype/FoodMarketingResilie_DATA_2024-08-14_1522.csv"
#' redcap_data <- proc_redcap(visit_data_path, data_de_path, return = TRUE)
#'
#' }
#'
#' @importFrom utils tail write.table read.csv head
#' @importFrom rlang .data
#' @export

proc_redcap <- function(visit_data_path, data_de_path, overwrite = FALSE, return_data = FALSE) {

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

  #### Load data ####
  redcap_visit_data <- read.csv(visit_data_path, header = TRUE) # visit data
  redcap_de_data <- read.csv(data_de_path, header = TRUE) # double entry data

  #### Extract visit data ####

  # Make ID column bids compliant: Convert record_id to strings padded with zeros and add "sub_"
  redcap_visit_data$record_id <- sprintf("sub-%03d", redcap_visit_data$record_id)

  # # subset events and remove unnecessary columns
  redcap_long_wide <- function(event_name, data){

    #subset
    sub_dat <- data[data[['redcap_event_name']] == event_name, ]

    #remove empty columns
    sub_dat <- sub_dat[, !colSums(is.na(sub_dat) | sub_dat == "") == nrow(sub_dat)]
    #return
    return(sub_dat)
  }

  # Extract visit data
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

  #### Generate date_data with visit dates/ages and sex ####

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
  date_data[['v1_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v1_date']])/lubridate::years(1), 1)
  date_data[['v2_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v2_date']])/lubridate::years(1), 1)
  date_data[['v3_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v3_date']])/lubridate::years(1), 1)
  date_data[['v4_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v4_date']])/lubridate::years(1), 1)
  date_data[['v5_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v5_date']])/lubridate::years(1), 1)
  date_data[['brief_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['brief_date']])/lubridate::years(1),1)

  # re-label sex var and save to sex
  date_data$sex <- ifelse(date_data$prs_sex == 0, "female", ifelse(date_data$prs_sex == 1, "male", NA))
  date_data <- date_data[,!(names(date_data) %in% c("prs_sex"))] # remove prs_sex

  #update column names in date_data
  names(date_data)[names(date_data) == "record_id"] <- "participant_id"

  #### Process visit data ####
  child_v1_data <- util_redcap_child_v1(child_visit_1_arm_1)
  parent_v1_data <- util_redcap_parent_v1(parent_visit_1_arm_1)
  child_v2_data <- util_redcap_child_v2(child_visit_2_arm_1)
  parent_v2_data <- util_redcap_parent_v2(parent_visit_2_arm_1, agesex_data = date_data)
  child_v3_data <- util_redcap_child_v3(child_visit_3_arm_1)
  parent_v3_data <- util_redcap_parent_v3(parent_visit_3_arm_1)
  child_v4_data <- util_redcap_child_v4(child_visit_4_arm_1)
  parent_v4_data <- util_redcap_parent_v4(parent_visit_4_arm_1)
  child_v5_data <- util_redcap_child_v5(child_visit_5_arm_1)
  parent_v5_data <- util_redcap_parent_v5(parent_visit_5_arm_1)


  #### Process double-entry data ####
  processed_de_data <- util_redcap_de(redcap_de_data)

  #### Stack visit data collected on multiple visits ####

  # Note: double entry data collected on multiple visits is stacked by util_redcap_de()

  stacked_stq <-
    dplyr::bind_rows(child_v1_data$stq_data, child_v5_data$stq_data)

  stacked_kbas <-
    dplyr::bind_rows(child_v1_data$kbas_data, child_v5_data$kbas_data)

  stacked_loc <-
    dplyr::bind_rows(child_v4_data$loc_data,
                     child_v5_data$loc_data)

  stacked_household <-
    dplyr::bind_rows(parent_v1_data$household_data,
                     parent_v5_data$household_data)

  stacked_cebq <-
    dplyr::bind_rows(
      parent_v1_data$cebq_data$bids_phenotype,
      parent_v5_data$cebq_data$bids_phenotype
    ) %>% dplyr::relocate("session_id", .after = 1)

  stacked_cbq <-
    dplyr::bind_rows(parent_v2_data$cbq_data$bids_phenotype,
                     parent_v5_data$cbq_data$bids_phenotype)

  stacked_cshq <-
    dplyr::bind_rows(
      parent_v2_data$cshq_data$bids_phenotype,
      parent_v5_data$cshq_data$bids_phenotype
    )

  # not scored/in bids_phenotype yet
  stacked_pstca <- dplyr::bind_rows(parent_v3_data$pstca_data, parent_v5_data$pstca_data)
  # stacked_pstca <- dplyr::bind_rows(parent_v3_data$pstca_data$bids_phenotype, parent_v5_data$pstca_data$bids_phenotype)

  stacked_audit <-
    dplyr::bind_rows(
      parent_v4_data$audit_data$bids_phenotype,
      parent_v5_data$audit_data$bids_phenotype)

  # not scored/in bids_phenotype yet
  stacked_pmum <- dplyr::bind_rows(parent_v4_data$pmum_data, parent_v5_data$pmum_data)
  # stacked_pmum <- dplyr::bind_rows(parent_v4_data$pmum_data$bids_phenotype, parent_v5_data$pmum_data$bids_phenotype)

  stacked_cfpq <-
    dplyr::bind_rows(
      parent_v4_data$cfpq_data$bids_phenotype,
      parent_v5_data$cfpq_data$bids_phenotype)

  # not scored/in bids_phenotype yet - will it be?
  stacked_rank <- dplyr::bind_rows(parent_v1_data$rank_data, parent_v5_data$rank_data)
  # stacked_rank <- dplyr::bind_rows(parent_v1_data$rank_data$bids_phenotype, parent_v5_data$rank_data$bids_phenotype)

  # not scored/in bids_phenotype yet - will it be?
  stacked_class <- dplyr::bind_rows(parent_v3_data$class_data, parent_v5_data$class_data)
  # stacked_class <- dplyr::bind_rows(parent_v3_data$class_data$bids_phenotype, parent_v5_data$class_data$bids_phenotype)

  # stack child (V5) and parent (V1, V5) puberty data
  stacked_puberty <- dplyr::bind_rows(
    transform(parent_v1_data$puberty_data$bids_phenotype, respondent = "parent"),
    transform(parent_v5_data$puberty_data$bids_phenotype, respondent = "parent"),
    transform(child_v5_data$puberty_data$bids_phenotype, respondent = "child")) %>% dplyr::relocate("respondent", .after = 4)

  # note: visit column is liking_visit_protocol -- this data will merged with intake data by session_id only
  stacked_liking_data <- dplyr::bind_rows(
    transform(child_v1_data$liking_data, liking_visit_protocol = "1"),
    transform(child_v5_data$liking_data, liking_visit_protocol = "5")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("liking_visit_protocol", .after = 2)

  stacked_wanting_data <- dplyr::bind_rows(
      transform(child_v3_data$eah_wanting, visit_protocol = "3"),
      transform(child_v4_data$eah_wanting, visit_protocol = "4"),
      transform(child_v5_data$eah_wanting, visit_protocol = "5")
    ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  stacked_fullness_data <- dplyr::bind_rows(
    transform(child_v1_data$freddy_data, visit_protocol = "1"),
    transform(child_v3_data$freddy_data, visit_protocol = "3"),
    transform(child_v4_data$freddy_data, visit_protocol = "4"),
    transform(child_v5_data$freddy_data, visit_protocol = "5")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  stacked_food_paradigm_info <- dplyr::bind_rows(
    transform(child_v1_data$food_paradigm_info, visit_protocol = "1"),
    transform(child_v3_data$food_paradigm_info, visit_protocol = "3"),
    transform(child_v4_data$food_paradigm_info, visit_protocol = "4"),
    transform(child_v5_data$food_paradigm_info, visit_protocol = "5")
    ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  stacked_updates <- dplyr::bind_rows(
    parent_v2_data$visit_data_parent,
    parent_v3_data$visit_data_parent,
    parent_v4_data$visit_data_parent,
    parent_v5_data$visit_data_parent
  )

  stacked_visit_anthro_data <- dplyr::bind_rows(
    transform(child_v1_data$anthro_data, visit_protocol = "1"),
    transform(child_v5_data$anthro_data, visit_protocol = "5")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  stacked_visit_intake_data <- dplyr::bind_rows(
    transform(child_v1_data$intake_data, visit_protocol = "1"),
    transform(child_v3_data$intake_data, visit_protocol = "3"),
    transform(child_v4_data$intake_data, visit_protocol = "4"),
    transform(child_v5_data$intake_data, visit_protocol = "5")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  ### Process stacked intake data from visit forms (not double entered) ----
  # intake data from visit forms will output until double entry data is ready
  stacked_visit_intake_data  <- util_calc_intake(stacked_visit_intake_data)

  ### Merge intake-related data ----
  # merge intake-related data (paradigm info, liking data, wanting data, intake data, fullness data)
  merged_intake <- merge(stacked_food_paradigm_info, stacked_liking_data, by=c("participant_id", "session_id"), all = TRUE) #paradigm info and liking
  merged_intake <- merge(merged_intake, stacked_wanting_data, by=c("participant_id","visit_protocol", "session_id"), all = TRUE) # add wanting
  merged_intake <- merge(merged_intake, stacked_fullness_data, by=c("participant_id","visit_protocol", "session_id"), all = TRUE) # add fullness -- for now, use visit data (not double entered)
  merged_intake <- merge(merged_intake, stacked_visit_intake_data, by=c("participant_id","visit_protocol", "session_id"), all = TRUE) # add intake -- for now, use visit data (not double entered)
  # merged_intake <- merge(merged_intake, processed_de_data$intake_data, by=c("participant_id","visit_protocol", "session_id"), all = TRUE) # uncomment when double-entered intake data is available

  ## merge notes/visit data? update data?

  # merge MRI visit data and cams/fullness data -- cams/fullness data may eventually be double entered, for now, take from child_v2_data
  merged_mri <- merge(child_v2_data$mri_notes, child_v2_data$mri_cams_ff, by = c("participant_id", "session_id"), all = TRUE)

  #### Process stacked anthro data ####

  # Extract parent 2 BMI from household_data and stack
  stacked_parent2_anthro <-
    dplyr::bind_rows(parent_v1_data$household_data[, c("participant_id",
                                                       "parent2_reported_bmi",
                                                       "session_id",
                                                       "demo_child_relationship")],
                     parent_v5_data$household_data[, c("participant_id",
                                                       "parent2_reported_bmi",
                                                       "session_id",
                                                       "demo_child_relationship")])
  # Add parent 2 anthro variables
  ## !! Uncomment line below if/when anthro_data is available in processed_de_data !!
  ## merged_anthro <- merge(processed_de_data$anthro_data, stacked_parent2_anthro, by=c("participant_id", "session_id"), all = TRUE)
  ## !! comment out this line when double entry data is available !!
  merged_anthro <- merge(stacked_visit_anthro_data, stacked_parent2_anthro, by=c("participant_id", "session_id"), all = TRUE)

  # add variables needed to calculate BMI percentiles
  merged_anthro <- dplyr::left_join(merged_anthro, date_data[c("participant_id", "v1_age", "v5_age", "sex")], by = "participant_id") # merge dates and ages from date_data
  merged_anthro <- merged_anthro %>%   # create column 'child_age' based on ages at V1 and V5
    dplyr::mutate(child_age = dplyr::case_when(
      session_id == "ses-1" ~ v1_age,
      session_id == "ses-2" ~ v5_age

    )) %>%
    dplyr::select(-v1_age, -v5_age)  %>% # drop v1_age and v5_age columns
    dplyr::relocate("session_id", .after = 1) #move session_id var after column 1


  # compute bmi variables
  merged_anthro$child_bmi <- round(merged_anthro$child_weight_average / ((merged_anthro$child_height_average / 100) ^ 2), digits = 2)
  merged_anthro$parent1_bmi <- round(merged_anthro$parent1_weight_average_kg / ((merged_anthro$parent1_height_average_cm / 100) ^ 2), digits = 2)
  merged_anthro$child_bmi <- round(merged_anthro$child_weight_average / ((merged_anthro$child_height_average / 100) ^ 2), digits = 2)
  merged_anthro$child_bmi_z <- round(childsds::sds(value = merged_anthro[["child_bmi"]], age = merged_anthro[["child_age"]], sex = merged_anthro[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = "male", female = "female"), digits = 2)
  merged_anthro$child_bmi_p <- round((childsds::sds(value = merged_anthro[["child_bmi"]], age = merged_anthro[["child_age"]], sex = merged_anthro[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "perc", male = "male", female = "female")) * 100, digits = 2)

  # Define parental BMI values and method
  ## parent1_sex ("female" or "male") indicates the parent with measured anthro; demo_child_relationship (0 = bio-mom, 1 = bio-dad) indicates parent that reported height/weight for bio parent *not* at visit in household demo form
  ## parent1_sex and demo_child_relationship should indicate the same parent, but referencing both in ifelse statements in case of scenario where this is not true
  merged_anthro$maternal_anthro_method <- ifelse(merged_anthro$parent1_sex == "female", "measured",
                                                              ifelse(merged_anthro$demo_child_relationship == 1, "reported", NA))

  merged_anthro$maternal_bmi <- ifelse(merged_anthro$maternal_anthro_method == "measured", merged_anthro$parent1_bmi,
                                                    ifelse(merged_anthro$maternal_anthro_method == "reported", merged_anthro$parent2_reported_bmi, NA))

  merged_anthro$paternal_anthro_method <- ifelse(merged_anthro$parent1_sex == "male", "measured",
                                                              ifelse(merged_anthro$demo_child_relationship == 0, "reported", NA))

  merged_anthro$paternal_bmi <- ifelse(merged_anthro$paternal_anthro_method == "measured", merged_anthro$parent1_bmi,
                                                    ifelse(merged_anthro$paternal_anthro_method == "reported", merged_anthro$parent2_reported_bmi, NA))

  #### Generate demographics dataframe  ####

  # combine demo data from demo_data and household form
  demo_data <- merge(parent_v1_data$demo_data, stacked_household[c("session_id", "participant_id", "demo_education_mom", "demo_income")], by = "participant_id", all = TRUE)

  # add dates and ages at start of sessions (V1 and V5) from date_data form

  ## merge dates, ages, and sex from date_data
  demo_data <- dplyr::left_join(demo_data, date_data[c("participant_id", "v1_date", "v5_date", "v1_age", "v5_age", "sex")], by = "participant_id") # merge dates and ages from date_data

  ## create column 'date_session_start' based on dates at V1 and V5
  demo_data <- demo_data %>%
    dplyr::mutate(date_session_start = dplyr::case_when(
      session_id == "ses-1" ~ v1_date,
      session_id == "ses-2" ~ v5_date
    )) %>%
    dplyr::select(-v1_date, -v5_date) # drop date_v1 and date_v1 columns

  ## create column 'child_age' based on ages at V1 and V5
  demo_data <- demo_data %>%
    dplyr::mutate(child_age = dplyr::case_when(
      session_id == "ses-1" ~ v1_age,
      session_id == "ses-2" ~ v5_age

    )) %>%
    dplyr::select(-v1_age, -v5_age) # drop v1_age and v5_age columns


  demo_data <- merge(demo_data, merged_anthro[c("participant_id", "session_id", "child_bmi", "child_bmi_p", "child_bmi_z", "maternal_bmi", "maternal_anthro_method")], by=c("participant_id", "session_id"), all = TRUE)

  # add risk status - compute based on ses-1 maternal_bmi
  ses_1_data <- subset(demo_data, session_id == "ses-1") # subset the dataset to include only session 1 data
  ses_1_data$risk_status_maternal <- ifelse(ses_1_data$maternal_bmi <= 26, "low-risk", ifelse(ses_1_data$maternal_bmi >= 29, "high-risk", NA)) # calculate risk based on maternal bmi
#  ses_1_data$risk_status_both_parents <- ifelse(dplyr::between(ses_1_data$maternal_bmi, 18.5, 26), "low-risk", ifelse(ses_1_data$maternal_bmi >= 29, "high-risk", NA)) # calculate risk based on maternal and paternal
  ses_1_data$child_bmi_criteria <- ifelse(is.na(ses_1_data$child_bmi_p), NA, ifelse(ses_1_data$child_bmi_p < 95, 1,0)) # calculate risk based on maternal bmi
  demo_data <- merge(demo_data, ses_1_data[, c("participant_id", "risk_status_maternal", "child_bmi_criteria")], by = "participant_id", all = TRUE) # merge 'risk_status' variable into demo_data

  # rename columns
  names(demo_data)[names(demo_data) == "demo_ethnicity"] <- "ethnicity"
  names(demo_data)[names(demo_data) == "demo_race"] <- "race"

  # reorder columns

  #### Generate participants dataframe ####

  # add demo_data and date_data (visit dates, visit ages, child sex)
  participants_data <- merge(parent_v1_data$demo_data, date_data, by = "participant_id", all = TRUE)

  # add risk status -- take from demo_data
  participants_data <- merge(participants_data,
                                         demo_data[demo_data$session_id == "ses-1", c("participant_id", "risk_status_maternal")],
                                         by = "participant_id",
                                         all = TRUE)


  # remove birthday and other columns
  participants_data <- participants_data[, -grep("birthdate|timestamp|brief", names(participants_data))]

  # rename columns
  names(participants_data)[names(participants_data) == "demo_ethnicity"] <- "ethnicity"
  names(participants_data)[names(participants_data) == "demo_race"] <- "race"

  names(participants_data)[names(participants_data) == "v1_date"] <- "child_protocol_1_date"
  names(participants_data)[names(participants_data) == "v2_date"] <- "child_protocol_2_date"
  names(participants_data)[names(participants_data) == "v3_date"] <- "child_protocol_3_date"
  names(participants_data)[names(participants_data) == "v4_date"] <- "child_protocol_4_date"
  names(participants_data)[names(participants_data) == "v5_date"] <- "child_protocol_5_date"
  names(participants_data)[names(participants_data) == "v1_age"] <- "child_protocol_1_age"
  names(participants_data)[names(participants_data) == "v2_age"] <- "child_protocol_2_age"
  names(participants_data)[names(participants_data) == "v3_age"] <- "child_protocol_3_age"
  names(participants_data)[names(participants_data) == "v4_age"] <- "child_protocol_4_age"
  names(participants_data)[names(participants_data) == "v5_age"] <- "child_protocol_5_age"

  # make column child_protocol_order (e.g., 13425) based on order of child protocol dates - only include visits that have dates (i.e., occurred)

  ## define function to get order of dates for each row
  get_order <- function(row) {

    # specify date columns
    date_cols <- c("child_protocol_1_date", "child_protocol_2_date", "child_protocol_3_date", "child_protocol_4_date", "child_protocol_5_date")

    # get number of missing visits (date cols with NA)
    n_na <- sum(is.na(row[date_cols]))

    # get order of dates w/ missing visits at end, collapse integers into single string
    order_all_visits <- paste0(order(row[date_cols], na.last = TRUE), collapse = '')

    # remove last n_na characters from order_all_visits, will yeild a string with length = number of visits attended
    stringr::str_sub(order_all_visits, end=-(n_na + 1))
  }

  ## apply function to get visit order
  participants_data$visit_protocol_order <- apply(participants_data, 1, get_order)

  # reorder columns
  participants_data <-
    participants_data %>%
    dplyr::relocate("risk_status_maternal", .after = 1) %>% #move risk_status var after column 1
    dplyr::relocate("sex", .after = 2) %>% #move sex var after column 2
    dplyr::select(-dplyr::contains("date")) %>% #remove date columns from participants_data
    dplyr::bind_cols(participants_data %>% dplyr::select(dplyr::contains("date"))) # Bind date columns to end of participants_data

  #### Export Data ####

  # make a list dataframes to export, where the name is the corresponding json function without json_)
  data_to_export <- list(

    participants = participants_data,

    # single visit questionnaires
    infancy = parent_v1_data$infancy_data,
    cfq = parent_v1_data$cfq_data$bids_phenotype,
    efcr = parent_v1_data$efcr_data$bids_phenotype,
    lbc = parent_v1_data$lbc_data$bids_phenotype,
    pss = parent_v1_data$pss_data$bids_phenotype,
    chaos = parent_v1_data$chaos_data$bids_phenotype,
    brief2 = parent_v2_data$brief_data$bids_phenotype,
    bes = parent_v2_data$bes_data$bids_phenotype,
    ffbs = parent_v2_data$ffbs_data$bids_phenotype,
#    fsq = parent_v2_data$fsq_data$bids_phenotype, # not in bids_phenotype yet
    fsq = parent_v2_data$fsq_data,
    spsrq = parent_v3_data$spsrq_data$bids_phenotype,
    pwlb = parent_v3_data$pwlb_data$bids_phenotype,
    tfeq = parent_v3_data$tfeq_data$bids_phenotype,
    bisbas = parent_v3_data$bisbas_data$bids_phenotype,
    debq = parent_v3_data$debq_data$bids_phenotype,
    scpf = parent_v3_data$scpf_data$bids_phenotype,
    hfssm = parent_v4_data$hfssm_data$bids_phenotype,
    cchip = parent_v4_data$cchip_data$bids_phenotype,
    hfias = parent_v4_data$hfias_data$bids_phenotype,
    fhfi = parent_v4_data$fhfi_data, # not in bids_phenotype yet
    sleeplog = child_v3_data$sleeplog_data, # not in bids_phenotype yet
    pptq = child_v4_data$pptq_data$bids_phenotype,
    sic = child_v4_data$sic_data,

    # stacked questionnaires
    stq = stacked_stq,
    kbas = stacked_kbas,
    household = stacked_household,
    cebq = stacked_cebq,
    cbq = stacked_cbq,
    stq = stacked_stq,
    cshq = stacked_cshq,
    pstca = stacked_pstca,
    audit = stacked_audit,
    pmum = stacked_pmum,
    cfpq = stacked_cfpq,
    rank = stacked_rank,
    puberty = stacked_puberty,
    loc = stacked_loc,
    class = stacked_class,

    # non-questionnaire data
    demographics = demo_data,
    anthropometrics = merged_anthro,
    intake = merged_intake,
    mri_visit = merged_mri,
    dexa = processed_de_data$dexa_data

  )

  # loop through data_to_export and export data and meta-data
  for (i in 1:length(data_to_export)) {

    # Get the dataframe
    df <- data_to_export[[i]]

    # get the phenotype name
    phenotype_name <- names(data_to_export)[i]

    # define json function name
    json_func_name = paste0("json_", phenotype_name)

    # Get the json function by name
    json_func <- get(json_func_name)

    # Call the function
    json <- json_func()

    # add extensions to phenotype_name
    if (phenotype_name == "participants") {
      # export participants into bids_wd
      filename_tsv <- paste0(bids_wd, slash, "participants.tsv")
      filename_json <- paste0(bids_wd, slash, "participants.json")
    } else {
      # export phenotype data into phenotype_wd
      filename_tsv <- paste0(phenotype_wd, slash, phenotype_name, ".tsv")
      filename_json <- paste0(phenotype_wd, slash, phenotype_name, ".json")
    }

    # write tsv
    if ( isTRUE(overwrite) | !file.exists(filename_tsv) ) {
      write.table(
        df,
        filename_tsv,
        quote = FALSE,
        sep = '\t',
        col.names = TRUE,
        row.names = FALSE,
        na = "n/a" # use 'n/a' for missing values for BIDS compliance
      )
    }

    # write json
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(json, filename_json)
    }
  }


  #### Return Data ####
  if (isTRUE(return_data)) {
    return(list(
      input_data = list(visit_data = redcap_visit_data,
                        de_data = redcap_de_data),
      visit_data = list(
        child_v1_data = child_v1_data,
        child_v2_data = child_v2_data,
        child_v3_data = child_v3_data,
        child_v4_data = child_v4_data,
        child_v5_data = child_v5_data,
        parent_v1_data = parent_v1_data,
        parent_v2_data = parent_v2_data,
        parent_v3_data = parent_v3_data,
        parent_v4_data = parent_v4_data,
        parent_v5_data = parent_v5_data
      ),
      double_entry_data = processed_de_data,
      phenotype_data = data_to_export
    ))
  }

}

