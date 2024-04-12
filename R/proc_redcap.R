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
#' \dontrun{
#' data_de_path = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids/sourcedata/phenotype/REACHDataDoubleEntry_DATA_2024-03-12_1045.csv"
#' visit_data_path = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids/sourcedata/phenotype/FoodMarketingResilie_DATA_2024-03-22_1446.csv"
#'
#' phenotype_data <- proc_redcap(visit_data_path, data_de_path, return = TRUE)
#'
#' }
#'
#' @importFrom utils tail write.table read.csv
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


  #### Load and organize visit data ####
  redcap_visit_data <- read.csv(visit_data_path, header = TRUE)

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
  date_data[['v1_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v1_date']])/lubridate::years(1), 1)
  date_data[['v2_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v2_date']])/lubridate::years(1), 1)
  date_data[['v3_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v3_date']])/lubridate::years(1), 1)
  date_data[['v4_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v4_date']])/lubridate::years(1), 1)
  date_data[['v5_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['v5_date']])/lubridate::years(1), 1)
  date_data[['brief_age']] <- round(lubridate::interval(date_data[['demo_child_birthdate']], date_data[['brief_date']])/lubridate::years(1),1)

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

  # not scored/in bids_phenotype yet
  # stacked_cshq <- dplyr::bind_rows(parent_v2_data$cshq_data$bids_phenotype, parent_v5_data$cshq_data$bids_phenotype)

  # not scored/in bids_phenotype yet
  # stacked_pstca <- dplyr::bind_rows(parent_v3_data$pstca_data$bids_phenotype, parent_v5_data$pstca_data$bids_phenotype)

  stacked_audit <-
    dplyr::bind_rows(
      parent_v4_data$audit_data$bids_phenotype,
      parent_v5_data$audit_data$bids_phenotype)

  # not scored/in bids_phenotype yet
  # stacked_pmum <- dplyr::bind_rows(parent_v4_data$pmum_data$bids_phenotype, parent_v5_data$pmum_data$bids_phenotype)

  stacked_cfpq <-
    dplyr::bind_rows(
      parent_v4_data$cfpq_data$bids_phenotype,
      parent_v5_data$cfpq_data$bids_phenotype)

  # not scored/in bids_phenotype yet - will it be?
  # stacked_rank <- dplyr::bind_rows(parent_v1_data$rank_data$bids_phenotype, parent_v5_data$rank_data$bids_phenotype)

  # not scored/in bids_phenotype yet - will it be?
  # stacked_class <- dplyr::bind_rows(parent_v3_data$class_data$bids_phenotype, parent_v5_data$class_data$bids_phenotype)

  # stack child (V5) and parent (V1, V5) puberty data
  stacked_puberty <- dplyr::bind_rows(
    transform(parent_v1_data$puberty_data$bids_phenotype, respondent = "parent"),
    transform(parent_v5_data$puberty_data$bids_phenotype, respondent = "parent"),
    transform(child_v5_data$puberty_data$bids_phenotype, respondent = "child"))

  # note: visit column is vas_visit_protocol -- this data will merged with intake data by session_id only
  stacked_meal_vas_data <- dplyr::bind_rows(
    transform(child_v1_data$meal_vas_data, vas_visit_protocol = "1"),
    transform(child_v5_data$meal_vas_data, vas_visit_protocol = "5")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("vas_visit_protocol", .after = 2)

  # note: visit column is vas_visit_protocol -- this data will merged with intake data by session_id only
  stacked_eah_vas_data <- dplyr::bind_rows(
    transform(child_v1_data$eah_vas_data, vas_visit_protocol = "1"),
    transform(child_v5_data$eah_vas_data, vas_visit_protocol = "5")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("vas_visit_protocol", .after = 2)

  stacked_meal_data <- dplyr::bind_rows(
    transform(child_v1_data$meal_data, visit_protocol = "1"),
    transform(child_v3_data$meal_data, visit_protocol = "3"),
    transform(child_v4_data$meal_data, visit_protocol = "4"),
    transform(child_v5_data$meal_data, visit_protocol = "5")
    ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  stacked_eah_data <- dplyr::bind_rows(
    transform(child_v3_data$eah_data, visit_protocol = "3"),
    transform(child_v4_data$eah_data, visit_protocol = "4"),
    transform(child_v5_data$eah_data, visit_protocol = "5")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  stacked_parent2_anthro <-
    dplyr::bind_rows(parent_v1_data$household_data[, c("participant_id",
                                                       "parent2_reported_bmi",
                                                       "session_id",
                                                       "household_form_date")],
                     parent_v5_data$household_data[, c("participant_id",
                                                       "parent2_reported_bmi",
                                                       "session_id",
                                                       "household_form_date")])

  stacked_updates <- dplyr::bind_rows(
    parent_v2_data$visit_data_parent,
    parent_v3_data$visit_data_parent,
    parent_v4_data$visit_data_parent,
    parent_v5_data$visit_data_parent
  )

  #### Add to participants_data ####

  # add sex
  participants_data <- merge(parent_v1_data$participants_data, parent_v1_data$puberty_data$bids_phenotype[, c("participant_id", "sex")], by = "participant_id", all = TRUE)

  # add maternal edu and income from visit 1 and append "v1" to variable names
  participants_data <- merge(participants_data,
                             parent_v1_data$household_data[, c("participant_id", "demo_education_mom", "demo_income")],
                             by = "participant_id",
                             all = TRUE) %>% dplyr::rename(demo_education_mom_v1 = .data$demo_education_mom, demo_income_v1 = .data$demo_income)

  # add maternal edu and income from visit 5 and append "v5" to variable names
  participants_data <- merge(participants_data,
                             parent_v5_data$household_data[, c("participant_id", "demo_education_mom", "demo_income")],
                             by = "participant_id",
                             all = TRUE) %>% dplyr::rename(demo_education_mom_v5 = .data$demo_education_mom, demo_income_v5 = .data$demo_income)

  # add date_data (visit dates and ages)
  participants_data <- merge(participants_data, date_data, by=c("participant_id","sex"), all = TRUE)

  # add maternal BMI
  # add risk status

  ## bmi and bmi-z

  # remove birthday and other columns
  participants_data <- participants_data[, -grep("birthdate|timestamp|brief", names(participants_data))]

  # rename columns
  names(participants_data) <- gsub('demo_', '', names(participants_data))
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

  # reorder columns

  #### Merge visit intake (meal, EAH, vas) data ####
  merged_vas_data <- merge(stacked_eah_vas_data, stacked_meal_vas_data, by=c("participant_id","session_id", "vas_visit_protocol"), all = TRUE)
  merged_intake <- merge(stacked_meal_data, stacked_eah_data, by=c("participant_id","visit_protocol", "session_id", "advertisement_condition"), all = TRUE)
  merged_intake <- merge(merged_intake, merged_vas_data, by=c("participant_id", "session_id"), all = TRUE)

  #### Merge visit data and double entry (de) data ####

  # de anthro_data with stacked_parent2_anthro (data from household_data)
  merged_anthro <- merge(processed_de_data$anthro_data$anthro_long, stacked_parent2_anthro, by=c("participant_id", "session_id"), all = TRUE)

  # merge intake data
  merged_intake <- merge(merged_intake, processed_de_data$stacked_intake, by=c("participant_id","visit_protocol", "session_id"), all = TRUE)

  # merge notes/visit data? update data?

  # merge MRI visit data double entry CAMS / MRI freddies
  merged_mri <- merge(child_v2_data$mri_notes, processed_de_data$mri_visit_data, by = "participant_id", all = TRUE)


  #### Export Data ####
#
#   # write participant.tsv
#   participants_tsv <- paste0(bids_wd, slash, "participants.tsv")
#
#   if ( isTRUE(overwrite) | !file.exists(participants_tsv) ) {
#     # write tsv
#     write.table(
#       participants_data,
#       participants_tsv,
#       quote = FALSE,
#       sep = '\t',
#       col.names = TRUE,
#       row.names = FALSE
#     )
#   }
#
#   # write participant.json
#   participants_json <- paste0(bids_wd, slash, "participants.json")
#
#   if ( isTRUE(overwrite) | !file.exists(participants_json) ) {
#     # write json
#     write(json_participants(), participants_json)
#   }

  # write phenotype data (tsv and json files)

  # generate phenotype_wd if it doesnt exist
  if (!file.exists(phenotype_wd)){
    dir.create(file.path(phenotype_wd))
  }

  # make a list of lists including dataframe, and export name (without extension)
  data_to_export <- list(

    # # single visit data
    # list(parent_v1_data$birth_data, "birth_data"), # no json yet
    list(parent_v1_data$cfq_data$bids_phenotype, "cfq"),
    list(parent_v1_data$efcr_data$bids_phenotype, "efcr"),
    list(parent_v1_data$lbc_data$bids_phenotype, "lbc"),
    list(parent_v1_data$pss_data$bids_phenotype, "pss"),
    # list(parent_v1_data$chaos_data$bids_phenotype, "chaos"), # not in bids_phenotype yet

    list(parent_v2_data$brief_data$bids_phenotype, "brief2"),
    list(parent_v2_data$bes_data$bids_phenotype, "bes"),
    list(parent_v2_data$ffbs_data$bids_phenotype, "ffbs"),
    # list(parent_v2_data$ffq_data$bids_phenotype, "ffq"), # not in bids_phenotype yet

    list(parent_v3_data$spsrq_data$bids_phenotype, "spsrq"),
    list(parent_v3_data$pwlb_data$bids_phenotype, "pwlb"),
    list(parent_v3_data$tfeq_data$bids_phenotype, "tfeq"),
    list(parent_v3_data$bisbas_data$bids_phenotype, "bisbas"),
    list(parent_v3_data$debq_data$bids_phenotype, "debq"),
    list(parent_v3_data$scpf_data$bids_phenotype, "scpf"),

    list(child_v4_data$pptq_data$bids_phenotype, "pptq"),

    # stacked visit data
    list(stacked_stq, "stq"),
    list(stacked_kbas, "kbas"),
    list(stacked_household, "household"),
    list(stacked_cebq, "cebq"),
    list(stacked_cbq, "cbq"),
    list(stacked_stq, "stq"),
    # list(stacked_cshq, "cshq"), # not in bids_phenotype yet
    # list(stacked_pstca, "pstca"), # not in bids_phenotype yet
    list(stacked_audit, "audit"),
    # list(stacked_pmum, "pmum"), # not in bids_phenotype yet
    list(stacked_cfpq, "cfpq"),
    # list(stacked_rank, "rank"),  # not in bids_phenotype yet
    list(stacked_puberty, "puberty"),
    list(stacked_loc, "loc"),

    # merged data
    list(merged_anthro, "anthropometrics"),
    list(merged_intake, "intake"),
    list(merged_mri, "mri_visit"),

    # double entry data
    list(processed_de_data$dexa_data, "dexa")

  )


  for (i in 1:length(data_to_export)) {

    # Get the dataframe
    df <- data_to_export[[i]][[1]]

    # get the phenotype name
    phenotype_name <- data_to_export[[i]][[2]]

    # define json function name
    json_func_name = paste0("json_", phenotype_name)

    # Get the json function by name
    json_func <- get(json_func_name)

    # Call the function
    json <- json_func()

    # add extensions to phenotype_name
    filename_tsv <- paste0(phenotype_wd, slash, phenotype_name, ".tsv")
    filename_json <- paste0(phenotype_wd, slash, phenotype_name, ".json")

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
                 ))
  }
}

