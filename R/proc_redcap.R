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
#' @param overwrite overwrite existing files (default = FALSE)
#' @param return_data return phenotype to console (default = FLASE)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean raw phenotype datasets for each task
#'  2) meta-data/.json inforamtion for each task
#'
#' @examples
#' visit_data_path = "/Users/baf44/projects/Keller_Marketing/ParticipantData/bids/sourcedata/phenotype/FoodMarketingResilie_DATA_2024-02-16_1544.csv"
#'
#' phenotype_data <- proc_redcap(visit_data_path, return = TRUE)
#'
#' \dontrun{
#' }
#'
#' @importFrom utils tail write.csv read.csv
#' @export

proc_redcap <- function(visit_data_path, overwrite = FALSE, return_data = FALSE) {

  # For testing
  # visit_data_path = "/Users/baf44/Keller_Marketing/ParticipantData/bids/sourcedata/phenotype/FoodMarketingResilie_DATA_2024-02-16_1544.csv"

  #### 1. Set up/initial checks #####

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

  # if (!grep('.csv', data_de_path)) {
  #   data_de_file <- paste0(data_de_path, '.csv')
  # } else {
  #   data_de_file <- data_de_path
  # }

  # check file existis
  if (!file.exists(visit_data_file)) {
    stop ('entered visit_data_path is not an existing file - be sure it is entered as a string and contains the full data path and file name')
  }

  # if (!file.exists(data_de_file)) {
  #   stop ('entered data_de_path is not an existing file - be sure it is entered as a string and contains the full data path and file name')
  # }




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

  # process visit data ####
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

  # get visit 1 date data - needed for util_redcap_parent_v1()
  v1_date_data <- child_visit_1_arm_1[, c("record_id", "v1_date")]
  names(v1_date_data)[names(v1_date_data) == "record_id"] <- "participant_id"

  # get age and sex data - needed for util_redcap_parent_v2()
  agesex_data <- parent_visit_1_arm_1[, c("record_id", "prs_sex", "demo_child_birthdate")]
  agesex_data <- merge(agesex_data, child_visit_2_arm_1[, c("record_id", "v2_date")], by = "record_id")
  agesex_data[['demo_child_birthdate']] <- lubridate::as_date(agesex_data[['demo_child_birthdate']])
  agesex_data[['v2_date']] <- lubridate::as_date(agesex_data[['v2_date']])
  agesex_data[['v2_age']] <- lubridate::interval(agesex_data[['demo_child_birthdate']], agesex_data[['v2_date']])/lubridate::years(1)
  agesex_data <- agesex_data[, c("record_id", "prs_sex", "v2_age")]
  colnames(agesex_data) <- c("participant_id", "sex", "age")

  # get visit 5 date data - needed for util_redcap_parent_v5()
  v5_date_data <- child_visit_5_arm_1[, c("record_id", "v5_date")]
  v5_date_data <- merge(v5_date_data, parent_visit_1_arm_1[, c("record_id", "demo_child_birthdate")], by = "record_id")
  names(v5_date_data)[names(v5_date_data) == "record_id"] <- "participant_id"

  # # organize event data
  child_v1_data <- util_redcap_child_v1(child_visit_1_arm_1)
  parent_v1_data <- util_redcap_parent_v1(parent_visit_1_arm_1, v1_date_data = v1_date_data)
  child_v2_data <- util_redcap_child_v2(child_visit_2_arm_1)
  parent_v2_data <- util_redcap_parent_v2(parent_visit_2_arm_1, agesex_data = agesex_data)
  child_v3_data <- util_redcap_child_v3(child_visit_3_arm_1)
  parent_v3_data <- util_redcap_parent_v3(parent_visit_3_arm_1)
  child_v4_data <- util_redcap_child_v4(child_visit_4_arm_1)
  parent_v4_data <- util_redcap_parent_v4(parent_visit_4_arm_1)
  child_v5_data <- util_redcap_child_v5(child_visit_5_arm_1)
  # parent_v5_data <- util_redcap_parent_v5(parent_visit_5_arm_1, v1_date_data = v1_date_data)


  # check cog task completion for RPPR ####

  sum(child_visit_1_arm_1$toolbox_age_7_flanker_check == 1 | child_visit_1_arm_1$toolbox_age_8_flanker_check == 1, na.rm = TRUE)
  sum(child_visit_1_arm_1$toolbox_age_7_dccs_check == 1 | child_visit_1_arm_1$toolbox_age_8_dccs_check == 1, na.rm = TRUE)
  sum(child_visit_1_arm_1$toolbox_list_sorting_check == 1, na.rm = TRUE)
  sum(child_visit_3_arm_1$spaec_game_check == 1, na.rm = TRUE)
  sum(child_visit_5_arm_1$toolbox_list_sorting_check == 1, na.rm = TRUE)
  sum(child_visit_5_arm_1$toolbox_age_8_dccs_check == 1, na.rm = TRUE)
  sum(child_visit_5_arm_1$toolbox_age_8_flanker_check == 1, na.rm = TRUE)
  sum(child_visit_3_arm_1$pit_task_completed_check == 1, na.rm = TRUE)
  sum(child_visit_5_arm_1$pit_task_completed_check == 1, na.rm = TRUE)


  # #### Load and organize double-entry data ####
  # redcap_de_data <- read.csv(data_de_path, header = TRUE)
  #
  # # all validated so can just take reviewer 1 data
  # redcap_de_data <- redcap_de_data[grepl('--1', redcap_de_data[['record_id']]), ]
  #
  #
  #
  # # merge
  # participant_data <- merge(prepost_v1_data$prepost_data$data, prepost_v2_data$data, by = 'participant_id')
  # participant_data <- merge(participant_data, child_v1_data$child_visit1_data$data, by = 'participant_id')
  # participant_data <- merge(participant_data, child_v2_data$child_visit2_data$data, by = 'participant_id')
  # participant_data <- merge(participant_data, child_v2_data$loc_data$data, by = 'participant_id')
  # participant_data <- merge(participant_data, parent_v1_data$demo_data$data, by = 'participant_id')
  #
  # participant_data$participant_id <- as.numeric(participant_data$participant_id)
  #
  # double_enter_data$bodpod_data$data$participant_id <- as.numeric(double_enter_data$bodpod_data$data$participant_id)
  # #### Load and organize data double entry ####
  # redcap_de_data <- read.csv(data_de_path, header = TRUE)
  #
  # double_enter_data <- util_redcap_de(redcap_de_data)
  # participant_data <- merge(participant_data, double_enter_data$bodpod_data$data, by = 'participant_id')
  #
  #
  # #### Merge/stack visit data/notes ###

    # merge demo data?? parent-reported parent2 height and weight are in household_data -- BMI not automatically calculated for parent2 v5 in child form
    # merge dexa_notes with dexa_data? task notes with task data?

  # merge intake data

  # merge notes/visit data?

  # stack data collected on 2 visits

  # Combine the datasets with an additional "visit" column, move "visit" to column 2

  stacked_anthro <- dplyr::bind_rows(
    transform(child_v1_data$anthro_data, visit = "1"),
    transform(child_v5_data$anthro_data, visit = "5")
  ) %>% dplyr::relocate(visit, .after = 1)

  stacked_stq <- dplyr::bind_rows(
    transform(child_v1_data$stq_data, visit = "1"),
    transform(child_v5_data$stq_data, visit = "5")
  ) %>% dplyr::relocate(visit, .after = 1)

  stacked_kbas <- dplyr::bind_rows(
    transform(child_v1_data$kbas_data, visit = "1"),
    transform(child_v5_data$kbas_data, visit = "5")
  ) %>% dplyr::relocate(visit, .after = 1)

  # stacked_cebq <- dplyr::bind_rows(
  #   mutate(parent_v1_data$cebq_data$bids_phenotype, visit = "1"),
  #   mutate(parent_v5_data$cebq_data$bids_phenotype, visit = "5")
  # ) %>% dplyr::relocate(visit, .after = 1)

  # stacked_cebq <- rbind(parent_v1_data$cebq_data$bids_phenotype, parent_v5_data$cebq_data$bids_phenotype)
  # stacked_cbq <- rbind(parent_v2_data$cbq_data$bids_phenotype, parent_v5_data$cbq_data$bids_phenotype)
  # stacked_cshq <- rbind(parent_v2_data$cshq_data$bids_phenotype, parent_v5_data$cshq_data$bids_phenotype)
  # stacked_audit <- rbind(parent_v4_data$audit_data$bids_phenotype, parent_v5_data$audit_data$bids_phenotype)
  # stacked_pstca <- rbind(parent_v3_data$pstca_data$bids_phenotype, parent_v5_data$pstca_data$bids_phenotype)
  # stacked_pmum <- rbind(parent_v4_data$pmum_data$bids_phenotype, parent_v5_data$pmum_data$bids_phenotype)
  # stacked_cfpq <- rbind(parent_v4_data$cfpq_data$bids_phenotype, parent_v5_data$cfpq_data$bids_phenotype)
# stacked_rank <- rbind(parent_v1_data$rank_data$bids_phenotype, parent_v5_data$rank_data$bids_phenotype) # will this bids_phenotype/scored?
# stacked_puberty <- rbind(parent_v1_data$cebq_data$bids_phenotype, parent_v5_data$cebq_data$bids_phenotype) # will this be bids_phenotype/scored
# stacked_class <-
# stacked_household <-
# stacked_demo <-
# stacked_loc <-



  # #### Export Phenotype Data ####
  #

  # generate phenotype_wd if it doesnt exist
  if (!file.exists(phenotype_wd)){
    dir.create(file.path(phenotype_wd))
  }

  # export forms collected at 1 visit only (i.e., not stacked data) -- can this be done using a list of lists and a loop?

  write.csv(parent_v1_data$cfq_data$bids_phenotype, paste0(phenotype_wd, slash, 'cfq.tsv'), row.names = FALSE) # cfq
  write.csv(parent_v1_data$efcr_data$bids_phenotype, paste0(phenotype_wd, slash, 'efcr.tsv'), row.names = FALSE) #efcr
  write.csv(parent_v1_data$lbc_data$bids_phenotype, paste0(phenotype_wd, slash, 'lbc.tsv'), row.names = FALSE) #lbc
  # write.csv(parent_v1_data$pss_data$bids_phenotype, paste0(phenotype_wd, slash, 'pss.tsv'), row.names = FALSE) # pss -- will this be bids_phenotype

  # write.csv(parent_v2_data$brief_data$bids_phenotype, paste0(phenotype_wd, slash, 'brief.tsv'), row.names = FALSE) #brief
  # write.csv(parent_v2_data$bes_data$bids_phenotype, paste0(phenotype_wd, slash, 'bes.tsv'), row.names = FALSE) #bes
  # write.csv(parent_v2_data$ffbs_data$bids_phenotype, paste0(phenotype_wd, slash, 'ffbs.tsv'), row.names = FALSE) #ffbs
  # write.csv(parent_v2_data$ffq_data$bids_phenotype, paste0(phenotype_wd, slash, 'ffq.tsv'), row.names = FALSE) #ffq -- will this be bids_phenotype

  write.csv(parent_v3_data$spsrq_data$bids_phenotype, paste0(phenotype_wd, slash, 'spsrq.tsv'), row.names = FALSE) # spsrq
  write.csv(parent_v3_data$pwlb_data$bids_phenotype, paste0(phenotype_wd, slash, 'pwlb.tsv'), row.names = FALSE) # pwlb
  write.csv(parent_v3_data$tfeq_data$bids_phenotype, paste0(phenotype_wd, slash, 'tfeq.tsv'), row.names = FALSE) # tfeq
  write.csv(parent_v3_data$bisbas_data$bids_phenotype, paste0(phenotype_wd, slash, 'bisbas.tsv'), row.names = FALSE) # bisbas
  write.csv(parent_v3_data$debq_data$bids_phenotype, paste0(phenotype_wd, slash, 'debq.tsv'), row.names = FALSE) # debq
  # write.csv(parent_v3_data$scpf_data$bids_phenotype, paste0(phenotype_wd, slash, 'debq.tsv'), row.names = FALSE) # scpf -- will this be bids_phenotype

  # export stacked dataframes

  write.csv(stacked_anthro, paste0(phenotype_wd, slash, 'anthropometrics.tsv'), row.names = FALSE)
  write.csv(stacked_stq, paste0(phenotype_wd, slash, 'stq.tsv'), row.names = FALSE)
  write.csv(stacked_kbas, paste0(phenotype_wd, slash, 'kbas.tsv'), row.names = FALSE)
  # write.csv(stacked_cebq, paste0(phenotype_wd, slash, 'cebq.tsv'), row.names = FALSE) #cebq
  # write.csv(stacked_cbq, paste0(phenotype_wd, slash, 'cebq.tsv'), row.names = FALSE) #cbq
  # write.csv(stacked_cshq, paste0(phenotype_wd, slash, 'cebq.tsv'), row.names = FALSE) #cshq
  # ...

  # Call function to export all jsons -- can add input arg that only outputs jsons if overwritejsons = TRUE

  if (isTRUE(return_data)){
    return(list( child_v1_data = child_v1_data,
                 parent_v1_data = parent_v1_data,
                 parent_v2_data = parent_v2_data,
                 parent_v3_data = parent_v3_data,
                 parent_v4_data = parent_v4_data
                 ))
  }
}

