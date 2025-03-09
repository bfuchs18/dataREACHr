#' util_redcap_de: Organize double-entry data from REDCap (called within proc_redcap.R)
#'
#' This function organizes REDCap double entry data data
#'
#' @importFrom rlang .data
#' @param data double-entry data
#' @param return_data If return_data is set to TRUE, will return a list including: dexa_data, intake_data
#'

util_redcap_de <- function(data, return_data = TRUE) {

  #### Set up/initial checks #####

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

  ## Extract merged and unmerged data ####

  # get logical vector of rows that contain unmerged data (contain "--" in participant_id)
  unmerged_rows <- grepl("--", data$participant_id)

  # subset unmerged rows only
  unmerged_data <- data[unmerged_rows, ]

  # subset merged data only (remove rows with "--")
  merged_data <- data[!unmerged_rows, ]

  # Make ID column bids compliant: add "sub_"
  merged_data$participant_id <- paste0("sub-", merged_data$participant_id)

  ## define function to compare unmerged data (i.e., do the double-entry verification) ----
  compare_de <- function(unmerged_data) {

    # get list of IDs
    de_ids <- unmerged_data$participant_id
    ids <- unique(gsub("--1|--2","",de_ids))

    verified_data_list <- list()
    discrepancy_log <- list()

    for (i in 1:length(ids)) {

      id = ids[i]

      # subset participant rows
      sub_de_data <- unmerged_data[grepl(id, unmerged_data$participant_id),]

      # extract entries without participant_id
      entry_1 <- sub_de_data[1,!names(sub_de_data) %in% c("participant_id")]
      entry_2 <- sub_de_data[2,!names(sub_de_data) %in% c("participant_id")]

      # if rows are equal
      if ( isTRUE(all.equal(entry_1, entry_2, check.attributes = FALSE)) ) {

        # add entry 1 row to list
        verified_data_list[[i]] <- sub_de_data[1,]

      } else {
        print(paste("sub", id, "double entry discrepancy. Check discrepancy_log"))

        # add discrepancy to discrepancy_log
        discrepancy_log[[length(discrepancy_log) + 1]] <- paste("ID:", id, "Discrepancy:", all.equal(entry_1, entry_2, check.attributes = FALSE))
      }

    }

    # create verified bids dataframe
    verified_data <- dplyr::bind_rows(verified_data_list)
    verified_data$participant_id <- gsub("--1", "", verified_data$participant_id)
    verified_data$participant_id <- paste0("sub-", verified_data$participant_id)

    return(list(verified_data = verified_data, discrepancy_log = discrepancy_log))
  }

  ## DEXA data ####

  # visit 1 data
  dexa_v1_unmerged <- unmerged_data[, grep("participant_id|dxa_scan_date_v1|dxa_sex_v1|dxa_height_v1|dxa_weight_v1|dxa_age_v1|^left.*v1$|right.*v1$|^v1.*v1$", names(unmerged_data))] # column identifiers: (1) Starts with "dxa", ends with "v1", (2) Starts with "left", ends with "v1", (3) Starts with "left", ends with "v1", (4) starts with "v1", ends with "v1"
  colnames(dexa_v1_unmerged) <- gsub("^v1_|_v1$", "", colnames(dexa_v1_unmerged)) # Remove "v1_" and "_v1" from column names
  dexa_v1_unmerged <- dexa_v1_unmerged %>% dplyr::mutate(across(-c(participant_id, dxa_scan_date,), as.numeric)) # convert cols to numeric

  # visit 5 data
  dexa_v5_unmerged <- unmerged_data[, grep("participant_id|dxa_scan_date_v5|dxa_sex_v5|dxa_height_v5|dxa_weight_v5|dxa_age_v5|^left.*v5$|right.*v5$|^v1.*v5$", names(unmerged_data))] # column identifiers: (1) Starts with "dxa", ends with "v5", (2) Starts with "left", ends with "v5", (3) Starts with "left", ends with "v5", (4) starts with "v1", ends with "v5
  colnames(dexa_v5_unmerged) <- gsub("^v1_|_v5$", "", colnames(dexa_v5_unmerged)) # Remove "v1_" and "_v5" from column names
  dexa_v5_unmerged <- dexa_v5_unmerged %>% dplyr::mutate(across(-c(participant_id, dxa_scan_date,), as.numeric)) # convert cols to numeric

  # run comparison function
  compared_v1_dexa <- compare_de(dexa_v1_unmerged)
  compared_v5_dexa <- compare_de(dexa_v5_unmerged)

  # extract verified dataframes
  dexa_v1_data <- compared_v1_dexa$verified_data
  dexa_v5_data <- compared_v5_dexa$verified_data

  # stack visit 1 and visit 5 data, add "visit_protocol" and "session_id" columns and reorder
  stacked_dexa <- dplyr::bind_rows(
    transform(dexa_v1_data, visit_protocol = "1", session_id = "ses-1"),
    transform(dexa_v5_data, visit_protocol = "5", session_id = "ses-2")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)


  # update column names -- mostly to match names from food and brain to facilitate compiling

  # add "dexa" prefix to all cols except "participant_id", "session_id", "visit_protocol" and cols that already start with "dxa"
  names(stacked_dexa) <- ifelse(names(stacked_dexa) %in% c("participant_id", "session_id", "visit_protocol", grep("^dxa", names(stacked_dexa), value = TRUE)),
                                names(stacked_dexa),
                                paste0("dxa_", names(stacked_dexa)))
  names(stacked_dexa) <- gsub("left", "l", names(stacked_dexa))
  names(stacked_dexa) <- gsub("right", "r", names(stacked_dexa))
  names(stacked_dexa) <- gsub("_am", "_ptile", names(stacked_dexa))
  names(stacked_dexa) <- gsub("_am", "_ptile", names(stacked_dexa))
  names(stacked_dexa) <- gsub("fat_trunk_over_leg", "percfat_trunk_legs_ratio", names(stacked_dexa))
  names(stacked_dexa) <- gsub("lean_over_height", "lean_height_ratio", names(stacked_dexa))
  names(stacked_dexa) <- gsub("fat_mass_over_height", "fatmass_height_ratio", names(stacked_dexa))
  names(stacked_dexa) <- gsub("trunk_over_limb_fat", "fatmass_trunk_limb_ratio", names(stacked_dexa)) # name wont match Food and Brain which uses leg instead of limb for these vars, but limb is more accurate based on description
  names(stacked_dexa) <- gsub("z_score", "zscore", names(stacked_dexa))
  names(stacked_dexa) <- gsub("lean_and_bmc", "lean_bmc_comb", names(stacked_dexa))


  ## intake data ####

  intake_data <- merged_data[, grep("participant_id|bread|butter|cheese|tender|carrot|chips|fruit|water|ranch|ketchup|meal|brownie|corn_chip|kiss|ice_cream|oreo|popcorn|pretzel|skittle|starburst|eah", names(merged_data))]
  intake_data <- intake_data[, -grep("complete|notes|intake_eah_visit_number|consumed|ad_cond", names(intake_data))]
  colnames(intake_data) <- gsub("freddy", "fullness", colnames(intake_data)) # Replace "freddy" with "fullness" in colnames

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

  # make all values numeric except column 1 (participant_id)
  v1_intake_data <- dplyr::mutate_at(v1_intake_data, -1, function(x) as.numeric(as.character(x)))
  v3_intake_data <- dplyr::mutate_at(v3_intake_data, -1, function(x) as.numeric(as.character(x)))
  v4_intake_data <- dplyr::mutate_at(v4_intake_data, -1, function(x) as.numeric(as.character(x)))
  v5_intake_data <- dplyr::mutate_at(v5_intake_data, -1, function(x) as.numeric(as.character(x)))

  # stack intake data
  stacked_intake <- dplyr::bind_rows(
    transform(v1_intake_data, visit_protocol = "1", session_id = "ses-1"),
    transform(v3_intake_data, visit_protocol = "3", session_id = "ses-1"),
    transform(v4_intake_data, visit_protocol = "4", session_id = "ses-1"),
    transform(v5_intake_data, visit_protocol = "5", session_id = "ses-2")
  ) %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("visit_protocol", .after = 2)

  # compute intake variables
  stacked_intake <- util_calc_intake(stacked_intake)

  if (isTRUE(return_data)) {
    return(
      list(
        dexa_data = stacked_dexa,
        intake_data = stacked_intake,
        discrepancies = list(v1_dexa = compared_v1_dexa$discrepancy_log,
                             v5_dexa = compared_v5_dexa$discrepancy_log)
      )
    )
  }

}

