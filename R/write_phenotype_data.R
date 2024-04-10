#' write_phenotype_data: Export phenotype data and associated meta-data (called within proc_task.R)
#' This function exports phenotype data as .tsv and associated meta-data as .json. For bids compliance, missing data is replaced with 'n/a'
#' This function will only work if called within proc_task.R because it uses objects saved in the global environment by proc_task.R
#'
#' @param export_dir string with absolute path to export directory (typically bids phenotype directory)
#' @param overwrite logical (TRUE/FALSE) to indicate if  files should be overwritten


# Update to only export if overwrite == TRUE

write_phenotype_data <- function(export_dir, overwrite) {

  # generate export_dir if it doesnt exist
  if (!file.exists(export_dir)){
    dir.create(file.path(export_dir))
  }

  # make a list of lists including dataframe, and export name (without extension)
  data_to_export <- list(

    # # single visit data
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

    # replace na in dataframe with 'n/a'
    df[is.na(df)] <- 'n/a'

    # get the phenotype name
    phenotype_name <- data_to_export[[i]][[2]]

    # define json function name
    json_func_name = paste0("json_", phenotype_name)

    # Get the json function by name
    json_func <- get(json_func_name)

    # Call the function
    json <- json_func()

    # add extensions to phenotype_name
    filename_tsv <- paste0(export_dir, slash, phenotype_name, ".tsv")
    filename_json <- paste0(export_dir, slash, phenotype_name, ".json")

    # write tsv
    if ( isTRUE(overwrite) | !file.exists(filename_tsv) ) {
      # write tsv
      write.table(
        df,
        filename_tsv,
        quote = FALSE,
        sep = '\t',
        col.names = TRUE,
        row.names = FALSE
      )
    }


    # write json
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {

      # write json
      write(json, filename_json)
    }

  }
}

