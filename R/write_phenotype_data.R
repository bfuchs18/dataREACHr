#' write_phenotype_data: Export phenotype data as .tsv. (called within proc_task.R)
#' This function exports phenotype data. For bids compliance, missing data is replaced with 'n/a'
#'
#' @param export_dir string with absolute path to export directory (typically bids phenotype directory)
#' @param overwrite logical (TRUE/FALSE) to indicate if .tsv files should be overwritten


# Update to only export if overwrite == TRUE

write_phenotype_data <- function(export_dir, overwrite) {

  # generate export_dir if it doesnt exist
  if (!file.exists(export_dir)){
    dir.create(file.path(export_dir))
  }

  # make a list of lists including dataframe and export name
  data_to_export <- list(

    # single visit data
    list(parent_v1_data$cfq_data$bids_phenotype, "cfq.tsv"),
    list(parent_v1_data$efcr_data$bids_phenotype, "efcr.tsv"),
    list(parent_v1_data$lbc_data$bids_phenotype, "lbc.tsv"),
    list(parent_v1_data$pss_data$bids_phenotype, "pss.tsv"),
    # list(parent_v1_data$chaos_data$bids_phenotype, "chaos.tsv"), # not in bids_phenotype yet

    list(parent_v2_data$brief_data$bids_phenotype, "brief2.tsv"),
    list(parent_v2_data$bes_data$bids_phenotype, "bes.tsv"),
    list(parent_v2_data$ffbs_data$bids_phenotype, "ffbs.tsv"),
    # list(parent_v2_data$ffq_data$bids_phenotype, "ffq.tsv"), # not in bids_phenotype yet

    list(parent_v3_data$spsrq_data$bids_phenotype, "spsrq.tsv"),
    list(parent_v3_data$pwlb_data$bids_phenotype, "pwlb.tsv"),
    list(parent_v3_data$tfeq_data$bids_phenotype, "tfeq.tsv"),
    list(parent_v3_data$bisbas_data$bids_phenotype, "bisbas.tsv"),
    list(parent_v3_data$debq_data$bids_phenotype, "debq.tsv"),
    list(parent_v3_data$scpf_data$bids_phenotype, "scpf.tsv"),

    list(child_v4_data$pptq_data$bids_phenotype, "pptq.tsv"),

    # stacked visit data
    list(stacked_stq, "stq.tsv"),
    list(stacked_kbas, "kbas.tsv"),
    list(stacked_household, "household.tsv"),
    list(stacked_cebq, "cebq.tsv"),
    list(stacked_cbq, "cbq.tsv"),
    list(stacked_stq, "stq.tsv"),
    # list(stacked_cshq, "cshq.tsv"), # not in bids_phenotype yet
    # list(stacked_pstca, "pstca.tsv"), # not in bids_phenotype yet
    list(stacked_audit, "audit.tsv"),
    # list(stacked_pmum, "pmum.tsv"), # not in bids_phenotype yet
    list(stacked_cfpq, "cfpq.tsv"),
    # list(stacked_rank, "rank.tsv"),  # not in bids_phenotype yet
    list(stacked_puberty, "puberty.tsv"),
    list(stacked_loc, "loc.tsv"),

    # merged data
    list(merged_anthro, "anthropometrics.tsv"),
    list(merged_intake, "intake.tsv"),
    list(merged_mri, "mri_visit.tsv"),

    # double entry data
    list(processed_de_data$dexa_data, "dexa.tsv")

  )


  for (i in 1:length(data_to_export)) {

    # Get the dataframe
    df <- data_to_export[[i]][[1]]

    # replace na in dataframe with 'n/a'
    df[is.na(df)] <- 'n/a'

    # define filename for export
    filename <- paste0(export_dir, slash, data_to_export[[i]][[2]])

    # If overwrite is TRUE
    if (isTRUE(overwrite)) {

      # write
      write.table(
        df,
        filename,
        quote = FALSE,
        sep = '\t',
        col.names = TRUE,
        row.names = FALSE
      )

      # If overwrite is FALSE

    } else if (isFALSE(overwrite)) {
      # write if file doesn't exist
      if (!file.exists(filename)) {
        write.table(
          df,
          filename,
          quote = FALSE,
          sep = '\t',
          col.names = TRUE,
          row.names = FALSE
        )
      }
    }

  }
}

