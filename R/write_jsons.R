#' write_jsons: Write jsons created with json functions (called within proc_redcap.R)
#'
#' This function organizes writes json meta-data files
#' @param export_dir string with absolute path to export directory (typically bids phenotype directory)
#' @param overwrite logical (TRUE/FALSE) to indicate if json files should be overwritten


write_jsons <- function(export_dir, overwrite) {

  #### Set up/initial checks #####

  # check that overwrite argument exists and is logical
  overwrite_arg <- methods::hasArg(overwrite)

  if (isFALSE(overwrite_arg)) {
    stop("must enter overwrite argument (TRUE/FALSE)")
  } else {
    if (isFALSE(is.logical(overwrite))) {
      stop("overwrite argument must be logical (TRUE/FALSE)")
    }
  }

  # check that export_dir argument exists and is a string
  export_dir_arg <- methods::hasArg(export_dir)

  if (isFALSE(export_dir_arg)) {
    stop("must enter export_dir argument")
  } else {
    if (is.character(export_dir)) {

      # generate export_dir if it doesn't exist
      if (!file.exists(export_dir)){
        dir.create(file.path(export_dir))
      }

    } else {
      stop("export_dir argument must be string")
    }
  }

  #### Export meta-data #####

  # List of json functions and corresponding filenames
  json_functions <- list(

    # questionnaire jsons
    json_cebq = "cebq.json",
    json_cbq = "cbq.json",
    json_cfq = "cfq.json",
    json_efcr = "efcr.json",
    json_ffbs = "ffbs.json",
    json_spsrq = "spsrq.json",
    json_debq = "debq.json",
    json_tfeq18 = "tfeq.json",
    json_bisbas = "bisbas.json",
    json_scpf = "scpf.json",
    json_hfssm = "hfssm.json",
    json_audit = "audit.json",
    json_rank = "rank.json",
    json_puberty = "puberty.json",
    json_chaos = "chaos.json",
    json_pss = "pss.json",
    json_lbc = "lbc.json",
    json_brief2 = "brief2.json",
    json_cshq = "cshq.json",
    json_bes = "bes.json",
    json_fsq = "fsq.json",
    json_pwlb = "pwlb.json",
    json_pptq = "pptq.json",
    json_sic = "sic.json",

    # non-questionnaire jsons
    json_mri_visit = "mri_visit.json",
    json_anthro = "anthro.json"

  )


  for (func_name in names(json_functions)) {
    filename <- paste0(export_dir, json_functions[[func_name]])

    # If overwrite is false
    if (isFALSE(overwrite)) {

      # Write json if it doesn't exist
      if (!file.exists(filename)) {
        func <- get(func_name)  # Get the function by name
        json <- func()          # Call the function
        write(json, filename)   # Write json
      }

    } else {
      func <- get(func_name)  # Get the function by name
      json <- func()          # Call the function
      write(json, filename)
    }
  }


}

