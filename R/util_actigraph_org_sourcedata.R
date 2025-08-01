#' util_actigraph_org_sourcedata: Organize actigraphy raw data into BIDS format sourcedata
#'
#' This function copies data and saves it in sourcedata
#'
#' @inheritParams proc_tasks
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#'
#' @examples
#'
#' # organize actigraphy data
#' org_actigraph <- util_actigraph_org_sourcedata(tsub_str = 'sub_001', ses_str = 'ses-1', base_wd, overwrite = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_actigraph_org_sourcedata <- function(base_wd, sub_str, ses_str, overwrite = FALSE) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  path_arg <- methods::hasArg(base_wd)

  if (isTRUE(path_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but file does not exist. Check base_wd string.")
    }
  } else if (isFALSE(path_arg)) {
    stop("base_wd must be entered as a string")
  }

  # get paths
  raw_actigraph_path <- file.path(base_wd, 'actigraph', 'raw', 'gt3x_ files')


  # get all files for sub in raw_untouched
  raw_files <- list.files(path = raw_actigraph_path, pattern = sub_str)

  # new file name
  rename_files <- gsub('_actigraph', paste0(ses_str, '_tracksys-ActiGraph_motion'), raw_files)

  #### Save in sourcedata #####
  # set paths for other directories
  source_wd <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

  raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'beh')

  #make directory if needed
  if (!dir.exists(source_wd)) {
    dir.create(source_wd, recursive = TRUE)
  }

  #make directory if needed
  if (!dir.exists(raw_wd)) {
    dir.create(raw_wd, recursive = TRUE)
  }

  # copy files
  if (!file.exists(file.path(source_wd, rename_files[1])) | isTRUE(overwrite)) {

    file.copy(from = file.path(raw_actigraph_path, raw_files), to = file.path(source_wd, rename_files), overwrite = overwrite)

    file.copy(from = file.path(raw_actigraph_path, raw_files), to = file.path(raw_wd, rename_files), overwrite = overwrite)

    #return message
    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }

  } else {
    return('exists')
  }

}
