#' proc_task: Process task data in untouchedRaw into bids
#'
#' This function:
#' 1) copies task data from untouchedRaw into sourcedata
#' 2) cleans sourcedata to save in BIDS format in rawdata. Produces the following files:
#'    *
#' 3) calls functions to create .json files for task data
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @param base_wd full path to directory that contains untouchedRaw/ and bids/
#' @param overwrite overwrite existing files (default = FALSE)
#' @param return_data return phenotype to console (default = FLASE)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean datasets for each task
#'  2) meta-data/.json inforamtion for each task
#'
#' @examples
#' \dontrun{
#' task_data <- proc_task(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", return_data = TRUE)
#'
#' }
#'
#' @importFrom utils tail write.csv read.csv
#' @export

proc_task <- function(base_wd, overwrite = FALSE, return_data = FALSE) {

  #### Set up/initial checks #####

  # check that base_wd exists
  path_arg <- methods::hasArg(base_wd)

  if (isTRUE(path_arg)) {
    if (!is.character(base_wd)) {
      stop("base_wd must be entered as a string")
    } else if (!file.exists(base_wd)) {
      stop("base_wd entered, but path does not exist. Check base_wd string.")
    }
  } else if (isFALSE(path_arg)) {
    stop("base_wd must be entered as a string")
  }


  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The task_redcap.R has not been thoroughly tested on Windows systems, may have visit_data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  bids_wd <- paste0(base_wd, slash, "bids", slash)
  sourcedata_wd <- paste0(base_wd, slash, "bids", slash, "sourcedata", slash)
  raw_wd <- paste0(base_wd, slash, "bids", slash, "rawdata", slash)

  ####Copy data into to sourcedata ####

  # copy task data from untouchedRaw in to sourcedata
  util_org_sourcedata(base_wd, overwrite = FALSE)

  #### To do ####
  # reduce repetition in processing tasks by looping?

  #### Process food view data ####

  ## get list of foodview files in sourcedata
  foodview_source_files <- list.files(sourcedata_wd, pattern = "foodview", recursive = TRUE)

  # get list of subjects with food view files in sourcedata
  foodview_subs <- unique(substr(foodview_source_files, 1, 7))

  ## initialize list to save subject data to
  foodview_data <- list()

  # process foodview task data and organize into bids/rawdata for each subject
  for (sub_str in foodview_subs) {

    # get sub num
    sub <- as.numeric(gsub("sub-","", sub_str))

    # process
    sub_foodview_data <- util_task_foodview(sub = sub, ses = 1, bids_wd = bids_wd, overwrite = overwrite, return_data = TRUE)

    # append sub_foodview_data to foodview_data
    sub_label <- paste0("sub-", sub)
    foodview_data[[sub_label]] <- sub_foodview_data

  }

  #### Process SST data ####

  ## get list of foodview files in sourcedata
  sst_source_files <- list.files(sourcedata_wd, pattern = "stop", recursive = TRUE)

  # get list of subjects with sst files in sourcedata
  sst_subs <- unique(substr(sst_source_files, 1, 7))

  ## initialize list to save subject data to
  sst_data <- list()

  # process sst task data and organize into bids/rawdata for each subject
  for (sub_str in sst_subs) {

    # get sub number from sub_str
    sub <- as.numeric(gsub("sub-","", sub_str))

    # process
    sub_sst_data <- util_task_sst(sub = sub, ses = 1, bids_wd = bids_wd, overwrite = overwrite, return_data = TRUE)

    # append sub_sst_data to sst_data
    sst_data[[sub_str]] <- sub_sst_data

  }

  #### Process Space Game data ####


  #### Process NIH data ####


  # Export meta-data
  # make separate overwrite args -- 1 for dataframes and 1 for jsons?
  meta_data = write_task_jsons(export_dir = raw_wd, overwrite = overwrite)

  #### Return data ####
  if (isTRUE(return_data)){
    return(list(foodview_data = foodview_data,
                sst_data = sst_data,
                meta_data = meta_data
    ))
  }
}

