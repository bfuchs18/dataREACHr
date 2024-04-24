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
#' @param base_wd full path to directory that contains untouchedRaw/ and bids/ (string)
#' @param reparse_rrv Re-generate CSVs from parsed text files in untouchedRaw/rrv_task/ (default = FALSE) (logical)
#' @param overwrite_sourcedata overwrite existing files in sourcedata. Applies to all tasks (default = FALSE) (logical)
#' @param overwrite_rawdata_vector names of tasks for which rawdata should be overwritten or "all_tasks" to overwrite all rawdata. Options include: "sst", "foodview", "all_tasks". Default is empty vector. (vector of characters)
#' @param overwrite_jsons overwrite existing jsons in rawdata. Applies to all tasks (default = FALSE) (logical)

#' @param return_data return phenotype to console (default = FLASE) (logical)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean datasets for each task
#'  2) meta-data/.json inforamtion for each task
#'
#' @examples
#' \dontrun{
#'
#' # process task data without overwriting any existing files
#' task_data <- proc_task(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", return_data = TRUE)
#'
#' # reparse RRV text files, overwrite sourcedata, and overwrite foodview and sst rawdata
#' task_data <- proc_task(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", reparse_rrv = TRUE, overwrite_sourcedata = TRUE, overwrite_rawdata_list = c("foodview", "sst"))
#'
#' # overwrite all task data in rawdata
#' task_data <- proc_task(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", overwrite_rawdata_list = c("all_tasks"))
#'
#' }
#'
#' @importFrom utils tail write.csv read.csv
#' @export

proc_task <- function(base_wd, reparse_rrv = FALSE, overwrite_sourcedata = FALSE, overwrite_rawdata_tasks = c(), overwrite_jsons = FALSE, return_data = FALSE) {

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
  untouchedRaw_wd <- paste0(base_wd, slash, "untouchedRaw", slash)
  sourcedata_wd <- paste0(base_wd, slash, "bids", slash, "sourcedata", slash)
  raw_wd <- paste0(base_wd, slash, "bids", slash, "rawdata", slash)

  #### Parse RRV files in untouchedRaw ####

  print("Checking for RRV text files to parse")

  ## get list of subject directories in untouchedRaw/rrv_task/
  rrv_subdirs <- list.dirs(paste0(untouchedRaw_wd, "rrv_task/"), recursive = FALSE, full.names = TRUE)

  # remove previously parsed files if reparse_rrv = TRUE
  if (isTRUE(reparse_rrv)) {

    # get a list of files in rrv_subdirs
    file_list <- list.files(rrv_subdirs, recursive = FALSE,  full.names = TRUE)

    # identify files containing the substring "parsed"
    parsed_files <- grepl("parsed", file_list)

    # get the filenames of parsed files
    parsed_files_names <- file_list[parsed_files]

    # Remove parsed files
    file.remove(parsed_files_names, quiet = TRUE)
  }


  ## for each subdir
  for (subdir in rrv_subdirs) {

    subdir_files <- list.files(subdir, recursive = FALSE,  full.names = TRUE)

    # if list of files does not contain "game" or "summary"
    if (!any(grepl("game", subdir_files)) & !any(grepl("summary", subdir_files))) {

      # if there is a text file
      if (any(grepl("\\.txt$", subdir_files))) {
        # Find indices of strings that contain ".txt"
        txt_indice <- grep("\\.txt$", subdir_files)

        # Subset the original list using the indices
        txt_file <- subdir_files[txt_indice]

        # apply text parser to generate CSVs
        rrv_parse_text(rrv_file = txt_file, return_data = FALSE)
      }

    }

  }


  #### Copy data into to sourcedata ####

  print("Copying task data from untouchedRaw in to sourcedata")

  # copy task data from untouchedRaw in to sourcedata
  util_task_untouched_to_source(base_wd, overwrite = overwrite_sourcedata)

  #### To do ####
  # reduce repetition in processing tasks by looping?

  #### Process food view data ####

  print("Processing Food View Task Data")

  ## get foodview overwrite arg
  overwrite_fv <- "foodview" %in% overwrite_rawdata_vector | "all_tasks" %in% overwrite_rawdata_vector

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
    sub_foodview_data <- util_task_foodview(sub = sub, ses = 1, bids_wd = bids_wd, overwrite = overwrite_fv, return_data = TRUE)

    # append sub_foodview_data to foodview_data
    sub_label <- paste0("sub-", sub)
    foodview_data[[sub_label]] <- sub_foodview_data

  }

  #### Process SST data ####

  print("Processing SST Data")

  ## get SST overwrite arg
  overwrite_sst <- "sst" %in% overwrite_rawdata_vector | "all_tasks" %in% overwrite_rawdata_vector

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
    sub_sst_data <- util_task_sst(sub = sub, ses = 1, bids_wd = bids_wd, overwrite = overwrite_sst, return_data = TRUE)

    # append sub_sst_data to sst_data
    sst_data[[sub_str]] <- sub_sst_data

  }

  #### Process Space Game data ####


  #### Process NIH data ####


  # Export meta-data
  meta_data = write_task_jsons(export_dir = raw_wd, overwrite = overwrite_jsons)

  #### Return data ####
  if (isTRUE(return_data)){
    return(list(foodview_data = foodview_data,
                sst_data = sst_data,
                meta_data = meta_data
    ))
  }
}

