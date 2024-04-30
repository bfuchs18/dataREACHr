#' proc_task: Process task data from untouchedRaw to bids/rawdata
#'
#' This function:
#' 1) generates CSVs from txt files in untouchedRaw/rrv_task if CSVs don't exist, using rrv_parse_text()
#' 2) copies task data from untouchedRaw into bids/sourcedata, using util_task_untouched_to_source()
#' 3) processes task sourcedata into BIDS and exports into bids/rawdata for the following tasks: sst (beh and func), foodview, using task-specific util_task_{task-name} functions
#' 4) creates .json files for task data, using write_task_jsons()
#'
#' To use this function, the correct path to the directory containing untouchedRaw/ and bids/ must be supplied to base_wd
#'
#' @param base_wd full path to directory that contains untouchedRaw/ and bids/ (string)
#' @param overwrite_parsed_rrv overwrite CSVs from parsed text files in untouchedRaw/rrv_task/ (default = FALSE) (logical)
#' @param overwrite_sourcedata overwrite existing files in sourcedata. Applies to all tasks (default = FALSE) (logical)
#' @param overwrite_rawdata_vector names of tasks for which rawdata should be overwritten or "all_tasks" to overwrite all rawdata. Options include: "sst", "foodview", "nih_toolbox", all_tasks". Default is empty vector. (vector of characters)
#' @param overwrite_jsons overwrite existing jsons in rawdata. Applies to all tasks (default = FALSE) (logical)

#' @param return_data return BIDS data (i.e., data ready for bids/rawdata) for each task to console (default = FLASE) (logical)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) clean datasets for each task
#'  2) meta-data information for each task stored in JSON format
#'
#' @examples
#' \dontrun{
#'
#' # process task data without overwriting any existing files and return processed data
#' task_data <- proc_task(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", return_data = TRUE)
#'
#' # overwrite RRV CSVs from parsed text files, overwrite sourcedata, and overwrite foodview and sst rawdata
#' proc_task(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", reparse_rrv = TRUE, overwrite_sourcedata = TRUE, overwrite_rawdata_list = c("foodview", "sst"))
#'
#' # overwrite all task data in rawdata
#' proc_task(base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/", overwrite_rawdata_list = c("all_tasks"))
#'
#' }
#'
#' @importFrom utils tail write.csv read.csv
#' @export

proc_task <- function(base_wd, overwrite_parsed_rrv = FALSE, overwrite_sourcedata = FALSE, overwrite_rawdata_vector = c(), overwrite_jsons = FALSE, return_data = FALSE) {

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

  # remove previously parsed files if overwrite_parsed_rrv = TRUE
  if (isTRUE(overwrite_parsed_rrv)) {

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


  #### Process NIH toolbox data ####

  print("Processing NIH toolbox Data")

  ## get toolbox overwrite arg
  overwrite_toolbox <- "nih_toolbox" %in% overwrite_rawdata_vector | "all_tasks" %in% overwrite_rawdata_vector

  # list all toolbox files (ses-1 and ses-2) with full path
  toolbox_source_files <- list.files(sourcedata_wd, pattern = "toolbox", recursive = TRUE, full.names = TRUE)

  for (session in c(1,2)) {

    ses_str <- paste0("ses-", session)

    # Filter toolbox_source_files by session
    toolbox_session_source_files <- toolbox_source_files[grepl(ses_str, toolbox_source_files)]

    # get list of subjects with toolbox files (subset the toolbox_session_source_files to include "sub-" and the next 3 characters)
    toolbox_subs <- unique(stringr::str_extract(toolbox_session_source_files, "sub-..."))

    # process sst task data and organize into bids/rawdata for each subject
    for (sub_str in toolbox_subs) {

      # get sub number from sub_str
      sub <- as.numeric(gsub("sub-","", sub_str))

      # process assessment (response data)
      # sub_toolbox_assessment_data <- util_task_toolbox(sub = sub, ses = session, bids_wd = bids_wd, overwrite = overwrite_toolbox, return_data = TRUE)

      # process score by adding it to phenotype/toolbox.csv
      sub_toolbox_score_data <- util_phenotype_toolbox(sub = sub, ses = session, bids_wd = bids_wd, overwrite = overwrite_toolbox, return_data = TRUE)

    }
  }



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

