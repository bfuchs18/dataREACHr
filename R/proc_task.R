#' proc_task: Process task data from untouchedRaw to bids/rawdata
#'
#' This function:
#' 1) copies task data from untouchedRaw into bids/sourcedata for all tasks (food view, pit, sst, nih toolbox, spacegame), using util_task_untouched_to_source(all_tasks = TRUE)
#' 2) processes task sourcedata and exports cleaned dataframes into bids/rawdata for the following tasks: rrv, sst, foodview, using task-specific util_task_{task-name} functions
#' 3) exports JSON meta-data files for tasks organized into rawdata (rrv, sst, foodview), using write_task_jsons()
#'
#' To use this function, the correct path to the directory containing untouchedRaw/ and bids/ must be supplied to base_wd
#'
#' @param base_wd (string) full path to directory that contains untouchedRaw/ and bids/
#' @param overwrite_sourcedata (logical) overwrite files in sourcedata for all tasks (default = FALSE)
#' @param overwrite_rawdata (logical) overwrite files in rawdata for all tasks (default = FALSE)
#' @param overwrite_rawdata_vector (vector) vector with names of tasks for which rawdata should be overwritten. Options include: c("sst", "foodview", "nih_toolbox", "rrv").
#' @param overwrite_jsons overwrite existing jsons in rawdata. Applies to all tasks (default = FALSE) (logical)

#' @param return_data return BIDS data (i.e., data ready for bids/rawdata) for each task to console (default = FLASE) (logical)
#'
#' @return If return_data is set to TRUE, will return a list including:
#'  1) lists with cleaned dataframes for each subject for each task
#'  2) meta-data information for each task stored in JSON format
#'
#' @examples
#' \dontrun{
#'
#' # set base_wd
#' base_wd = "/Users/baf44/projects/Keller_Marketing/ParticipantData/"
#'
#' # process task data without overwriting any existing files, return processed data
#' task_data <- proc_task(base_wd = base_wd, return_data = TRUE)
#'
#' # process task data and overwrite rawdata for foodview task only
#' proc_task(base_wd = base_wd, overwrite_rawdata_vector = c("foodview"))
#'
#' # process task data and overwrite all sourcedata, rawdata, and meta-data
#' proc_task(base_wd = base_wd, overwrite_sourcedata = TRUE, overwrite_rawdata = TRUE, overwrite_jsons = TRUE)
#'
#' }
#'
#' @importFrom utils tail write.csv read.csv
#' @export

proc_task <- function(base_wd, overwrite_sourcedata = FALSE, overwrite_rawdata = FALSE, overwrite_rawdata_vector, overwrite_jsons = FALSE, return_data = FALSE) {

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

  # check that overwrite_all[] options are logical
  if (!is.logical(overwrite_sourcedata)) {
    stop("overwrite_sourcedata must be logical. Enter TRUE or FALSE")
  }

  if (!is.logical(overwrite_rawdata)) {
    stop("overwrite_rawdata must be logical. Enter TRUE or FALSE")
  }

  # check overwrite_rawdata_vector input
  rawdata_vector_arg <- methods::hasArg(overwrite_rawdata_vector)

  if (isTRUE(rawdata_vector_arg)) {
    if (!is.vector(overwrite_rawdata_vector)) {
      stop("Input to overwrite_rawdata_vector must be vector (e.g., overwrite_rawdata_vector = c('rrv')")
    } else {
      for (task in overwrite_rawdata_vector) {
        if (!task %in% c("sst", "foodview", "nih_toolbox", "rrv")) {
          stop(paste(task, "is not an option for overwrite_sourcedata_vector"))
        }
      }
    }
  } else {
    overwrite_rawdata_vector = c()
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('The proc_task.R has not been thoroughly tested on Windows systems, may have visit_data_path errors. Contact Bari at baf44@psu.edu if there are errors')
  }

  # define paths for processing
  bids_wd <- paste0(base_wd, slash, "bids", slash)
  untouchedRaw_wd <- paste0(base_wd, slash, "untouchedRaw", slash)
  sourcedata_wd <- paste0(base_wd, slash, "bids", slash, "sourcedata", slash)
  raw_wd <- paste0(base_wd, slash, "bids", slash, "rawdata", slash)

  #### Copy data into to sourcedata ####

  print("Copying task data from untouchedRaw in to sourcedata")

  # copy task data from untouchedRaw in to sourcedata
  util_task_untouched_to_source(base_wd, overwrite = overwrite_sourcedata, all_tasks = TRUE)

  #### To do ####
  # reduce repetition in processing tasks by looping?

  #### Process food view data ####

  print("Processing Food View Task Data")

  ## get foodview overwrite arg
  overwrite_fv <- "foodview" %in% overwrite_rawdata_vector | isTRUE(overwrite_rawdata)

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
    foodview_data[[sub_str]] <- sub_foodview_data

  }

  #### Process SST data ####

  print("Processing SST Data")

  ## get SST overwrite arg
  overwrite_sst <- "sst" %in% overwrite_rawdata_vector | isTRUE(overwrite_rawdata)

  ## get list of sst files in sourcedata
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

  #### Process NIH toolbox data ####

  print("Processing NIH toolbox Data")

  ## get toolbox overwrite arg
  overwrite_toolbox <- "nih_toolbox" %in% overwrite_rawdata_vector | isTRUE(overwrite_rawdata)

  # list all toolbox files (ses-1 and ses-2) with full path
  toolbox_source_files <- list.files(sourcedata_wd, pattern = "toolbox", recursive = TRUE, full.names = TRUE)

  ## initialize list to save subject assessment data to
  toolbox_data <- list()

  for (session in c(1,2)) {

    ses_str <- paste0("ses-", session)

    toolbox_data[[ses_str]] <- list()

    # Filter toolbox_source_files by session
    toolbox_session_source_files <- toolbox_source_files[grepl(ses_str, toolbox_source_files)]

    # get list of subjects with toolbox files (subset the toolbox_session_source_files to include "sub-" and the next 3 characters)
    toolbox_subs <- unique(stringr::str_extract(toolbox_session_source_files, "sub-..."))

    # process sst task data and organize into bids/rawdata for each subject
    for (sub_str in toolbox_subs) {

      # get sub number from sub_str
      sub <- as.numeric(gsub("sub-","", sub_str))

      # process assessment (response data)
      sub_toolbox_assessment_data <- util_task_toolbox(sub = sub, ses = session, bids_wd = bids_wd, overwrite = overwrite_toolbox, return_data = TRUE)

      # process score by adding it to phenotype/toolbox.csv
      sub_toolbox_score_data <- util_phenotype_toolbox(sub = sub, ses = session, bids_wd = bids_wd, overwrite = overwrite_toolbox, return_data = TRUE)

      # add assessment data to toolbox_data
      toolbox_data[[ses_str]][[sub_str]] <- sub_toolbox_assessment_data

    }
  }

  #### Process RRV data ####

  print("Processing RRV Data")

  ## get RRV overwrite arg
  overwrite_rrv <- "rrv" %in% overwrite_rawdata_vector | isTRUE(overwrite_rawdata)

  ## get list of rrv files in sourcedata
  rrv_source_files <- list.files(sourcedata_wd, pattern = "rrv", recursive = TRUE)

  # get list of subjects with sst files in sourcedata
  rrv_subs <- unique(substr(rrv_source_files, 1, 7))

  ## initialize list to save subject data to
  rrv_data <- list()

  # process sst task data and organize into bids/rawdata for each subject
  for (sub_str in rrv_subs) {

    # get sub number from sub_str
    sub <- as.numeric(gsub("sub-","", sub_str))

    # process
    sub_rrv_data <- util_task_rrv(sub = sub, ses = 1, bids_wd = bids_wd, overwrite = overwrite_rrv, return_data = TRUE)

    # append sub_rrv_data to rrv_data
    rrv_data[[sub_str]] <- sub_rrv_data

  }


  #### TO DO: Process PIT data ####


  #### TO DO: Process Space Game data ####

  #### Export meta-data ####
  meta_data = write_task_jsons(bids_wd = bids_wd, overwrite = overwrite_jsons)


  #### Return data ####
  if (isTRUE(return_data)){
    return(list(foodview_data = foodview_data,
                sst_data = sst_data,
                rrv_data = rrv_data,
                toolbox_data = toolbox_data,
                meta_data = meta_data
    ))
  }
}

