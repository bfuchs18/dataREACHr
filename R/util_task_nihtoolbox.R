#' util_task_nihtoolbox: Clean and organize NIH toolbox assessment data into rawdata
#'
#' This function formats and organizes NIH toolbox assessment data from bids/sourcedata into rawdata for a given subject. Assessment data includes responses to each trial in the NIH toolbox.
#'
#'
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_task_foodview
#' @inheritParams util_task_foodview
#'
#' @return If return_data is set to TRUE, will return a list with 1 cleaned dataframe per run
#'
#' @examples
#'
#' \dontrun{
#' # process assessment (response) data for the NIH toolbox
#' util_task_nihtoolbox(sub_str = 'sub-001', ses_str = 'ses-1', bids_wd = bids_wd)
#'
#' }
#'
#' @importFrom utils read.csv
#' @export

util_task_nihtoolbox <- function(sub_str, ses_str = 'ses-1', bids_wd, overwrite = FALSE) {

  print(sub_str)

  #### Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(bids_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(bids_wd)) {
      stop('bids_wd must be entered as a string')
    } else if (!file.exists(bids_wd)) {
      stop('bids_wd entered, but file does not exist. Check bids_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('bids_wd must be entered as a string')
  }

  # get directory paths
  source_beh_wd <- file.path(bids_wd, 'sourcedata', sub_str, ses_str, 'beh')
  raw_beh_wd <- file.path(bids_wd, 'rawdata', sub_str, ses_str, 'beh')

  if (length(list.files(path = source_beh_wd, pattern = 'events')) > 0){

    toolbox_files_data <- list.files(path = source_beh_wd, pattern = 'events')
    toolbox_files_scores <- list.files(path = source_beh_wd, pattern = 'scores')

    data_list <- sapply(toolbox_files_data, function(x) read.table(file.path(source_beh_wd, x), header = TRUE, sep = '\t'), simplify = FALSE)
    data <- rbind.data.frame(data_list[[1]], data_list[[2]])

    scores_list <- sapply(toolbox_files_scores, function(x) read.table(file.path(source_beh_wd, x), header = TRUE, sep = '\t'), simplify = FALSE)
    scores <- rbind.data.frame(scores_list[[1]], scores_list[[2]])

  } else {
    data_source_file <- file.path(source_beh_wd, paste0(sub_str, '_', ses_str, '_task-nih_toolbox_data.tsv'))
    score_source_file <- file.path(source_beh_wd, paste0(sub_str, '_', ses_str, '_task-nih_toolbox_scores.tsv'))

    if (file.exists(data_source_file)) {
      data <- read.table(data_source_file, sep = '\t', header = TRUE)
    } else {
      print(paste(sub_str, 'has no sst assessment data file'))
    }

    if (file.exists(score_source_file)) {
      scores <- read.table(score_source_file, sep = '\t', header = TRUE)
    } else {
      print(paste(sub_str, 'has no sst assessment scores file. Aborting task processing for this sub.'))
      return()
    }
  }

  # function to process data
  proc_nih <- function(data){
    # make separate columns for task (e.g., 'Flanker Inhibitory Control') and test ages (e.g., 'Ages 8-11 v2.1') from Inst (e.g., 'NIH Toolbox Flanker Inhibitory Control and Attention Test Ages 8-11 v2.1') ??
    # Separate the 'Inst' column into 'Test' and 'Ages' columns
    data <- tidyr::separate(data, Inst, into = c('Test', 'Test_Ages'), sep = 'Test', remove = FALSE)

    # Replace values in the 'Test' column
    data <- data %>%
      dplyr::mutate(Test = dplyr::case_when(
        stringr::str_detect(Test, 'Flanker Inhibitory Control') ~ 'flanker',
        stringr::str_detect(Test, 'Dimensional Change Card Sort') ~ 'dccs',
        stringr::str_detect(Test, 'List Sorting Working Memory') ~ 'listsort',
        TRUE ~ 'other'  # Default case
      ))

    # remove columns where Test = other
    data <- data[!(data[['Test']] %in% 'other'),]

    # add subject column
    data[['sub']] <- sub_str
    data <- data %>% dplyr::relocate('sub') # move sub to first column

    # add session column
    data[['ses']] <- ses_str
    data <- data %>% dplyr::relocate('ses', .after = 1) # after col 1

    # bids compliance
    names(data) <- tolower(names(data))

    # get numeric sub
    names(data) <- gsub('\\.', '_', names(data))

    #update names
    names(data)[names(data) == 'deviceid'] <- 'device_id'

    return(data)
  }

  #proc data
  data <- proc_nih(data)
  scores <- proc_nih(scores)

  #update names
  names(data)[names(data) == 'sub'] <- 'participant_id'
  names(data)[names(data) == 'ses'] <- 'session_id'
  names(data)[names(data) == 'instordr'] <- 'inst_ordr'
  names(data)[names(data) == 'instsctn'] <- 'inst_sctn'
  names(data)[names(data) == 'itmordr'] <- 'itm_ordr'
  names(data)[names(data) == 'itemid'] <- 'item_id'
  names(data)[names(data) == 'datatype'] <- 'data_type'
  names(data)[names(data) == 'responsetime'] <- 'response_time'
  names(data)[names(data) == 'datecreated'] <- 'date_created'
  names(data)[names(data) == 'inststarted'] <- 'inst_started'
  names(data)[names(data) == 'instended'] <- 'inst_ended'


  names(scores)[names(scores) == 'sub'] <- 'participant_id'
  names(scores)[names(scores) == 'ses'] <- 'session_id'
  names(scores) <- gsub('standard_score', 'ss', names(scores))
  names(scores)[names(scores) == 'datefinished'] <- 'date_finished'
  names(scores)[names(scores) == 'national_percentile__age_adjusted_'] <- 'national_percentile_age_adjusted'
  names(scores)[names(scores) == 'instrumentbreakoff'] <- 'instrument_breakoff'
  names(scores)[names(scores) == 'instrumentstatus2'] <- 'instrument_status2'
  names(scores)[names(scores) == 'instrumentrcreason'] <- 'instrument_rc_reason'
  names(scores)[names(scores) == 'instrumentrcreasonother'] <- 'instrument_rc_reason_other'


  #### Save in rawdata #####

  # create bids/rawdata directory if it doesn't exist
  if (!dir.exists(raw_beh_wd)) {
    dir.create(raw_beh_wd, recursive = TRUE)
  }

  # define output file with path
  outfiles <- file.path(raw_beh_wd, paste0(sub_str, '_', ses_str, '_task-nih_toolbox_', c('events', 'scores'), '.tsv'))

  if (!file.exists(outfiles[1]) | isTRUE(overwrite)){

    write.table(data, outfiles[1], sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )
    write.table(scores, outfiles[2], sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )

    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }
  } else {
    return('exists')
  }

}

