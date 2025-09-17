#' proc_task_derivs: Generate derivative task data from individual files in rawdata
#'
#' This function: \itemize{
#' \item{1) processes task data from rawdata to generate derivative data in bids/derivatives}
#' \item{2) generate json files for derivative task files}
#' \item{3) write data and json files}
#' }
#'
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @param proc_source whether to processes the raw data using proc_tasks.R. Default = FALSE
#' @inheritParams proc_tasks
#' @inheritParams proc_tasks
#' @inheritParams util_group_foodview
#'
#' @examples
#'
#' \dontrun{
#' # organize task data for space game and NIH toolbox in untouchedRaw into sourcedata and raw data
#' proc_task(base_wd = base_wd, task_list = c('spacegame', 'nih_toolbox'))
#'
#' }
#'
#'
#' @export
#'

proc_task_derivs <- function(base_wd, overwrite = FALSE, proc_source = proc_source, task_list = 'all', return_data = FALSE) {

  #### 1. Set up/initial checks #####

  # check that base_wd exist and is a data.frame
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

  # check that task options correctly specified
  task_list_arg <- methods::hasArg(task_list)

  if (isTRUE(task_list_arg)) {

    #hand task list
    if (length(task_list) == 1) {
      if (task_list == 'all') {
        task_list <- c('foodview', 'nih_toolbox', 'pit', 'rrv', 'spacegame', 'sst')
      }
    } else if (!is.vector(task_list)) {
      stop('Input to task_list must entered as a \'all\' or be vector (e.g., task_list = c("foodview")')
    } else {
      if (sum(!task_list %in% c('all', 'foodview', 'nih_toolbox', 'pit', 'rrv', 'spacegame', 'sst')) > 0) {
        stop(paste0('at least 1 item in tasks is not an option: ', task_list))
      }
    }
  } else {
    stop('Must provide at least 1 option in tasks argument')
  }

  #### Define paths ####
  bids_wd <- file.path(base_wd, 'bids')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives')


  # Food Rating ####

  if ('foodview' %in% task_list) {

    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, task_list = 'foodview')
    }

    print('-- creating Food View summary data')

    # get list of available subjects
    foodrating_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-1', 'func')), pattern = '*foodview_run-01_events.tsv', recursive = TRUE))
    names(foodrating_list) <- 'filename'

    #get list of subject IDs
    foodrating_list[['sub_str']] <- sapply(foodrating_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)

    #get summary data -> produces derivative dataframe
    foodrating_database <- util_group_foodview(data_list = foodrating_list, ses = 'ses-1', base_wd = base_wd, overwrite = TRUE, return_data = TRUE)

  }

  # NIH Toolbox ####

  if ('nih_toolbox' %in% task_list) {

    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, task_list = 'nih_toolbox')
    }

    print('-- creating NIH Toolbox summary data')

    # get list of available subjects
    nihtoolbox_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-*', 'beh')), pattern = '*nih_toolbox_events.tsv', recursive = TRUE))
    names(nihtoolbox_list) <- 'filename'


    #get list of subject IDs
    nihtoolbox_list['sub_str'] <- sapply(nihtoolbox_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)

    # session id
    nihtoolbox_list['ses'] <- ifelse(grepl('ses-1', nihtoolbox_list[['filename']]), 'ses-1', 'ses-2')

    #create derivative file
    if (!dir.exists(phenotype_wd)) {
      dir.create(phenotype_wd, recursive = TRUE)
    }

    nih_scores_dat <- do.call(rbind.data.frame, mapply(read.table, file.path(raw_wd, nihtoolbox_list[['sub_str']], nihtoolbox_list[['ses']], 'beh', paste0(nihtoolbox_list[['sub_str']], '_', nihtoolbox_list[['ses']], '_task-nih_toolbox_scores.tsv')), sep = '\t', header = TRUE, SIMPLIFY = FALSE))

    write.table(nih_scores_dat, file.path(phenotype_wd, 'nih_toolbox_scores.tsv'), sep = '\t', quote = FALSE, row.names = FALSE, na = "n/a" )

    #generate json file for derivative data
    nihtoolbox_json <- json_nihtoolbox_scores()

    nihtoolbox_filename_json <- file.path(phenotype_wd, 'nih_toolbox_scores.json')

    if ( isTRUE(overwrite) | !file.exists(nihtoolbox_filename_json) ) {
      write(nihtoolbox_json, nihtoolbox_filename_json)
    }
  }

  # Food Choice ####

  if ('pit' %in% task_list) {

    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, task_list = 'pit')
    }

    print('-- creating PIT summary data')

    # get list of available subjects
    pit_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-*', 'beh')), pattern = '*pit_events.tsv', recursive = TRUE))
    names(pit_list) <- 'filename'

    #get list of subject and session IDs
    pit_list[['sub_str']] <- sapply(pit_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)

    pit_list[['ses_str']] <- ifelse(grepl('ses-1', pit_list[['filename']]), 'ses-1', 'ses-2')

    #get summary data -> produces derivative dataframe
    pit_database <- util_group_pit(data_list = pit_list, base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
  }

  # RRV ####

  if ('rrv' %in% task_list) {

    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, task_list = 'rrv')
    }

    print('-- creating RRV summary data')

    # get list of available subjects
    rrv_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-1', 'beh')), pattern = '*rrv_events.tsv', recursive = TRUE))
    names(rrv_list) <- 'filename'

    #get list of subject IDs
    rrv_list[['sub_str']] <- sapply(rrv_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)

    #get summary data -> produces derivative dataframe
    rrv_database <- util_group_rrv(data_list = rrv_list, base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
  }

  # Stop-Signal Task  ####

  if ('sst' %in% task_list) {

    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, task_list = 'sst')
    }

    print('-- creating Taste-Test summary data')

    # get list of available subjects
    sst_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-1', 'beh')), pattern = '*prescan_events.tsv', recursive = TRUE))
    names(sst_list) <- 'filename'

    #get list of subject IDs
    sst_list[['sub_str']] <- sapply(sst_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)

    #get summary data -> produces derivative dataframe
    sst_database <- util_group_sst(data_list = sst_list, ses = 'ses-1', base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
  }


  # Space Game ####

  if ('spacegame' %in% task_list) {

    if (isTRUE(proc_source)) {
      #organize data into BIDS sourcedata and rawdata
      proc_tasks(base_wd = base_wd, overwrite = overwrite, task_list = 'spacegame')
    }

    print('-- creating Space Game summary data')

    # get list of available subjects
    spacegame_list <- as.data.frame(list.files(path = Sys.glob(file.path(raw_wd, 'sub-*', 'ses-baseline', 'beh')), pattern = '*spacegame_events.tsv', recursive = TRUE))
    names(spacegame_list) <- 'filename'

    #get list of subject IDs
    spacegame_list[['sub_str']] <- sapply(spacegame_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)

    #get summary data -> produces derivative dataframe
    spacegame_database <- util_group_spacegame(data_list = spacegame_list, ses = 'baseline', base_wd = base_wd, overwrite = TRUE, return_data = TRUE)
  }




  if (isTRUE(return_data)){
    task_data <- list(
      foodrating_database = foodrating_database,
      foodchoice_database = foodchoice_database,
      shapegame_database = shapegame_database,
      spacegame_database = spacegame_database,
      nihtoolbox_database = nih_scores_dat,
      sst_database = sst_database
    )

    return(task_data)
  }
}

