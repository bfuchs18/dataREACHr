#' proc_tasks: Process task data from untouchedRaw to create bids compliant files
#'
#' This function: \itemize{
#' \item{1) copies task data from untouchedRaw into bids/sourcedata using util_task_untouched_to_source}
#' \item{2) processes task sourcedata and exports cleaned dataframes into bids/rawdata using task-specific util_task_{task-name} functions}
#' }
#'
#'
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @param task_list tasks to process. Options include 'all' to process all task data or a list of the following:\itemize{
#'  \item{'foodview' - fMRI Food Viewing task}
#'  \item{'nihtoolbox' - NIH Toolbox data}
#'  \item{'pit' - Pavlovian Instrumental Transfer task data}
#'  \item{'rrv' - Relative Reinforcing Value of Food task}
#'  \item{'spacegame' - Space Game data (need to finish processing in Matlab)}
#'  \item{'sst' - fMRI Stop-Signal Task data}
#' }
#'
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

proc_tasks <- function(base_wd, overwrite = FALSE, task_list) {

  #### Set up/initial checks ####

  # check that base_wd exist and is a string
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  # check that task options correctly specified
  task_list_arg <- methods::hasArg(task_list)

  if (isTRUE(task_list_arg)) {
    if (task_list != 'all' & !is.vector(task_list)) {
      stop('Input to task_list must entered as a \'all\' or be vector (e.g., task_list = c("rrv"")')
    } else {
      if (sum(!task_list %in% c('all', 'sst','foodview','spacegame','nih_toolbox','rrv','pit')) > 0) {
        stop(paste0('at least 1 item in tasks is not an option: ', task_list))
      }
    }
  } else {
    stop('Must provide at least 1 option in tasks argument')
  }

  #### Define paths ####
  bids_wd <- file.path(base_wd, 'bids')
  data_path <- file.path(base_wd, 'untouchedRaw')
  sourcedata_wd <- file.path(base_wd, 'bids', 'sourcedata')
  raw_wd <- file.path(base_wd, 'bids', 'rawdata')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')


  #### FoodView task ####

  if (task_list == 'all' | 'foodview' %in% task_list) {
    print('-- copying Food View')

    foodview_dir <- file.path(data_path, 'foodview_task')

    # get list of available subjects
    foodview_list <- list.files(foodview_dir, pattern = '.txt', recursive = TRUE)
    foodview_list <- as.data.frame(foodview_list[!(grepl('onsets', foodview_list))])
    names(foodview_list) <- 'filename'

    #get list of subject IDs
    foodview_list[['id']] <- as.numeric(sapply(foodview_list[['filename']], function(x) substr(x, unlist(gregexpr('-', x))+1, unlist(gregexpr('.txt', x))-1), simplify = TRUE))

    foodview_list[['sub_str']] <- sapply(foodview_list[['id']], function(x) sprintf('sub-%03d', x), simplify = TRUE)

    #organize data into BIDS sourcedata
    foodview_list[['sourcedata_done']] <- sapply(foodview_list[['id']], function(x) util_copy_to_source(base_wd = base_wd, task_dir = foodview_dir, task_str = 'foodview', sub_id = x, sub_str = sprintf('sub-%03d', x), ses_str = 'ses-1', overwrite = overwrite), simplify = TRUE)

    #process raw data
    foodview_list[['rawproc_done']] <- sapply(foodview_list[['sub_str']], function(x) util_task_foodview(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)

    #generate json file for rawdata
    foodview_json <- json_foodview()

    foodview_filename_json <- file.path(bids_wd, 'task-foodview_events.json')

    if ( isTRUE(overwrite) | !file.exists(foodview_filename_json) ) {
      write(foodview_json, foodview_filename_json)
    }

  }

  #### NIH toolbox ####
  if (task_list == 'all' | 'nih_toolbox' %in% task_list) {
    print('-- copying NIH toolbox')

    nih_list_v1 <- as.data.frame(list.files(path = Sys.glob(file.path(data_path, 'nih-toolbox', 'visit1', 'reach*')), pattern = '.csv'))
    names(nih_list_v1) <- 'filename'

    nih_list_v5 <- as.data.frame(list.files(path = Sys.glob(file.path(data_path, 'nih-toolbox', 'visit5', 'reach*')), pattern = '.csv'))
    names(nih_list_v5) <- 'filename'

    # add session
    nih_list_v1['ses'] <- 'ses-1'
    nih_list_v5['ses'] <- 'ses-2'

    nih_list_v1['ses_folder'] <- 'visit1'
    nih_list_v5['ses_folder'] <- 'visit5'

    # combine
    nih_list <- rbind.data.frame(nih_list_v1, nih_list_v5)

    # remove any registration data
    nih_list <- nih_list[!grepl('reg', nih_list[['filename']]), ]

    #get list of subject IDs
    nih_list['id_str'] <- sapply(nih_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))-1), simplify = TRUE)

    nih_list['sub_str'] <- paste0('sub-', nih_list[['id_str']])

    nih_list['id'] <- as.numeric(nih_list[['id_str']])

    nih_list['id_folder'] <- paste0('reach_',  nih_list[['id_str']])

    #organize data into BIDS sourcedata
    nih_list[['sourcedata_done']] <- mapply(util_copy_to_source, sub_id = nih_list[['id']], sub_str = nih_list[['sub_str']], task_dir = file.path(data_path, 'nih-toolbox', nih_list[['ses_folder']], nih_list[['id_folder']]), ses_str = nih_list[['ses']], MoreArgs = list(base_wd = base_wd, task_str = 'nih', overwrite = overwrite))

    # process raw data
    nih_list[['rawproc_done']] <- mapply(util_task_nihtoolbox, sub_str = nih_list[['sub_str']], ses_str = nih_list[['ses']], MoreArgs = list(base_wd = base_wd, overwrite = overwrite))

    #generate json file for rawdata
    nihtoolbox_events_json <- json_nihtoolbox_events()
    nihtoolbox_scores_json <- json_nihtoolbox_scores()

    nihtoolbox_filename_events_json <- file.path(bids_wd, 'task-nih_toolbox_events.json')
    nihtoolbox_filename_scores_json <- file.path(bids_wd, 'task-nih_toolbox_scores.json')

    if ( isTRUE(overwrite) | !file.exists(nihtoolbox_filename_events_json) ) {
      write(nihtoolbox_events_json, nihtoolbox_filename_events_json)
      write(nihtoolbox_scores_json, nihtoolbox_filename_scores_json)
    }
  }

  #### PIT ####

  if (task_list == 'all' | 'pit' %in% task_list) {
    print('-- copying PIT task')

    pit_list_ses1 <- as.data.frame(list.files(path = Sys.glob(file.path(data_path, 'pit_task', 'visit3-4')), pattern = '*Food-PIT*'))
    names(pit_list_ses1) <- 'filename'
    pit_list_ses1['ses'] <- 'ses-1'
    pit_list_ses1['ses_folder'] <- 'visit3-4'

    pit_list_ses2 <- as.data.frame(list.files(path = Sys.glob(file.path(data_path, 'pit_task', 'visit5')), pattern = '*Food-PIT*'))
    names(pit_list_ses2) <- 'filename'
    pit_list_ses2['ses'] <- 'ses-2'
    pit_list_ses2['ses_folder'] <- 'visit5'

    # combine
    pit_list <- rbind.data.frame(pit_list_ses1, pit_list_ses2)

    #get list of subject IDs
    pit_list[['id']] <- as.numeric(sapply(pit_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))[1]-1), simplify = TRUE))

    pit_list[['sub_str']] <- sapply(pit_list[['id']], function(x) sprintf('sub-%03d', x), simplify = TRUE)

    # reduce to 1 line per participant
    pit_list <- pit_list[grepl('psydat', pit_list[['filename']]), ]

    #organize data into BIDS sourcedata
    pit_list[['sourcedata_done']] <- mapply(util_copy_to_source, sub_id = pit_list[['id']], sub_str = pit_list[['sub_str']], task_dir = file.path(data_path, 'pit_task', pit_list[['ses_folder']]), ses_str = pit_list[['ses']], MoreArgs = list(base_wd = base_wd, task_str = 'pit', overwrite = overwrite))

    #process raw data
    pit_list[['rawdata_done']] <- mapply(util_task_pit, sub_str = pit_list[['sub_str']], ses_str = pit_list[['ses']], MoreArgs = list(base_wd = base_wd, overwrite = overwrite))

    #generate json file for rawdata
    pit_json <- json_pit()

    pit_filename_json <- file.path(bids_wd, 'task-pit_events.json')

    if ( isTRUE(overwrite) | !file.exists(pit_filename_json) ) {
      write(pit_json, pit_filename_json)
    }

  }

  #### RRV ####
  if (task_list == 'all' | 'rrv' %in% task_list) {

    print('-- copying RRV')

    # get list of subject directories

    rrv_list <- list.files(path = Sys.glob(file.path(data_path, 'rrv_task', 'reach*')), pattern = '.txt')

    # remove practice
    rrv_list <- as.data.frame(rrv_list[!grepl('prac', rrv_list)])

    names(rrv_list) <- 'filename'

    #get list of subject IDs
    rrv_list[['id']] <- as.numeric(sapply(rrv_list[['filename']], function(x) substr(x, unlist(gregexpr('_', x))+1, unlist(gregexpr('\\.', x))-1), simplify = TRUE))

    rrv_list[['sub_str']] <- sapply(rrv_list[['id']], function(x) sprintf('sub-%03d', as.numeric(x)), simplify = TRUE)


    #organize data into BIDS sourcedata
    rrv_list[['sourcedata_done']] <- mapply(util_copy_to_source, sub_id = rrv_list[['id']], sub_str = rrv_list[['sub_str']], task_dir = file.path(data_path, 'rrv_task', paste0('reach_', sprintf('%03d', rrv_list[['id']]))), MoreArgs = list(base_wd = base_wd, task_str = 'rrv', ses_str = 'ses-1', overwrite = overwrite))

    #process raw data
    rrv_list[['rawproc_done']] <- sapply(rrv_list[['sub_str']], function(x) util_task_rrv(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, overwrite = overwrite))

    #generate json file for rawdata
    rrv_json <- json_rrv()

    rrv_filename_json <- file.path(bids_wd, 'task-rrv_events.json')

    if ( isTRUE(overwrite) | !file.exists(rrv_filename_json) ) {
      write(rrv_json, rrv_filename_json)
    }
  }

  #### Stop Signal Task ####
  if (task_list == 'all' | 'sst' %in% task_list) {
    print('-- copying SST')

    # get list of available subjects
    sst_list <- list.files(path = Sys.glob(file.path(data_path, 'sst_task', 'reach*')), pattern = '.txt')

    sst_list <- sst_list[(!grepl('1st', sst_list))]

    sst_list <- as.data.frame(sst_list[(grepl('prac', sst_list))])
    names(sst_list) <- 'filename'


    #get list of subject IDs
    sst_list[['id']] <- sapply(sst_list[['filename']], function(x) substr(x, unlist(gregexpr('-', x))+1, unlist(gregexpr('.txt', x))-1), simplify = TRUE)

    sst_list[['sub_str']] <- sapply(sst_list[['id']], function(x) sprintf('sub-%03d', as.numeric(x)), simplify = TRUE)

    sst_list['id'] <- as.numeric(sst_list[['id']])

    #organize data into BIDS sourcedata
    sst_list[['sourcedata_done']] <- mapply(util_copy_to_source, sub_id = sst_list[['id']], sub_str = sst_list[['sub_str']], task_dir = file.path(data_path, 'sst_task', paste0('reach_', sprintf('%03d', sst_list[['id']]))), MoreArgs = list(base_wd = base_wd, task_str = 'stop', ses_str = 'ses-1', overwrite = overwrite))

    #process raw data
    sst_list[['rawproc_done']] <- sapply(sst_list[['sub_str']], function(x) util_task_sst(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)


    #generate json file for rawdata
    sst_json_prescan <- json_sst_prescan()
    sst_filename_json_prescan <- file.path(bids_wd, 'task-sst_acq-prescan_events.json')
    sst_filename_json_prac <- file.path(bids_wd, 'task-sst_acq-practice_events.json')

    sst_json_fmri <- json_sst_bold()
    sst_filename_json_fmri <- file.path(bids_wd, 'task-sst_acq-practice_events.json')

    if ( isTRUE(overwrite) | !file.exists(sst_filename_json_prescan) ) {
      write(sst_json_prescan, sst_filename_json_prescan)
      write(sst_json_prescan, sst_filename_json_prac)
      write(sst_json_fmri, sst_filename_json_fmri)
    }
  }


  #### Space Game ####
  if (task_list == 'all' | 'spacegame' %in% task_list) {
    print('-- copying Space Game')

    # Get list of subs with spacegame files in untouchedRaw based on filenames
    space_dir <- file.path(data_path, 'space_game') # set spacegame dir

    # get list of available subjects
    space_list <- as.data.frame(list.files(space_dir, pattern = 'mbmfNovelStakes', full.names = FALSE))
    names(space_list) <- 'filename'

    #get list of subject IDs
    space_list[['id']] <- sapply(space_list[['filename']], function(x) ifelse(grepl('026cancelled', x), '26-1', ifelse(grepl('059-01', x), '059', ifelse(grepl('069-09', x), '069', ifelse(grepl('117-09', x), '117', substr(x, unlist(gregexpr('_', x))[1]+1, unlist(gregexpr('-', x))[1] -1))))), simplify = TRUE)

    space_list[['sub_str']] <- sapply(space_list[['id']], function(x) ifelse(grepl('26-1', x), 'sub-026-1', paste0('sub-', x)), simplify = TRUE)

    space_list[space_list['id'] == '26-1', 'id'] <- '26'

    space_list['id'] <- as.numeric(space_list[['id']])

    #organize data into BIDS sourcedata
    space_list[['sourcedata_done']] <- sapply(space_list[['id']], function(x) util_copy_to_source(task_dir = space_dir, task_str = 'mbmfNovelStakes', sub_id = x, sub_str = sprintf('sub-%03d', x), ses_str = 'ses-1', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)

    #process raw data
    space_list[!grepl('26-1', space_list[['sub_str']]), 'rawproc_done'] <- sapply(space_list[!grepl('26-1', space_list[['sub_str']]), 'sub_str'], function(x) util_task_spacegame(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, overwrite = overwrite), simplify = TRUE)

    #generate json file for rawdata
    space_json <- json_spacegame_events()

    space_filename_json <- file.path(bids_wd, 'task-spacegame_events.json')

    if ( isTRUE(overwrite) | !file.exists(space_filename_json) ) {
      write(space_json, space_filename_json)
    }

  }

}
