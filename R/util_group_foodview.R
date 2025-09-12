#' util_group_foodview: Get summary data from the Food Rating task
#'
#' This function calculates summary performance data and saves the output
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param data_list A data frame with variable 'sub_str' that includes all participants that have task data in rawdata
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @param return logical indicating if computed summary data should be returned. Default = FALSE
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Rating Task
#' group_foodview_data <- util_group_foodview(data_list, ses, base_wd, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_foodview <- function(data_list, ses_str, base_wd, overwrite = FALSE, return_data = FALSE) {


  #### Check args #####

  # check that audit_data exist and is a data.frame
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

  #### Participant Summary Function #####
  sum_database_fn <- function(sub_str, ses, base_wd, format){

    #print(sub_str)

    # get directory paths
    raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses, 'func')

    # load all run files
    data_files <- as.data.frame(Sys.glob(file.path(raw_wd, paste0(sub_str, '_', ses, '_task-foodview_*'))))
    names(data_files) <- 'file_path'

    dat <- do.call(rbind.data.frame, t(sapply(data_files[['file_path']], function(x) read.table(x, sep='\t', header = TRUE, na.strings = 'n/a'), simplify = FALSE)))

    if (format == 'wide'){
      sum_dat <- util_foodview_summary(dat, 'wide')

    } else {
      dat['block_str'] <- paste0(dat[['run_num']], '_', dat[['commercial_cond']], '_', dat[['food_ed']], '_', dat[['food_taste']])

      sum_dat <- do.call(rbind.data.frame, t(sapply(unique(dat[['block_str']]) , function(x) util_foodview_summary(dat[dat['block_str'] == x, ], 'long'), simplify = FALSE)))

      #print(sub_str)
      sum_dat['block_num'] <- c(unlist(sapply(unique(sum_dat[['run_num']]), function(x) seq(1, nrow(sum_dat[sum_dat['run_num'] == x, ])))))

      sum_dat <- sum_dat[c('participant_id', 'session_id', 'run_num', 'block_num', names(sum_dat)[!grepl('id|num', names(sum_dat))])]

    }

    return(as.data.frame(sum_dat))
  }

  #### Save in derivatives #####
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives', 'func-beh')

  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }

  ## Wide/Overall Data ####
  if (!file.exists(file.path(deriv_wd, 'task-foodview_desc-bycond_beh.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'ses-1', base_wd = base_wd, format = 'wide'), simplify = FALSE, USE.NAMES = TRUE)))

    sum_database[!grepl('_id|cond', names(sum_database))] <- sapply(sum_database[!grepl('_id|cond', names(sum_database))], function(x) round(as.numeric(x), 3))

    sum_database[grepl('_id|cond', names(sum_database))] <- sapply(sum_database[grepl('_id|cond', names(sum_database))], function(x) as.character(x))

    write.table(as.data.frame(sum_database), file.path(deriv_wd, 'task-foodview_desc-bycond_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    foodview_bycond_json <- json_foodview_bycond()

    foodview_bycond_filename_json <- file.path(deriv_wd, 'task-foodview_desc-bycond_beh.json')

    if ( isTRUE(overwrite) | !file.exists(foodview_bycond_filename_json) ) {
      write(foodview_bycond_json, foodview_bycond_filename_json)
    }

  }

  ## Long Data ####
  if (!file.exists(file.path(deriv_wd, 'task-foodview_desc-byblock_beh.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database_long <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses = 'ses-1', base_wd = base_wd, format = 'long'), simplify = FALSE, USE.NAMES = TRUE)))


    sum_database_long[!grepl('_id|cond|food', names(sum_database_long))] <- sapply(sum_database_long[!grepl('_id|cond|food', names(sum_database_long))], function(x) round(as.numeric(x), 3))

    sum_database_long[grepl('_id|cond|food', names(sum_database_long))] <- sapply(sum_database_long[grepl('_id|cond|food', names(sum_database_long))], function(x) as.character(x))

    write.table(as.data.frame(sum_database_long), file.path(deriv_wd, 'task-foodview_desc-byblock_beh.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    foodview_byblock_json <- json_foodview_byblock()

    foodview_byblock_filename_json <- file.path(deriv_wd, 'task-foodview_desc-byblock_beh.json')

    if ( isTRUE(overwrite) | !file.exists(foodview_byblock_filename_json) ) {
      write(foodview_byblock_json, foodview_byblock_filename_json)
    }

  }

  if (isTRUE(return_data)){
    foodview_data <- list(bycond = list(data = sum_database, meta = foodview_bycond_json),
                          byblock = list(data = sum_database_long, meta = foodview_byblock_json))

    return(foodview_data)
  }

}

