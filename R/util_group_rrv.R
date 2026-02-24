#' util_group_rrv: Get summary data from the Reinforcing Value of Food task
#'
#' This function calculates summary performance data and saves the output
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param data_list A data frame with variable 'sub_str' that includes all participants that have task data in rawdata
#' @inheritParams util_copy_to_source
#' @inheritParams util_copy_to_source
#' @inheritParams util_group_foodview
#'
#' @return If return_data is set to TRUE, will return a list including a clean raw dataset with meta-data
#'
#' @examples
#'
#' # process task data for the Food Rating Task
#' group_rrv_data <- util_group_rrv(data_list, base_wd, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_rrv <- function(data_list, base_wd, overwrite = FALSE, return_data = FALSE) {

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
  sum_database_fn <- function(sub_str, ses_str, base_wd, format){

   # print(sub_str)

    # get directory paths
    raw_wd <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'beh')

    # load all run files
    data_file <- Sys.glob(file.path(raw_wd, paste0(sub_str, '_', ses_str, '_task-rrv_*')))

    dat <- read.table(data_file, sep='\t', header = TRUE, na.strings = 'n/a')


    sum_dat <- util_rrv_summary(dat, format)


    return(as.data.frame(sum_dat))
  }

  #### Save in derivatives #####
  deriv_wd <- file.path(base_wd, 'bids', 'phenotype')

  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }

  ## Overall Data ####
  if (!file.exists(file.path(deriv_wd, 'rrv.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, format = 'wide'), simplify = FALSE)))

    sum_database[!grepl('_id', names(sum_database))] <- sapply(sum_database[!grepl('_id', names(sum_database))], function(x) round(as.numeric(x), 3))

    sum_database[grepl('_id', names(sum_database))] <- sapply(sum_database[grepl('_id', names(sum_database))], function(x) as.character(x))

    write.table(as.data.frame(sum_database), file.path(deriv_wd, 'rrv.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    rrv_json <- json_rrv_summary()

    rrv_filename_json <- file.path(deriv_wd, 'rrv.json')

    if ( isTRUE(overwrite) | !file.exists(rrv_filename_json) ) {
      write(rrv_json, rrv_filename_json)
    }

  }

  ## Long Data ####
  if (!file.exists(file.path(deriv_wd, 'rrv_desc-long.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database_long <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, format = 'long'), simplify = FALSE)))

    sum_database_long[!grepl('_id|schedule|^reinforcer', names(sum_database_long))] <- sapply(sum_database_long[!grepl('_id|schedule|^reinforcer', names(sum_database_long))], function(x) round(as.numeric(x), 3))

    sum_database_long[grepl('_id|schedule|^reinforcer', names(sum_database_long))] <- sapply(sum_database_long[grepl('_id|schedule|^reinforcer', names(sum_database_long))], function(x) as.character(x))

    write.table(as.data.frame(sum_database_long), file.path(deriv_wd, 'rrv_desc-long.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    rrv_long_json <- json_rrv_long_summary()

    rrv_long_filename_json <- file.path(deriv_wd, 'rrv_desc-long.json')

    if ( isTRUE(overwrite) | !file.exists(rrv_long_filename_json) ) {
      write(rrv_long_json, rrv_long_filename_json)
    }

  }

  if (isTRUE(return_data)){
    rrv_data <- list(wide_data = list(data = sum_database, meta = rrv_json),
                     long_data = list(data = sum_database_long, meta = rrv_long_json))

    return(rrv_data)
  }



}

