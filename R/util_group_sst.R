#' util_group_sst: Get summary data from the Stop Signal Task
#'
#' This function calculates summary performance data and saves the output
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the participant number.
#'
#' @param data_list A data frame with variable 'sub_str' that includes all participants that have task data in rawdata
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
#' group_sst_data <- util_group_sst(data_list, base_wd, return = TRUE)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_group_sst <- function(data_list, base_wd, overwrite = FALSE, return_data = FALSE) {

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
    raw_wd_beh <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'beh')
    raw_wd_func <- file.path(base_wd, 'bids', 'rawdata', sub_str, ses_str, 'func')

    # load all run files
    beh_data_file <- Sys.glob(file.path(raw_wd_beh, paste0(sub_str, '_', ses_str, '*prescan_events.tsv')))

    func_data_files <- as.data.frame(Sys.glob(file.path(raw_wd_func, paste0(sub_str, '_', ses_str, '*task-sst*'))))

    # load all run files
    names(func_data_files) <- 'file_path'

    func_dat <- do.call(rbind.data.frame, t(sapply(func_data_files[['file_path']], function(x) read.table(x, sep='\t', header = TRUE, na.strings = 'n/a'), simplify = FALSE)))

    beh_dat <- read.table(beh_data_file, sep='\t', header = TRUE, na.strings = 'n/a')

    # merge
    dat <- rbind(data.table::setDT(func_dat), data.table::setDT(beh_dat), fill = TRUE)
    dat <- as.data.frame(beh_dat)

    sum_dat <- util_sst_summary(dat, format)


    return(as.data.frame(sum_dat))
  }


  #### Save in derivatives #####
  deriv_wd <- file.path(base_wd, 'bids', 'phenotype')

  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }

  ## Overall Data ####
  if (!file.exists(file.path(deriv_wd, 'sst.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, format = 'wide'), simplify = FALSE)))

    sum_database[!grepl('_id', names(sum_database))] <- sapply(sum_database[!grepl('_id', names(sum_database))], function(x) round(as.numeric(x), 3))

    sum_database[grepl('_id', names(sum_database))] <- sapply(sum_database[grepl('_id', names(sum_database))], function(x) as.character(x))

    write.table(as.data.frame(sum_database), file.path(deriv_wd, 'sst.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    sst_json <- json_sst_summary()

    sst_filename_json <- file.path(deriv_wd, 'sst.json')

    if ( isTRUE(overwrite) | !file.exists(sst_filename_json) ) {
      write(sst_json, sst_filename_json)
    }

  }

  ## Long Data ####
  if (!file.exists(file.path(deriv_wd, 'sst_desc-long.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database_long <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, format = 'long'), simplify = FALSE)))

    sum_database_long[!grepl('_id|schedule|^reinforcer', names(sum_database_long))] <- sapply(sum_database_long[!grepl('_id|schedule|^reinforcer', names(sum_database_long))], function(x) round(as.numeric(x), 3))

    sum_database_long[grepl('_id|schedule|^reinforcer', names(sum_database_long))] <- sapply(sum_database_long[grepl('_id|schedule|^reinforcer', names(sum_database_long))], function(x) as.character(x))

    write.table(as.data.frame(sum_database_long), file.path(deriv_wd, 'sst_desc-long.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    sst_long_json <- json_sst_long_summary()

    sst_long_filename_json <- file.path(deriv_wd, 'sst_desc-long.json')

    if ( isTRUE(overwrite) | !file.exists(sst_long_filename_json) ) {
      write(sst_long_json, sst_long_filename_json)
    }

  }

  if (isTRUE(return_data)){
    sst_data <- list(wide_data = list(data = sum_database, meta = sst_json),
                     long_data = list(data = sum_database_long, meta = sst_long_json))

    return(sst_data)
  }



  return(list(summary_long_by_run = summary_byrun_df,
              summary_long_by_block = summary_byblock_df,
              summary_long_by_cond = summary_bycond_df))
}

