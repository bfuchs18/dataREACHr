#' util_group_sst: Get summary data from the Stop Signal Task
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
    dat <- as.data.frame(dat)

    sum_dat <- util_sst_summary(dat, format)


    return(sum_dat)
  }


  #### Save in derivatives #####
  deriv_wd <- file.path(base_wd, 'bids', 'derivatives', 'beh')

  if (!dir.exists(deriv_wd)) {
    dir.create(deriv_wd, recursive = TRUE)
  }

  ## Overall Data ####
  if (!file.exists(file.path(deriv_wd, 'sst.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, format = 'wide'), simplify = FALSE)))

    sum_database[!grepl('_id|trial_type', names(sum_database))] <- sapply(sum_database[!grepl('_id|trial_type', names(sum_database))], function(x) round(as.numeric(x), 3))

    sum_database[grepl('_id|trial_type', names(sum_database))] <- sapply(sum_database[grepl('_id|trial_type', names(sum_database))], function(x) as.character(x))

    write.table(as.data.frame(sum_database), file.path(deriv_wd, 'sst.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    wide_sum_json <- json_sst_summary()

    sst_filename_json <- file.path(deriv_wd, 'sst.json')

    if ( isTRUE(overwrite) | !file.exists(sst_filename_json) ) {
      write(wide_sum_json, sst_filename_json)
    }

  }

  ## Long Data - By Run ####
  if (!file.exists(file.path(deriv_wd, 'sst_desc-byrun.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database_byrun <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, format = 'byrun'), simplify = FALSE)))

    sum_database_byrun[!grepl('_id|trial_type|ad_cond', names(sum_database_byrun))] <- sapply(sum_database_byrun[!grepl('_id|trial_type|ad_cond', names(sum_database_byrun))], function(x) round(as.numeric(x), 3))

    sum_database_byrun[grepl('_id|trial_type|ad_cond', names(sum_database_byrun))] <- sapply(sum_database_byrun[grepl('_id|trial_type|ad_cond', names(sum_database_byrun))], function(x) as.character(x))

    write.table(as.data.frame(sum_database_byrun), file.path(deriv_wd, 'sst_desc-byrun.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    run_sum_json <- json_sst_byrun()

    sst_byrun_filename_json <- file.path(deriv_wd, 'sst_desc-byrun.json')

    if ( isTRUE(overwrite) | !file.exists(sst_byrun_filename_json) ) {
      write(run_sum_json, sst_byrun_filename_json)
    }

  }

  ## Long Data - By Block ####
  if (!file.exists(file.path(deriv_wd, 'sst_desc-byblock.tsv')) | isTRUE(overwrite)) {

    # generate summary database
    sum_database_byblock <- do.call(rbind.data.frame, t(sapply(data_list[['sub_str']], function(x) sum_database_fn(sub_str = x, ses_str = 'ses-1', base_wd = base_wd, format = 'byblock'), simplify = FALSE)))

    sum_database_byblock[!grepl('_id|trial_type|ad_cond|img_cat', names(sum_database_byblock))] <- sapply(sum_database_byblock[!grepl('_id|trial_type|ad_cond|img_cat', names(sum_database_byblock))], function(x) round(as.numeric(x), 3))

    sum_database_byblock[grepl('_id|trial_type|ad_cond|img_cat', names(sum_database_byblock))] <- sapply(sum_database_byblock[grepl('_id|trial_type|ad_cond|img_cat', names(sum_database_byblock))], function(x) as.character(x))

    write.table(as.data.frame(sum_database_byblock), file.path(deriv_wd, 'sst_desc-byblock.tsv'), sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

    #generate json file for derivative data
    block_sum_json <- json_sst_byblock()

    sst_byblock_filename_json <- file.path(deriv_wd, 'sst_desc-byblock.json')

    if ( isTRUE(overwrite) | !file.exists(sst_byblock_filename_json) ) {
      write(run_sum_json, sst_byblock_filename_json)
    }


  }

  return(list(sum_dat = sum_database,
              sum_dat_byrun = sum_database_byrun,
              sum_dat_byblock = sum_database_byblock))
}

