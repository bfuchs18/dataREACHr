#' write_microstructure: Write selected data and json files from processed microstructure data
#'
#' This function:
#' \itemize{
#'    \item{1) Calls proc_microstructure function to get clean and compiled data and metadata}
#'    \item{2) Exports all or select BIDS-compliant .tsv and .json files into bids/phenotype and/or bids/rawdata}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @inheritParams proc_tasks
#' @inheritParams proc_tasks
#' @inheritParams proc_microstructure
#' @param data_list list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'meal-boring' - meal microstructure behavior after boring commercials}
#'  \item{'meal-food' - meal microstructure behavior after food commercials}
#'  \item{'meal-toy' - meal microstructure behavior after toy commercials}
#'  \item{'eah-boring' - EAH microstructure behavior after boring commercials}
#'  \item{'eah-food' - EAH microstructure behavior after food commercials}
#'  \item{'eah-toy' - EAH microstructure behavior after toy commercials}
#' }
#' @param data_type list of strings matching the data types listed below. Defult = 'all' to export both:
#'  \itemize{
#'    \item{'beh_wide' - summary behavioral measures in wide formate by coder. Note: this will write out a summary dataset in bids/phenotype.}
#'    \item{'events_long' - event level data in log format by coder. Note: this writes out a file per participant into bids/rawdata.}
#'  }
#' @inheritParams write_tasks
#'
#' @return Does not return anything
#'
#'
#' @examples
#'
#' \dontrun{
#' write_microstructure(base_wd, intake_data, overwrite = FALSE, data_list = 'all', data_type = 'all')
#'
#' }
#'
#' @export

write_microstructure <- function(base_wd, intake_data, overwrite = FALSE, data_list = 'all', data_type = 'all', return_data = FALSE) {

  #### Set up/initial checks #####

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

  # set paths for other directories
  bids_wd <- file.path(base_wd, 'bids')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')
  micro_raw_dir <- file.path(base_wd, 'bids', 'rawdata')

  # get microstructure data
  proc_micro_data <- proc_microstructure(base_wd, intake_data)

  # get 'all' list
  data_list_options <- c('meal-boring', 'meal-food', 'meal-toy', 'eah-boring', 'eah-food', 'eah-toy')
  data_type_options <- c('beh_wide', 'events_long')

  if (length(data_list) == 1){
    if (data_list == 'all'){
      data_list_use = data_list_options
    } else {
      data_list_use = data_list
    }
  } else {
    data_list_use = data_list
  }

  if (length(data_type) == 1){
    if (data_type == 'all'){
      data_type_use = data_type_options
    } else {
      data_type_use = data_type
    }
  } else {
    data_type_use = data_type
  }

  # 2. process/save behavioral data ####
  if ('beh_wide' %in% data_type_use){

    # loop through data_list and merge together
    d_count <- 0

    for (d in data_list_use) {
      data_str <- paste0(d, '_beh')

      data_list_subset <- proc_micro_data[grepl(data_str, names(proc_micro_data))]

      for (dat_name in names(data_list_subset)){
        d_count <- d_count + 1

        if (d_count == 1){
          merge_data <- proc_micro_data[[dat_name]]$data
          json_beh <- proc_micro_data[[dat_name]]$meta
        } else {
          merge_data <- rbind.data.frame(merge_data, proc_micro_data[[dat_name]]$data)
        }
      }
    }

    filename_tsv <- file.path(phenotype_wd, 'microstructure.tsv')
    filename_json <- file.path(phenotype_wd, 'microstructure.json')

    # write tsv
    if ( isTRUE(overwrite) | !file.exists(filename_tsv) ) {
      # use 'n/a' for missing values for BIDS compliance

      write.table(merge_data, filename_tsv, quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE, na = 'n/a')
    }

    # write json
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(json_beh, filename_json)
    }

  }


  # 3. process/save in events data for each participant ####
  if ('events_long' %in% data_type_use){

    # add if statement to check for file/overwrite option
    raw_save <- function(micro_data, id, micro_raw_dir, ses_str, commercial_cond, paradigm, overwrite){

      # get directory and check it existis
      micro_sub_dir <- file.path(micro_raw_dir, id, ses_str, 'video')

      if (!dir.exists(micro_sub_dir)) {
        dir.create(micro_sub_dir, recursive = TRUE)
      }

      # save file path
      save_file_path <- file.path(micro_sub_dir, paste0(id, '_', ses_str, '_', paradigm, '-micro_commercial-', commercial_cond, '_events.tsv'))

      # check if file exists or should overwrite

      file_exists <- file.exists(save_file_path)

      if (isFALSE(file_exists) || isTRUE(overwrite)){
        data <- micro_data[micro_data['participant_id'] == id, ]

        write.table(data, file = save_file_path, sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')

        return(paste0(id, ' raw saved'))

      } else {
        return(paste0(id, ' raw not overwritten'))
      }
    }


    # loop through data_list and to run save function for events
    for (d in data_list_use) {
      data_str <- paste0(d, '_events')

      data_list_subset <- proc_micro_data[grepl(data_str, names(proc_micro_data))]

      for (dat_name in names(data_list_subset)){

        ses_str <- ifelse(grepl('ses-1', dat_name), 'ses-1', 'ses-2')
        commercial_cond <- ifelse(grepl('boring', dat_name), 'boring', ifelse(grepl('food', dat_name), 'food', 'toy'))
        paradigm <- ifelse(grepl('meal', dat_name), 'meal', 'eah')

        # export dataset_description.json
        filename_json <- file.path(bids_wd, paste0(paradigm, '-micro_commercial-', commercial_cond, '_events.json'))

        json_events <- proc_micro_data[[dat_name]]$meta

        if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
          write(json_events, filename_json)
        }

        micro_data_long <- proc_micro_data[[dat_name]]$data

        save_msg <- sapply(unique(micro_data_long[['participant_id']]), function(x) raw_save(micro_data = micro_data_long, id = x, micro_raw_dir, ses_str, commercial_cond, paradigm, overwrite))
      }
    }
  }

  #### Return Data ####
  if (isTRUE(return_data)) {
    return(proc_micro_data)
  }
}

