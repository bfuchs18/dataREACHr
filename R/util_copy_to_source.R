#' util_copy_to_source: A function to copy a file into sourcedata
#'
#' @param base_wd (string) full path to directory that contains both the untouchedRaw and bids directories
#' @param task_dir (string) path to directory with all task files to be copied into sourcedata
#' @param task_str (string) task string used in filenaming (e.g., 'foodview')
#' @param sub_str (string) bids-formatted session string. e.g., 'sub-001'
#' @param sub_id (numeric) participant number
#' @param ses_str (string) bids-formatted session string. e.g., 'ses-1'
#' @param sourcefile_prefix (string) string to prefix filename with in sourcedata (optional)
#' @param file_pattern (string) sting to identify files (optional)
#' @param overwrite (logical) logical indicating whether file should be overwritten
#'
#' @examples
#' util_copy_to_source(task_dir = foodview_dir, task_str = 'foodview', sub_str = 'sub-001', ses_str = 'ses-1', overwrite = TRUE)
#'
#' @export
#'


util_copy_to_source <- function(base_wd, task_dir, task_str, sub_str, ses_str, sub_id, sourcefile_prefix, file_pattern, overwrite) {

  # check for sourcefile_prefix arg
  prefix_arg <- methods::hasArg(sourcefile_prefix)

  # get files
  if (grepl('foodview', task_str)) {
    raw_files <- list.files(path = task_dir, pattern = paste0(sub_id, '.txt'), recursive = TRUE)
  } else if (grepl('mbmfNovelStakes', task_str)) {
    raw_files <- basename(Sys.glob(file.path(task_dir, paste0(task_str, '_', sprintf('%03d', sub_id), '*'))))
  } else if (!grepl('actigraph|rrv|pit', task_str)) {
    raw_files <- list.files(path = task_dir, pattern = task_str)
  }

  # food view ####
  if (task_str == 'foodview'){
    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func')

    raw_files <- raw_files[grepl(paste0('-', sprintf('%03d', sub_id), '.txt'), raw_files)]

    raw_files_short <- basename(raw_files)
    raw_files_short <- substr(raw_files_short, 1, unlist(gregexpr('-', raw_files_short))-1)

    rename_files <- paste0(sub_str, '_', ses_str, '_task-', raw_files_short, '.tsv')
  }


  # Space Game ####
  if (task_str == 'mbmfNovelStakes'){
    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

    raw_files <- raw_files[grepl(sprintf('%03d', sub_id), raw_files)]
    raw_files <- raw_files[!grepl('cancel', raw_files)]

    raw_files_uscore <- unlist(gregexpr('_', raw_files))

    raw_files_short <- substr(raw_files, 1, raw_files_uscore[1]-1)

    rename_files <- paste0(sub_str, '_', ses_str, '_task-', gsub('mbmfNovelStakes', 'space', raw_files_short), '.mat')

    if (sub_id == 26){
      rename_files <- gsub('cance', '-cancel', rename_files)
    }
  }

  # NIH Toolbox ####
  if (task_str == 'nih'){
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')


    raw_files <- list.files(path = task_dir, pattern = '.csv')

    raw_files_short <- sapply(raw_files, function(x) substr(x, unlist(gregexpr('_', x))[1]+1, nchar(x)), simplify = TRUE, USE.NAMES = FALSE)

    raw_files_short <- gsub('assessment_', '', raw_files_short)

    rename_files <- paste0(sub_str, '_', ses_str, '_task-nih_toolbox_', raw_files_short)

    rename_files <- gsub('.csv', '.tsv', rename_files)
  }

  # PIT ####
  if (task_str == 'pit'){

    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

    sub_id_str <- sprintf('%03d', sub_id)
    raw_files <- list.files(path = task_dir, pattern = paste0(sub_id_str, '_Food-PIT*'))

    # output warning if .csv not found
    if (sum(grepl('.csv', raw_files)) == 0) {
      print(paste('WARNING: subject', sub_str, ' ', ses_str, 'has PIT output files but no csv' ))
    }

    raw_files_short <- substr(raw_files, tail(unlist(gregexpr('\\.', raw_files[1])), 1), nchar(raw_files))

    rename_files <- paste0(sub_str, '_', ses_str, '_task-pit', raw_files_short)

    rename_files <- gsub('.csv', '.tsv', rename_files)
    rename_files <- gsub('.txt', '.tsv', rename_files)
  }

  # RRV ####
  if (task_str == 'rrv'){

    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'beh')

    # convert .txt files
    raw_files_txt <- list.files(path = task_dir, pattern = '.txt')
    raw_files_path <- file.path(task_dir, raw_files_txt)

    csv_dataframes <- sapply(raw_files_path, function(x) util_rrv_parse_text(rrv_file = x, sub_str = sub_str), simplify = FALSE)

    raw_files_short <- sapply(raw_files_txt, function(x) substr(x, unlist(gregexpr(sub_id, x))+nchar(sub_id), nchar(x)), simplify = TRUE, USE.NAMES = FALSE)

    raw_files_short <- gsub('-prac|_prac', '_desc-prac', raw_files_short)
    rename_files_txt <- paste0(sub_str, '_', ses_str, '_task-rrv', raw_files_short)

    rename_files <- gsub('.txt', '.tsv', rename_files_txt)

  }

  # SST ####
  if (task_str == 'stop'){
    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'func')

    raw_files_short <- substr(raw_files, 1, unlist(gregexpr('-', raw_files))-1)

    if (sub_id == 37){
      raw_files_short[grepl('stop_prac', raw_files_short)] <- c('sst_prac1', 'sst_prac2')
    }

    rename_files <- paste0(sub_str, '_', ses_str, '_task-', gsub('stop', 'sst', raw_files_short), '.tsv')
  }

  # Actigraph ####
  if (task_str == 'actigraph'){

    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'bids', 'sourcedata', sub_str, ses_str, 'motion')

    raw_files <- list.files(path = task_dir, pattern = file_pattern)

    raw_files <- raw_files[grepl(paste0('REACH_', sprintf('%03d', sub_id)), raw_files)]

    rename_files <- gsub('REACH_', 'sub-', raw_files)

    if (file_pattern == '.agd'){
      rename_files <- gsub(paste0(sub_str, '_'), paste0(sub_str, '_', ses_str, '_motion-'), rename_files)
    } else {
      rename_files <- gsub(sub_str, paste0(sub_str, '_', ses_str, '_motion'), rename_files)
    }
  }


  # Save ####
  # set sourcedata file
  source_paths <- file.path(sub_task_source_dir, rename_files)

  if (task_str == 'stop'){
    source_paths[!grepl('fmri|onsets', source_paths)] <- gsub('func', 'beh', source_paths[!grepl('fmri|onsets', source_paths)])
  }

  if (sum(file.exists(source_paths[1])) != length(source_paths) | isTRUE(overwrite)) {

    # create sub_task_source_dir if it doesnt exist
    if (!dir.exists(sub_task_source_dir)) {
      dir.create(sub_task_source_dir, recursive = TRUE)
    }

    # copy files
    save_fun <- function(file_path, source_path){

      if (grepl('csv', file_path)){
        dat_read <- read.csv(file_path, header = TRUE)
      }

      if (grepl('txt', file_path)){
        dat_read <- read.table(file_path, header = TRUE)
      }

      write.table(dat_read, source_path, sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
    }

    if (task_str %in% c('foodview', 'stop', 'nih')){
      mapply(save_fun, file_path=file.path(task_dir, raw_files), source_path = source_paths)
    } else if (task_str %in% c('mbmfNovelStakes', 'actigraph')){
      # copy file into sub_task_source_dir
      file.copy(from = file.path(task_dir, raw_files), to = source_paths, overwrite = overwrite)
    } else if (task_str %in% c('pit')){
      if (sum(grepl('.csv', raw_files)) > 0){
        save_fun(file_path=file.path(task_dir, raw_files[grepl('.csv', raw_files)]), source_path = source_paths[grepl('.tsv', source_paths)])
      }

      file.copy(from = file.path(task_dir, raw_files[!grepl('.csv', raw_files)]), to = source_paths[!grepl('.tsv', source_paths)], overwrite = overwrite)
    } else if (task_str == 'rrv') {

      # write proccessed data
      save_rrv_fun <- function(source_path, proc_data){
        write.table(proc_data, source_path, sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
      }

      mapply(save_rrv_fun, source_path = source_paths, proc_data = csv_dataframes)

      file.copy(from = raw_files_path, to = file.path(sub_task_source_dir, rename_files_txt), overwrite = overwrite)
    }

    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }

  } else {
    return('exists')
  }
}
