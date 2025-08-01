#' proc_actigraph: Process raw data actigraph data
#'
#' This function calls task scripts that:
#' \itemize{
#'  \item{1) processes raw actigraph data by session using GGIR (modes 1-5) with activity thresholds set according to: }
#'  \itemize{
#'    \item{Hildebrand M, VAN Hees VT, Hansen BH, Ekelund U. Age group comparability of raw accelerometer output from wrist- and hip-worn monitors. Med Sci Sports Exerc. 2014 Sep;46(9):1816-24. doi: 10.1249/MSS.0000000000000289. (\href{https://pubmed.ncbi.nlm.nih.gov/24887173/}{PubMed})}
#'    \item{Hildebrand M, Hansen BH, van Hees VT, Ekelund U. Evaluation of raw acceleration sedentary thresholds in children and adults. Scand J Med Sci Sports. 2017 Dec;27(12):1814-1823. doi: 10.1111/sms.12795. Epub 2016 Nov 22. PMID: 27878845.(\href{https://pubmed.ncbi.nlm.nih.gov/27878845/}{PubMed})}
#'  }
#'  \item{completes post-processing with mMARCH.AC:}
#'  \itemize{
#'    \item{Guo, W., Leroux, A., Shou, H., Cui, L., Kang, S. J., Strippoli, M. F., Preisig, M., Zipunnikov, V., & Merikangas, K. R. (2023). Processing of Accelerometry Data with GGIR in Motor Activity Research Consortium for Health. Journal for the Measurement of Physical Behaviour, 6(1), 37-44. Retrieved Feb 14, 2025, from https://doi.org/10.1123/jmpb.2022-0018}
#'  }
#'  \item{computes Sleep Regularity Index scores using sleepreg:}
#'  \itemize{
#'    \item{Windred DP, Jones SE, Russell A, Burns AC, Chan P, Weedon MN, Rutter MK, Olivier P, Vetter C, Saxena R, Lane JM, Cain SW, Phillips AJK. Objective assessment of sleep regularity in 60 000 UK Biobank participants using an open-source package. Sleep. 2021 Dec 10;44(12):zsab254. doi: 10.1093/sleep/zsab254. PMID: 34748000.(\href{https://pubmed.ncbi.nlm.nih.gov/34748000/}{PubMed})}
#'    \item{Windred DP, Burns AC, Lane JM, Saxena R, Rutter MK, Cain SW, Phillips AJK. Sleep regularity is a stronger predictor of mortality risk than sleep duration: A prospective cohort study. Sleep. 2024 Jan 11;47(1):zsad253. doi: 10.1093/sleep/zsad253. PMID: 37738616; PMCID: PMC10782501.(\href{https://pubmed.ncbi.nlm.nih.gov/37738616/}{PubMed})}
#'  }
#' }
#'
#' NOTE: to get GGIR to work with sleep log coded in ActiLife by trained RAs, d1_wakeup needed to be set to an arbitrary time (06:00:00). We do not have wakeup time for day 1 because actigraph was applied mid-day. Addid this time so we didn't loose night 1 sleep.
#'
#'
#' @inheritParams proc_tasks
#' @inheritParams util_copy_to_source
#' @param overwrite_ggir_derivs (logical) whether to re-process and overwrite all GGIR derivative outputs (this is a time intensive process). Default = FALSE.
#'
#' @return data.frame for each task with status for each processing step
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' proc_tasks_pardat <- proc_tasks(base_wd, overwrite)
#'
#' \dontrun{
#' }
#'
#'
#' @export

proc_actigraph <- function(base_wd, overwrite = FALSE, overwrite_ggir_derivs = FALSE) {

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


  ## 2. copy to sourcedata ####
  # get raw file paths
  actigraph_gt3x_path <- file.path(base_wd, 'actigraph', 'raw', 'gt3x_files')
  actigraph_agd_path <- file.path(base_wd, 'actigraph', 'raw', 'agd_files')

  # get list of available subjects
  actigraph_gt3x_list <- list.files(actigraph_gt3x_path, pattern = '.gt3x')
  actigraph_gt3x_list <- as.data.frame(actigraph_gt3x_list)
  names(actigraph_gt3x_list) <- 'filename'


  actigraph_agd_list <- list.files(actigraph_agd_path, pattern = '.agd')
  actigraph_agd_list <- as.data.frame(actigraph_agd_list)
  names(actigraph_agd_list) <- 'filename'


  #get list of subject IDs
  actigraph_gt3x_list[['id']] <- as.numeric(sapply(actigraph_gt3x_list[['filename']], function(x) substr(x, unlist(gregexpr('_', x))+1, unlist(gregexpr('.gt3x', x))-1), simplify = TRUE))
  actigraph_gt3x_list[['file_sub_str']] <- sapply(actigraph_gt3x_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('.gt3x', x))-1), simplify = TRUE)

  actigraph_agd_list[['id']] <- as.numeric(sapply(actigraph_agd_list[['filename']], function(x) substr(x, unlist(gregexpr('_', x))[1]+1, unlist(gregexpr('_', x))[2]-1), simplify = TRUE))
  actigraph_agd_list[['file_sub_str']] <- sapply(actigraph_agd_list[['filename']], function(x) substr(x, 1, unlist(gregexpr('_', x))[2]-1), simplify = TRUE)

  # combine into a single data frame
  actigraph_list <- rbind.data.frame(actigraph_gt3x_list, actigraph_agd_list)

  actigraph_list[['sub_str']] <- sapply(actigraph_list[['id']], function(x) sprintf('sub-%03d', x), simplify = TRUE)

  #organize data into BIDS sourcedata
  actigraph_list[grepl('.agd', actigraph_list[['filename']]), 'sourcedata_done'] <- sapply(actigraph_list[grepl('.agd', actigraph_list[['filename']]), 'id'], function(x) util_copy_to_source(task_dir = actigraph_agd_path, task_str = 'actigraph', sub_id = x, sub_str = sprintf('sub-%03d', x), ses_str = 'ses-1', file_pattern = '.agd', overwrite = overwrite), simplify = TRUE)

  actigraph_list[grepl('gt3x', actigraph_list[['filename']]), 'sourcedata_done'] <- sapply(actigraph_list[grepl('gt3x', actigraph_list[['filename']]), 'id'], function(x) util_copy_to_source(task_dir = actigraph_gt3x_path, task_str = 'actigraph', sub_id = x, sub_str = sprintf('sub-%03d', x), ses_str = 'ses-1', file_pattern = '.gt3x', overwrite = overwrite), simplify = TRUE)


  ## 3. get sleep log scored and format correctly ####
  # get raw file paths
  sleeplog_filepath <- file.path(base_wd, 'actigraph', 'raw', 'actigraph_sleepscore_ses1.csv')

  # read data
  sleeplog_dat <- read.csv(sleeplog_filepath, header = TRUE)

  sleeplog_dat_format <- util_format_actigraph_sleeplog(sleeplog_dat)

  # add NAs for phenotype database
  sleeplog_dat_format_pheno <- sapply(names(sleeplog_dat_format), function(x) ifelse(sleeplog_dat_format[[x]] == '', NA, sleeplog_dat_format[[x]]))

  sleeplog_pheno_filename <- file.path(base_wd, 'bids', 'phenotype', 'actigraph_sleeplog_scored.tsv')

  # write tsv
  if ( isTRUE(overwrite) | !file.exists(sleeplog_pheno_filename) ) {
    # use 'n/a' for missing values for BIDS compliance
    write.table(sleeplog_dat_format_pheno, sleeplog_pheno_filename, sep='\t', quote = FALSE, row.names = FALSE, na = 'n/a')
  }

  # export for GGIR use
  deriv_dir_ggir <- file.path(base_wd, 'bids', 'derivatives', 'motion', 'ses-1')

  #change ID to match filenames
  sleeplog_dat_format['participant_id'] <- paste0(sleeplog_dat_format[['participant_id']], '_ses-1_motion.gt3x')

  # add random waketime to day 1 (pre-recording)
  sleeplog_dat_format['d1_wakeup'] <- '06:00:00'

  if (!dir.exists(deriv_dir_ggir)) {
    dir.create(deriv_dir_ggir, recursive = TRUE)
  }

  sleeplog_ggir_filename <- file.path(deriv_dir_ggir, 'actigraph_sleeplog_scored.csv')

  # write csv
  if ( isTRUE(overwrite) | !file.exists(sleeplog_ggir_filename) ) {
    write.csv(sleeplog_dat_format, sleeplog_ggir_filename, row.names = FALSE)
  }

  ## 4. GGIR ####

  # get lit of files
  sourcepath_list <- file.path(base_wd, 'bids', 'sourcedata', actigraph_list[grepl('gt3x', actigraph_list[['filename']]), 'sub_str'], 'ses-1', 'motion')

  source_data_files <-  list.files(path = sourcepath_list, pattern = '.gt3x')

  ggir_data_list <- file.path(sourcepath_list, source_data_files)

  # process data
  # ggir_data <- util_actigraph_ggir(data_list = ggir_data_list, deriv_dir = deriv_dir_ggir, study_name = 'reach', overwrite = overwrite_ggir_derivs, sleepwindowType = "TimeInBed", loglocation = sleeplog_ggir_filename, colid = 1, part5_agg2_60seconds = TRUE, part6CR = TRUE)

  ggir_data <- util_actigraph_ggir(data_list = ggir_data_list, deriv_dir = deriv_dir_ggir, study_name = 'reach', overwrite = overwrite_ggir_derivs, sleepwindowType = "SPT", loglocation = c(), part5_agg2_60seconds = TRUE, part6CR = TRUE)

  # 5. post-processing with mMRACH.AC ####
  deriv_dir_mMARCH <- file.path(base_wd, 'bids', 'derivatives', 'motion', 'ses-1', 'mMARCH')

  #make directory if needed
  if (!dir.exists(deriv_dir_mMARCH)) {
    dir.create(deriv_dir_mMARCH, recursive = TRUE)
  }

  ggir_path <- file.path(deriv_dir_ggir, 'ggir_output')

  filename2id <- function(filename) {
    newID <- substr(filename, 1, unlist(gregexpr('_', filename))[1]-1)
    return(as.character(newID))
  }

  #run set-up call
  #sapply(0:4, function(x) util_actigraph_mMARCH(mode = x, deriv_dir = deriv_dir_mMARCH, study_name = 'reach', data_list = ggir_data_list, ggir_path = ggir_path, filename2id = filename2id))

  ## organize data and write data ####
  #util_actigraph_clean(base_wd, metrics = c('SL', 'PA', 'CR'), overwrite)

}
