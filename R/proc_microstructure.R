#' proc_microstructure: Process raw microstructure coding data
#'
#'
#' This function loads the .txt raw data files from ObserverXT. Cleaning the data involves:
#' \itemize{
#'    \item{1) Reads ObserverXT data from OneDrive}
#'    \item{2) Calls util_ functions to clean and compile data in dataframes}
#'    \item{3) Calls json_ functions to create strings with meta-data stored in JSON format for each dataframe}
#'    \item{4) Compiles data repeated across visits and sessions}
#' }
#'
#'
#' @inheritParams proc_tasks
#' @param intake_data processed intake data from proc_redcap.R
#'
#' @examples
#' #if in same working directory as data:
#' microstructure <- proc_microstructure(base_wd_path)
#'
#' \dontrun{
#'
#' }
#'
#'
#' @export
#'
proc_microstructure <- function(base_wd, intake_data) {

  #### 1. Set up/initial checks #####

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

  # 2. Set up information ####

  # set paths for other directories
  micro_dir <- file.path(base_wd, 'noldus_video', 'behavioral-coding', 'observerxt_projects', 'observerxt_exports')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')

  # get list of available files
  micro_list <- list.files(micro_dir, pattern = '.txt')

  micro_list <- as.data.frame(micro_list)
  names(micro_list) <- 'filename'

  micro_list['filepath'] <- file.path(micro_dir, micro_list[['filename']])

  # get condition informtion
  micro_list['paradigm'] <- sapply(micro_list[['filename']], function(x) strsplit(x, "-")[[1]][1], USE.NAMES = FALSE)

  micro_list['ses'] <- sapply(micro_list[['filename']], function(x) strsplit(x, "-")[[1]][2], USE.NAMES = FALSE)
  micro_list['ses'] <- gsub('_commercial', '', micro_list[['ses']])

  micro_list['ses_str'] <- ifelse(micro_list[['ses']] == 'baseline', 'ses-1', 'ses-2')

  micro_list['commercial_cond'] <- sapply(micro_list[['filename']], function(x) strsplit(x, "_")[[1]][2], USE.NAMES = FALSE)
  micro_list['commercial_cond'] <- gsub('commercial-', '', micro_list[['commercial_cond']])



  # 3. Get summary information ####
  micro_data <- sapply(seq(1, nrow(micro_list)), function(x) util_microstructure(base_wd = base_wd, file_str = micro_list[x, 'filepath'], paradigm = micro_list[x, 'paradigm'], commercial_cond = micro_list[x, 'commercial_cond'], ses_str = micro_list[x, 'ses_str']))


  micro_summary <- function(micro_dat, file_str, paradigm_str, commercial_str, ses_str, intake_data){

    # get relevant intake data
    if (commercial_str == 'food') {
      intake_ses_data <- intake_data[intake_data['ad_cond_meal'] == 1, ]
    } else if (commercial_str == 'toy') {
      intake_ses_data <- intake_data[!is.na(intake_data['ad_cond_meal']) & intake_data['ad_cond_meal'] == 2, ]
      intake_ses_data <- intake_ses_data[!(intake_ses_data['participant_id'] == 'sub-040' & intake_ses_data['visit_protocol'] == 4), ]
    } else {
      if (ses_str == 'ses-1'){
        intake_ses_data <- intake_data[intake_data['ad_cond_meal'] == 0 & intake_data['session_id'] == 'ses-1', ]
      } else {
        intake_ses_data <- intake_data[intake_data['session_id'] == 'ses-2', ]
      }
    }

    #get summary data
    micro_dat$beh_wide_data$data[!grepl('_id|paradigm|bite1', names(micro_dat$beh_wide_data$data))] <- sapply(micro_dat$beh_wide_data$data[!grepl('_id|paradigm|bite1', names(micro_dat$beh_wide_data$data))], function(x) as.numeric(x))

    micro_intake <- merge(micro_dat$beh_wide_data$data, intake_ses_data, by = 'participant_id', all.x = TRUE)

    micro_dat$beh_wide_data$data['ad_cond_meal'] <- micro_intake[['ad_cond_meal']]
    micro_dat$beh_wide_data$data['ad_cond_eah'] <- micro_intake[['ad_cond_eah']]
    micro_dat$beh_wide_data$data['visit_protocol'] <- micro_intake[['visit_protocol']]

    #eating rate
    micro_dat$beh_wide_data$data['eat_rate_kcal_1'] <- micro_intake[['meal_kcal_consumed']]/micro_intake[['meal_dur_1']]
    micro_dat$beh_wide_data$data['eat_rate_kcal_2'] <- micro_intake[['meal_kcal_consumed']]/micro_intake[['meal_dur_2']]

    micro_dat$beh_wide_data$data['eat_rate_g_1'] <- micro_intake[['meal_g_consumed']]/micro_intake[['meal_dur_1']]
    micro_dat$beh_wide_data$data['eat_rate_g_2'] <- micro_intake[['meal_g_consumed']]/micro_intake[['meal_dur_2']]

    micro_dat$beh_wide_data$data['eat_rate_g_inc_water_1'] <- micro_intake[['meal_g_consumed_inc_water']]/micro_intake[['meal_dur_1']]
    micro_dat$beh_wide_data$data['eat_rate_g_inc_water_2'] <- micro_intake[['meal_g_consumed_inc_water']]/micro_intake[['meal_dur_2']]

    micro_dat$beh_wide_data$data['eat_rate_active_kcal_1'] <- micro_intake[['meal_kcal_consumed']]/micro_intake[['total_active_eating_1']]
    micro_dat$beh_wide_data$data['eat_rate_active_kcal_2'] <- micro_intake[['meal_kcal_consumed']]/micro_intake[['total_active_eating_2']]

    micro_dat$beh_wide_data$data['eat_rate_active_g_1'] <- micro_intake[['meal_g_consumed']]/micro_intake[['total_active_eating_1']]
    micro_dat$beh_wide_data$data['eat_rate_active_g_2'] <- micro_intake[['meal_g_consumed']]/micro_intake[['total_active_eating_2']]

    micro_dat$beh_wide_data$data['eat_rate_active_g_inc_water_1'] <- micro_intake[['meal_g_consumed_inc_water']]/micro_intake[['total_active_eating_1']]
    micro_dat$beh_wide_data$data['eat_rate_active_g_inc_water_2'] <- micro_intake[['meal_g_consumed_inc_water']]/micro_intake[['total_active_eating_2']]

    # bite size
    micro_dat$beh_wide_data$data['bite_size_kcal_1'] <- micro_intake[['meal_kcal_consumed']]/micro_intake[['nbites_1']]
    micro_dat$beh_wide_data$data['bite_size_kcal_2'] <- micro_intake[['meal_kcal_consumed']]/micro_intake[['nbites_2']]

    micro_dat$beh_wide_data$data['bite_size_g_1'] <- micro_intake[['meal_g_consumed']]/micro_intake[['nbites_1']]
    micro_dat$beh_wide_data$data['bite_size_g_2'] <- micro_intake[['meal_g_consumed']]/micro_intake[['nbites_2']]

    # re-order columns
    micro_dat$beh_wide_data$data <- micro_dat$beh_wide_data$data[c('participant_id', 'session_id', 'ad_cond_meal', 'ad_cond_eah', 'visit_protocol', 'paradigm', names(micro_dat$beh_wide_data$data)[grepl('_1', names(micro_dat$beh_wide_data$data))], names(micro_dat$beh_wide_data$data)[grepl('_2', names(micro_dat$beh_wide_data$data))])]

    return(micro_dat)
  }

  micro_intake_data <- sapply(seq(1, ncol(micro_data)), function(x) micro_summary(micro_data[1, x], file_str = micro_list[x, 'filepath'], paradigm_str = micro_list[x, 'paradigm'], commercial_str = micro_list[x, 'commercial_cond'], ses_str = micro_list[x, 'ses_str'], intake_data))

  # add event data
  for (r in seq(1, ncol(micro_data))){
    micro_intake_data[length(micro_intake_data) + 1] <- list(micro_data[2, r]$event_data)
    names(micro_intake_data)[length(micro_intake_data)] <- paste0(micro_list[r, 'ses_str'], '_', micro_list[r, 'paradigm'], '-', micro_list[r, 'commercial_cond'], '_events')
  }

  names(micro_intake_data) <- c(paste0(micro_list[['ses_str']], '_', micro_list[['paradigm']], '-', micro_list[['commercial_cond']], '_beh'), paste0(micro_list[['ses_str']], '_', micro_list[['paradigm']], '-', micro_list[['commercial_cond']], '_events'))


  return(micro_intake_data)
}
