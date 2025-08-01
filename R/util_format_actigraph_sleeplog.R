#' util_format_actigraph_sleeplog: formats actigraph scored sleep data for GGIR
#'
#' This function prepares actigraph scored sleep data for GGIR processing
#'
#'
#' @param sleeplog_dat scored sleeplog data from actigraph watches using ActiLife
#'
#' @examples
#'
#' # process data
#' sleeplog_dat_formatted <- util_format_actigraph_sleeplog(sleeplog_dat)
#'
#' @seealso [proc_actigraph()]
#'
#' @export
#'


util_format_actigraph_sleeplog <- function(sleeplog_dat) {

  ## fix participant IDs ####
  sleeplog_dat[sleeplog_dat[['participant_id']] == '25_REACH', 'participant_id'] <- 'REACH_025'
  sleeplog_dat[sleeplog_dat[['participant_id']] == '49', 'participant_id'] <- 'REACH_049'
  sleeplog_dat[sleeplog_dat[['participant_id']] == 'REACH_18', 'participant_id'] <- 'REACH_018'
  sleeplog_dat[sleeplog_dat[['participant_id']] == 'REACH_2', 'participant_id'] <- 'REACH_002'
  sleeplog_dat[sleeplog_dat[['participant_id']] == 'REACH_28', 'participant_id'] <- 'REACH_028'
  sleeplog_dat[sleeplog_dat[['participant_id']] == 'REACH_4', 'participant_id'] <- 'REACH_004'
  sleeplog_dat[sleeplog_dat[['participant_id']] == 'REACH_5', 'participant_id'] <- 'REACH_005'
  sleeplog_dat[sleeplog_dat[['participant_id']] == 'reach_6', 'participant_id'] <- 'REACH_006'
  sleeplog_dat[sleeplog_dat[['participant_id']] == 'REACH_0568', 'participant_id'] <- 'REACH_068'

  sleeplog_dat[['participant_id']] <- gsub('REACH_', 'sub-', sleeplog_dat[['participant_id']])

  ## remove NA columns
  cols_n_na <- sapply(names(sleeplog_dat), function(x) sum(is.na(sleeplog_dat[[x]]) | sleeplog_dat[[x]] == ''))
  sleeplog_dat <- sleeplog_dat[, (cols_n_na < 71)]

  ## get date columns for advanced organizaiton ####

  format_datetime <- function(data){
    date_data <- lubridate::mdy_hm(data)
    hours <- lubridate::hour(date_data)
    minutes <- lubridate::minute(date_data)

    time_dat <- sprintf("%02d:%02d:%02d", hours, minutes, 0)

    time_dat <- ifelse(time_dat == 'NA:NA:00', '', time_dat)

    return(time_dat)
  }

  sleeplog_times <- as.data.frame(sapply(names(sleeplog_dat)[!grepl('_id', names(sleeplog_dat))], function(x) format_datetime(sleeplog_dat[[x]])))

  format_date <- function(data){
    date_data <- lubridate::mdy_hm(data)
    date_ymd_dat <- lubridate::as_date(date_data)

    return(date_ymd_dat)
  }

  sleeplog_dates <- as.data.frame(sapply(names(sleeplog_dat)[grepl('inbed', names(sleeplog_dat))], function(x) format_date(sleeplog_dat[[x]]), simplify = FALSE))
  sleeplog_dates[['participant_id']] <- sleeplog_dat[['participant_id']]

  #fix date for participant 12, 37, 42, and 62 - went to bed after midnight so shows next day but should keep the date of current day
  sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-012', 'inbed_n5'] <- sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-012', 'inbed_n5'] - 1

  sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-037', 'inbed_n4'] <- sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-037', 'inbed_n4'] - 1

  sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-042', 'inbed_n4'] <- sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-042', 'inbed_n4'] - 1

  sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-062', 'inbed_n6'] <- sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-062', 'inbed_n6'] - 1

  sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-083', 'inbed_n6'] <- sleeplog_dates[sleeplog_dates['participant_id'] == 'sub-083', 'inbed_n6'] - 1


  # format data
  sleeplog_data_formated <- data.frame('participant_id' = sleeplog_dat[['participant_id']])

  n_count <- 0
  for (night_var in names(sleeplog_dates)[!grepl('_id', names(sleeplog_dates))]) {
    n_count <- n_count + 1

    var_date <- gsub('inbed', 'date', night_var)

    #get night information
    night_num <- as.numeric(substr(night_var, unlist(gregexpr('_', night_var))+2, nchar(night_var)))

    # get date var for night - need to subtrack one
    sleeplog_data_formated[[paste0('d', night_num, '_date')]] <- lubridate::as_date(sleeplog_dates[[night_var]])

    # wakeup
    if (night_num == 1){
      sleeplog_data_formated[[paste0('d', night_num, '_wakeup')]] <- ''
    } else {
      sleeplog_data_formated[[paste0('d', night_num, '_wakeup')]] <- sleeplog_times[[paste0('wakeup_n', night_num-1)]]
    }

    # sleep vars
    sleeplog_data_formated[[paste0('d', night_num, '_inbed')]] <- sleeplog_times[[paste0('inbed_n', night_num)]]
    sleeplog_data_formated[[paste0('d', night_num, '_onset')]] <- sleeplog_times[[paste0('onset_n', night_num)]]
  }

  sleeplog_data_formated[grepl('date', names(sleeplog_data_formated))] <- sapply(names(sleeplog_data_formated)[grepl('date', names(sleeplog_data_formated))], function(x) ifelse(is.na(sleeplog_data_formated[[x]]), '', as.character(sleeplog_data_formated[[x]])))

  # return data
  return(sleeplog_data_formated)

}
