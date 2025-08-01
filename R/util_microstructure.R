#' util_microstructure: Process raw microstructure coding data
#'
#' This function loads the .txt raw data files from ObserverXT. Cleaning the data involves:
#' \itemize{
#'    \item{ 1) loading data and cleaning data (fixing names, getting coder info, unique food bites/food combos)}
#'    \item{ 2) splitting coders and generating a wide summary microstructure data frame}
#'    \item{ 3) coding switches and foods tried}
#'    \item{ 4) computing all summary data}
#' }
#'
#'
#'
#' @inheritParams proc_tasks
#' @param file_str full path to exported ObserverXT coded data
#' @inheritParams util_micro_wide
#' @param commercial_cond (string) commercial condition - options: 'toys', 'food', 'boring'
#' @inheritParams util_copy_to_source
#'
#'
#' @return Will return a list including data and metadata for
#' #' \itemize{
#'  \item{'beh_wide' - microstructure summary metrics}
#'  \item{'event' - full event-based data for each participant (long format)}
#' }
#'
#' @examples
#' #if in same working directory as data:
#' microstructure_list <- util_microstructure(base_wd_path)
#'
#' \dontrun{
#'
#' }
#'
#'
#' @export
#'
util_microstructure <- function(base_wd, file_str, paradigm, commercial_cond, ses_str) {

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

  # check that base_wd exist and is a string
  file_arg <- methods::hasArg(file_str)

  if (isTRUE(file_arg)) {
    if (!is.character(file_str)) {
      stop('file_str must be entered as a string')
    }
  } else if (isFALSE(data_arg)) {
    stop('file_str must be entered as a string')
  }

  # check that paradigm exist and is a string
  paradigm_arg <- methods::hasArg(paradigm)

  if (isTRUE(paradigm_arg)) {
    if (!is.character(paradigm)) {
      stop('paradigm must be entered as a string')
    } else {
      paradigm <- tolower(paradigm)

      if (!(paradigm %in% c('meal', 'eah'))) {
        stop('paradigm must be entered as either \'meal\' or \'eah\'')
      }
    }

  } else if (isFALSE(paradigm_arg)) {
    stop('paradigm must be entered as a string')
  }

  # check that commercial exist and is a string
  commercial_arg <- methods::hasArg(commercial_cond)

  if (isTRUE(commercial_arg)) {
    if (!is.character(commercial_cond)) {
      stop('commercial_cond must be entered as a string')
    } else {
      commercial_cond <- tolower(commercial_cond)

      if (!(commercial_cond %in% c('toy', 'food', 'boring'))) {
        stop('commercial_cond must be entered as either \'toy\', \'food\', or \'boring\'')
      }
    }

  } else if (isFALSE(commercial_arg)) {
    stop('commercial_cond must be entered as a string')
  }


  # check that ses_str exist and is a string
  ses_arg <- methods::hasArg(ses_str)

  if (isTRUE(file_arg)) {
    if (!is.character(ses_str)) {
      stop('ses_str must be entered as a string')
    } else {
      ses_str <- tolower(ses_str)

      if (ses_str == 'ses-1') {
        ses_num = 1
      } else if (ses_str == 'ses-2') {
        ses_num = 2
      } else {
        stop('ses_str must be entered as either \'ses-1\' or \'ses-2\'')
      }
    }
  } else if (isFALSE(ses_arg)) {
    stop('ses_str must be entered as a string')
  }


  # 2. load and process data file ####
  micro_raw_dir <- file.path(base_wd, 'bids', 'rawdata')

  micro_data <- read.table(file_str, sep = ',', fileEncoding = 'utf-16le', header = TRUE)
  micro_data <- micro_data[!grepl('X', names(micro_data))]

  #fix naming
  names(micro_data) <- tolower(names(micro_data))

  names(micro_data)[names(micro_data) == 'date_time_absolute_dmy_hmsf'] <- 'date_time'
  names(micro_data)[names(micro_data) == 'date_dmy'] <- 'date'
  names(micro_data)[names(micro_data) == 'time_absolute_hms'] <- 'time'
  names(micro_data)[names(micro_data) == 'time_absolute_f'] <- 'time_frames'
  names(micro_data)[names(micro_data) == 'time_relative_hmsf'] <- 'time_hmsf'
  names(micro_data)[names(micro_data) == 'time_relative_hms'] <- 'time_hms'
  names(micro_data)[names(micro_data) == 'time_relative_f'] <- 'time_relative_frames'
  names(micro_data)[names(micro_data) == 'time_relative_sf'] <- 'time_relative'
  names(micro_data)[names(micro_data) == 'duration_sf'] <- 'duration'

  if (paradigm == 'meal') {
    names(micro_data)[names(micro_data) == 'modifier_1'] <- 'straw_issue'
    names(micro_data)[names(micro_data) == 'modifier_2'] <- 'gcheese'
    names(micro_data)[names(micro_data) == 'modifier_3'] <- 'fruit'
    names(micro_data)[names(micro_data) == 'modifier_4'] <- 'chips'
    names(micro_data)[names(micro_data) == 'modifier_5'] <- 'ketchup'
    names(micro_data)[names(micro_data) == 'modifier_6'] <- 'ranch'
    names(micro_data)[names(micro_data) == 'modifier_7'] <- 'chkn_tender'

    if (grepl('baseline_commercial-boring', file_str)){
      names(micro_data)[names(micro_data) == 'modifier_8'] <- 'chkn_skin'
      names(micro_data)[names(micro_data) == 'modifier_9'] <- 'chkn_meat'
      names(micro_data)[names(micro_data) == 'modifier_10'] <- 'carrot'
      names(micro_data)[names(micro_data) == 'modifier_11'] <- 'hand'
      names(micro_data)[names(micro_data) == 'modifier_12'] <- 'fork'
      names(micro_data)[names(micro_data) == 'modifier_13'] <- 'other'
      names(micro_data)[names(micro_data) == 'modifier_14'] <- 'child_activity'
    } else {
      names(micro_data)[names(micro_data) == 'modifier_8'] <- 'chkn_meat'
      names(micro_data)[names(micro_data) == 'modifier_9'] <- 'carrot'
      names(micro_data)[names(micro_data) == 'modifier_10'] <- 'hand'
      names(micro_data)[names(micro_data) == 'modifier_11'] <- 'fork'
      names(micro_data)[names(micro_data) == 'modifier_12'] <- 'other'
      names(micro_data)[names(micro_data) == 'modifier_13'] <- 'child_activity'
    }

  } else {

  }

  # replace '.' with '_'
  names(micro_data) <- gsub('\\.', '_', names(micro_data))

  # fix 1-off errors
  micro_data['observation'] <- gsub('REACH_001_RS', 'REACH_001_V1_RS', micro_data[['observation']])

  # parse observation string - need to use data.table and then convert back in case there is missing information in observation string
  obsv_data <- data.frame(data.table::rbindlist(sapply(micro_data[['observation']], function(x) data.table::transpose(data.table::setDT(strsplit(x, '_'))), USE.NAMES = FALSE, simplify = FALSE), fill = TRUE))

  names(obsv_data) <- c('study', 'id', 'visit_str', 'ph1_coder', 'ph2_coder')

  micro_data['participant_id'] <- paste0('sub-', obsv_data[['id']])
  micro_data['visit'] <- as.numeric(gsub('V', '', obsv_data[['visit_str']]))
  micro_data['ph1_coder'] <- obsv_data['ph1_coder']
  micro_data['ph2_coder'] <- obsv_data['ph2_coder']

  # get n coders
  micro_data['n_coders'] <- sapply(micro_data[['participant_id']], function(x) length(unique(micro_data[micro_data['participant_id'] == x, 'ph1_coder'])))

  coder_info <- function(id){
    data <- micro_data[micro_data['participant_id'] == id, ]

    obs_unique <- unique(data['ph1_coder'])

    if(nrow(obs_unique) == 1) {
      return(rep(1, nrow(data)))
    } else {

      coder_order <- c(rep(1, nrow(data[data['ph1_coder'] == obs_unique[1, ], ])), rep(2, nrow(data[data['ph1_coder'] == obs_unique[2, ], ])))

      return(coder_order)
    }
  }

  micro_data['coder_order'] <- unlist(sapply(unique(micro_data[['participant_id']]), function(x) coder_info(x), USE.NAMES = FALSE))

  ## concatenate foods
  if (paradigm == 'meal') {
    micro_data['chkn_tender'] <- gsub('Chicken Tenders', 'chkn_tender', micro_data[['chkn_tender']])
    micro_data['gcheese'] <- gsub('Grilled Cheese', 'gcheese', micro_data[['gcheese']])
    micro_data['chkn_meat'] <- gsub('Chicken Tenders \\(Meat Only\\)', 'chkn_meat', micro_data[['chkn_meat']])


    micro_data['fruit'] <- tolower(micro_data[['fruit']])
    micro_data['chips'] <- tolower(micro_data[['chips']])
    micro_data['ketchup'] <- tolower(micro_data[['ketchup']])
    micro_data['ranch'] <- tolower(micro_data[['ranch']])
    micro_data['carrot'] <- tolower(micro_data[['carrot']])

    if (grepl('baseline_commercial-boring', file_str)){
      micro_data['chkn_skin'] <- gsub('Chicken Tenders \\(Skin only\\)', 'chkn_skin', micro_data[['chkn_skin']])
      micro_data['food'] <- paste0(micro_data[['gcheese']], micro_data[['fruit']], micro_data[['chips']], micro_data[['ketchup']], micro_data[['ranch']], micro_data[['chkn_tender']], micro_data[['chkn_meat']], micro_data[['chkn_skin']], micro_data[['carrot']])
    } else {
      micro_data['food'] <- paste0(micro_data[['gcheese']], micro_data[['fruit']], micro_data[['chips']], micro_data[['ketchup']], micro_data[['ranch']], micro_data[['chkn_tender']], micro_data[['chkn_meat']], micro_data[['carrot']])
    }

    micro_data['food'] <- gsub('ranchchkn_tender', 'ranch_chkn_tender', micro_data[['food']])
    micro_data['food'] <- gsub('ketchupchkn_tender', 'ketchup_chkn_tender', micro_data[['food']])
    micro_data['food'] <- gsub('chipsketchup', 'chips_ketchup', micro_data[['food']])
    micro_data['food'] <- gsub('ranchcarrots', 'ranch_carrots', micro_data[['food']])
    micro_data['food'] <- gsub('gcheesefruit', 'gcheese_fruit', micro_data[['food']])
    micro_data['food'] <- gsub('chipsranch', 'chips_ranch', micro_data[['food']])
    micro_data['food'] <- gsub('gcheeseranch', 'gcheese_ranch', micro_data[['food']])
    micro_data['food'] <- gsub('gcheeseketchup', 'gcheese_ketchup', micro_data[['food']])
    micro_data['food'] <- gsub('ketchupranchchkn_tender', 'ketchup_ranch_chkn_tender', micro_data[['food']])
    micro_data['food'] <- gsub('ketchupranch_chkn_tender', 'ketchup_ranch_chkn_tender', micro_data[['food']])
    micro_data['food'] <- gsub('ketchupcarrots', 'ketchup_carrots', micro_data[['food']])
    micro_data['food'] <- gsub('chipschkn_tender', 'chips_chkn_tender', micro_data[['food']])
    micro_data['food'] <- gsub('chipsketchupranch', 'chips_ketchup_ranch', micro_data[['food']])
    micro_data['food'] <- gsub('chips_ketchupranch', 'chips_ketchup_ranch', micro_data[['food']])
    micro_data['food'] <- gsub('ketchupranch', 'ketchup_ranch', micro_data[['food']])
    micro_data['food'] <- gsub('gcheesechkn_tender', 'gcheese_chkn_tender', micro_data[['food']])
    micro_data['food'] <- gsub('gcheesechips', 'gcheese_chips', micro_data[['food']])

    hed_foods <- c('gcheese', 'chkn_tender', 'chkn_meat', 'chkn_skin', 'chips', 'ketchup', 'ranch', 'ketchup_ranch')
    led_foods <- c('fruit', 'carrots')

    # food ed
    micro_data[['food_ed']] <- ifelse(is.na(micro_data[['food']]), NA, ifelse(micro_data[['food']] == '', NA, ifelse(micro_data[['food']] %in% hed_foods | gsub('_ranch|ranch_|_ketchup|ketchup_', '', micro_data[['food']]) %in% hed_foods, 'h_ed', ifelse(micro_data[['food']] %in% led_foods | gsub('_ranch|ranch_|_ketchup|ketchup_', '', micro_data[['food']]) %in% led_foods, 'l_ed', ifelse(grepl('gcheese', micro_data[['food']]) & grepl('chkn', micro_data[['food']]), 'h_ed', ifelse(grepl('gcheese', micro_data[['food']]) & grepl('chips', micro_data[['food']]), 'h_ed', ifelse(grepl('chkn', micro_data[['food']]) & grepl('chips', micro_data[['food']]), 'h_ed', ifelse(grepl('fruit', micro_data[['food']]) & grepl('carrot', micro_data[['food']]), 'l_ed', 'mixed_ed'))))))))

  }

  # food/sip
  micro_data[['food_sip']] <- ifelse(is.na(micro_data[['food']]) | micro_data[['food']] == '', ifelse(micro_data[['behavior']] == 'Sips', 'sip', micro_data[['food']]), micro_data[['food']])

  # food/sip/beh
  micro_data[['food_sip_distract']] <- ifelse(is.na(micro_data[['food']]) | micro_data[['food']] == '', ifelse(micro_data[['behavior']] == 'Sips', 'sip', ifelse(micro_data[['behavior']] == 'Leaving Chair', 'distract_beh', micro_data[['food']])), micro_data[['food']])

  # 3. Switching Data ####

  micro_data_switch <- data.frame(sapply(c('food', 'food_ed', 'food_sip', 'food_sip_distract'), function(x) util_micro_switch(id_var = 'participant_id', micro_data, x, coder_var <- 'coder_order')))
  names(micro_data_switch) <- paste0(names(micro_data_switch), '_switch')

  micro_data <- cbind.data.frame(micro_data, micro_data_switch)

  # fix order and update names
  micro_data['session_id'] <- ifelse(micro_data[['visit']] == 5, 'ses-2', 'ses-1')

  micro_data <- micro_data[c('participant_id', 'session_id', 'visit', 'n_coders', 'coder_order', 'ph1_coder', 'ph2_coder', 'observation', names(micro_data)[grepl('date|time|affect', names(micro_data))], names(micro_data)[!grepl('id|visit|coder|observation|date|time|affect', names(micro_data))])]

  names(micro_data)[names(micro_data) == 'visit'] <- 'visit_protocol'

  ## 4. Make Summary Data ####
  micro_data_wide <- util_micro_wide(id_var = 'participant_id', micro_data, ses_num, paradigm, coder_var = 'coder_order')



  ## make list of data frame and associated labels
  meal_micro <- list(
    beh_wide_data = list(data = micro_data_wide, meta = NA),
    event_data = list(data = micro_data, meta = NA))

  ## want an export options??

  return(meal_micro)
}
