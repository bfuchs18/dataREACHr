#' util_sst_summary: Get summary data from individual participants for the Stop Signal Task
#'
#' This function calculates summary performance data for an individual participant
#'
#'
#' @param ind_dat Processed individual dataset from rawdata for the Stop Signal Task task
#' @param format Format in which the summary data is returned: 'wide' will return a 3-row summary data.frame() for all trials, behavioral pre-scan trials, and fMRI trials; 'byrun' will return the summary response data by run; 'byblock' will return the summary response data by run and block.
#'
#' @return a data.frame with summary data based on format requested
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' sst_summary_beh <- util_sst_summary(ind_dat)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_sst_summary <- function(ind_dat, format) {

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(ind_dat)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(ind_dat)) {
      stop('ind_dat must be entered as a data.frame')
    }
  } else if (isFALSE(data_arg)) {
    stop('ind_dat must be entered as a data.frame')
  }

  ## primary summary data funciton ####
  # define function to get summary metrics for a given subset of data. ----
  sum_data_fn <- function(sub_dat, cond, cond_var){

    if (is.na(cond)) {
      cond_data <- sub_dat[grepl('jpeg', sub_dat[['stim_file_name']]),]
    } else {
      # subset image rows for given cond
      cond_data <- sub_dat[grepl('jpeg', sub_dat[['stim_file_name']]) & sub_dat[cond_var] == cond,]
    }

    # subset trials by signal
    go_trials <- cond_data[cond_data['signal'] == 0,]
    stop_trials <- cond_data[cond_data['signal'] == 1,]

    # number of go/stop trials
    n_stop_trials = nrow(stop_trials)
    n_go_trials = nrow(go_trials)

    # number correct/incorrect
    n_go_cor = sum(go_trials[['correct']] == 4)
    n_go_error = sum(go_trials[['correct']] == 2)
    n_go_miss = sum(go_trials[['correct']] == 1)
    n_stop_fail = sum(stop_trials[['correct']] == 3)

    # response times
    go_rt_mean = mean(go_trials[['rt']], na.rm = TRUE)
    go_correct_rt_mean = mean(go_trials[go_trials['correct'] == 4, 'rt'])
    go_error_rt_mean = mean(go_trials[go_trials['correct'] == 2, 'rt'])
    us_rt_mean = mean(stop_trials[stop_trials['correct'] == 3, 'rt'])

    # proportion stops fails
    prop_stop_fail = n_stop_fail / n_stop_trials

    # mean stop signal delay
    ssd_mean = mean(stop_trials$trueSSD)

    if (n_stop_fail == 0) {

      racehorse_check = 0
      ssrt_mean = NA
      ssrt_int = NA

    } else if ( go_rt_mean > us_rt_mean ){
      racehorse_check = 1 # meet racehorse assumptions

      #calculate ssrt with mean method
      ssrt_mean = go_rt_mean - ssd_mean

      # replace omissions with max RT if there are omissions
      if (n_go_miss > 0) {

        # get max go rt
        max_go_rt = max(go_trials[['rt']], na.rm = TRUE)

        #replace omitted go rt values
        go_trials[is.na(go_trials['rt']), 'rt'] <- max_go_rt

      }

      #get rt at prop_stop_fail percentile
      nth_rt = quantile(go_trials[['rt']], probs = prop_stop_fail, names = FALSE)

      #calculate ssrt with integration method
      ssrt_int = nth_rt - ssd_mean

    } else {
      racehorse_check = 0 # fail to meet racehorse assumptions

      # don't calculate ssrt
      ssrt_mean = NA
      ssrt_int = NA
    }

    if (!is.na(cond_var) & cond_var == 'block') {
      sum_dat <- data.frame(img_cat = cond_data[1, 'img_cat'], racehorse_check = racehorse_check, n_stop_trials = n_stop_trials, n_go_trials = n_go_trials, go_rt_mean = go_rt_mean, n_go_cor = n_go_cor, go_correct_rt_mean = go_correct_rt_mean, n_go_error = n_go_error, go_error_rt_mean = go_error_rt_mean, n_go_miss = n_go_miss, prop_stop_fail = prop_stop_fail, us_rt_mean = us_rt_mean, ssd_mean = ssd_mean, ssrt_mean = ssrt_mean, ssrt_int = ssrt_int)
    } else {
      sum_dat <- data.frame(racehorse_check = racehorse_check, n_stop_trials = n_stop_trials, n_go_trials = n_go_trials, go_rt_mean = go_rt_mean, n_go_cor = n_go_cor, go_correct_rt_mean = go_correct_rt_mean, n_go_error = n_go_error, go_error_rt_mean = go_error_rt_mean, n_go_miss = n_go_miss, prop_stop_fail = prop_stop_fail, us_rt_mean = us_rt_mean, ssd_mean = ssd_mean, ssrt_mean = ssrt_mean, ssrt_int = ssrt_int)

      if (!is.na(cond)) {
        names(sum_dat) <- paste0(cond, '_', names(sum_dat))
      }
    }


    return(sum_dat)
  }

  ## wide/by condition function ####
  bycond_sumdat <- function(data, type, run){
    if (type == 'all'){
      sub_dat <- data
    } else {
      sub_dat <- ind_dat[ind_dat['type'] == type,]
    }

    if(hasArg(run)){
      sub_dat <- sub_dat[sub_dat['run_num'] == run, ]
    }

    # overall
    sum_dat <- sum_data_fn(sub_dat, cond = NA, cond_var = NA)

    # by sweet/savory
    foodcat_sum_dat <- do.call(cbind.data.frame, sapply(c('sweet', 'savory'), function(x) sum_data_fn(sub_dat, cond = x, cond_var = 'img_cat'), simplify = FALSE, USE.NAMES = FALSE))

    # combind for wide OR by run
    if (hasArg(run)) {
      sum_dat <- cbind.data.frame(sum_dat, foodcat_sum_dat)

      sum_dat['run'] <- run
      sum_dat['ad_cond'] <- sub_dat[1, 'run_cond']

      sum_dat <- sum_dat[c('run', 'ad_cond', names(sum_dat)[!grepl('run|ad_cond', names(sum_dat))])]

    } else {
      # by ad condition (determined by run)
      cond_sum_dat <- do.call(cbind.data.frame, sapply(c('food', 'toy'), function(x) sum_data_fn(sub_dat, cond = x, cond_var = 'run_cond'), simplify = FALSE, USE.NAMES = FALSE))

      # combine
      sum_dat <- cbind.data.frame(sum_dat, cond_sum_dat, foodcat_sum_dat)
    }

    return(sum_dat)
  }

  ## long by block function ####
  byblock_sumdat <- function(data, run){
    sub_dat <- data[data['run_num'] == run, ]

    # block list
    block_list <- unique(sub_dat[!is.na(sub_dat['block']), 'block'])

    if (length(block_list) > 0 ) {
      block_sum_dat <- do.call(rbind.data.frame, sapply(block_list, function(x) sum_data_fn(sub_dat, cond = x, cond_var = 'block'), simplify = FALSE, USE.NAMES = FALSE))

      block_sum_dat['run'] <- run
      block_sum_dat['ad_cond'] <- sub_dat[1, 'run_cond']
      block_sum_dat['block'] <- block_list

      block_sum_dat <- block_sum_dat[c('run', 'ad_cond', 'block', names(block_sum_dat)[!grepl('run|ad_cond|block', names(block_sum_dat))])]

      return(block_sum_dat)
    }
  }

  ## by run long data function ####
  byrun_sumdat <- function(data, type, block){

    # get data
    sub_dat <- ind_dat[ind_dat['type'] == type,]

    # runs
    run_list <- unique(sub_dat[['run_num']])

    # get by block
    if (isTRUE(block)){
      block_sum_dat <- do.call(rbind.data.frame, sapply(run_list, function(x) byblock_sumdat(sub_dat, run = x), simplify = FALSE))

      # add additional information
      block_sum_dat['trial_type'] <- tolower(type)

      block_sum_dat <- block_sum_dat[c('trial_type', names(block_sum_dat)[!grepl('trial_type', names(block_sum_dat))])]

      return(block_sum_dat)

    } else {
      # get by condition
      run_sum_dat <- do.call(rbind.data.frame, sapply(run_list, function(x) bycond_sumdat(sub_dat, type = 'all', run = x), simplify = FALSE))

      # add additional information
      run_sum_dat['trial_type'] <- tolower(type)

      run_sum_dat <- run_sum_dat[c('trial_type', names(run_sum_dat)[!grepl('trial_type', names(run_sum_dat))])]

      return(run_sum_dat)
    }

  }


  #### Create summary database ####

  if (format == 'wide') {

    trial_type_list <- unique(ind_dat[['type']])

    if (length(trial_type_list) > 1) {
      trial_type_list <- c('all', trial_type_list)
    }

    wide_sum_dat <- do.call(rbind.data.frame, sapply(trial_type_list, function(x) bycond_sumdat(ind_dat, type = x), simplify = FALSE))

    wide_sum_dat['participant_id'] <- ind_dat[1, 'participant_id']
    wide_sum_dat['session_id'] <- 'ses-1'
    wide_sum_dat['trial_type'] <- trial_type_list

    wide_sum_dat <- wide_sum_dat[c('participant_id', 'session_id', 'trial_type', names(wide_sum_dat)[!grepl('id|trial_type', names(wide_sum_dat))])]

    wide_sum_json <- json_sst_summary()

    return(wide_sum_dat)

  } else if (format == 'byrun') {
    #### process long data ####
    trial_type_list <- unique(ind_dat[['type']])

    run_sum_dat <- do.call(rbind.data.frame, sapply(trial_type_list, function(x) byrun_sumdat(ind_dat, type = x, block = FALSE), simplify = FALSE))

    run_sum_dat['participant_id'] <- ind_dat[1, 'participant_id']
    run_sum_dat['session_id'] <- 'ses-1'

    run_sum_dat <- run_sum_dat[c('participant_id', 'session_id', names(run_sum_dat)[!grepl('id', names(run_sum_dat))])]

    return(run_sum_dat)


  } else if (format == 'byblock') {
    # by block
    trial_type_list <- unique(ind_dat[['type']])

    block_sum_dat <- do.call(rbind.data.frame, sapply(trial_type_list, function(x) byrun_sumdat(ind_dat, type = x, block = TRUE), simplify = FALSE))

    block_sum_dat['participant_id'] <- ind_dat[1, 'participant_id']
    block_sum_dat['session_id'] <- 'ses-1'

    block_sum_dat <- block_sum_dat[c('participant_id', 'session_id', names(block_sum_dat)[!grepl('id', names(block_sum_dat))])]

    return(block_sum_dat)

  }
}

