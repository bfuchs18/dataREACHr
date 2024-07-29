#' deriv_sst: Generate derivative behavioral databases for SST analyses
#'
#' This function generates SST derivative databases from participant-level FoodView files
#'
#' @param data a list of list of dataframes. The top-level list represents individual subjects, and each subject has a sublist of dataframes. Each dataframe contains events data for a specific run of the foodview task for a given sub. A suitable list is returned by proc_task, or can be gathered from files in bids/rawdata
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) summary_long_by_cond = a long dataframe with summary data by commerical_condition (metrics calculated across runs)
#'  2) summary_long_by_block = a long dataframe with summary data by block
#'
#' @examples
#'
#' \dontrun{
#'
#' # process task data
#' base_dir = "/Users/bari/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/"
#' task_data <- proc_task(base_wd = base_dir, return_data = TRUE)
#'
#' # get deriv foodview data from processed task data
#' food_view_summary <- deriv_foodview(task_data$sst)
#' }

deriv_sst <- function(data) {

  # define function to get summary metrics for a given subset of data. Returns a dataframe with with summary values
  get_summary_row <- function(jpeg_events){

    # subset trials by signal
    go_trials <- run_jpeg_rows[run_jpeg_rows$signal == 0,]
    stop_trials <- run_jpeg_rows[run_jpeg_rows$signal == 1,]

    # calculate metrics
    n_stop_trials = nrow(stop_trials) # number of stop trials
    n_go_trials = nrow(go_trials) # number of go trials
    n_go_cor = sum(go_trials$correct == 4) # number of correct go trials
    n_go_error = sum(go_trials$correct == 2) # number of incorrect go trials (wrong left/right response)
    n_go_miss = sum(go_trials$correct == 1) # number of go response omissions
    n_stop_fail = sum(stop_trials$correct == 3) # number of unsuccessful stops


    go_rt_mean = mean(go_trials$response_time, na.rm = TRUE) # average reaction time on go trials, rm trials with no response
    go_correct_rt_mean = mean(go_trials[go_trials$correct == 4,]$response_time) # average reaction time on correct go trials
    go_error_rt_mean = mean(go_trials[go_trials$correct == 2,]$response_time) # average reaction time on incorrect correct go trials
    us_rt_mean = mean(stop_trials[stop_trials$correct == 3,]$response_time)  # average reaction time on unsuccessful stop trials
    prop_stop_resp = n_stop_fail / n_stop_trials  #proportion of of failed stop trials
    ssd_mean = mean(stop_trials$trueSSD) #average stop signal delay on stop trials

    ## ISSUE: if/else below will error if there are no unsuccessful stop trials, causing us_rt_mean to be NaN -- what is racehorse_check in this case?

    if (n_stop_fail == 0) {
      print(paste("n_stop_fail is 0 for", go_trials$sub[1]))
    }

    # determine racehorse_check and ssrt
    if ( go_rt_mean > us_rt_mean ){
      racehorse_check = 1 # meet racehorse assumptions

      #calculate ssrt with mean method
      ssrt_mean = go_rt_mean - ssd_mean

      # replace omissions with max RT if there are omissions
      if (n_go_miss > 0) {

        # get max go rt
        max_go_rt = max(go_trials$response_time, na.rm = TRUE)

        # make copy of go ataset
        go_trials_replace <- go_trials

        #replace omitted go rt values
        go_trials_replace$response_time[is.na(go_trials_replace$response_time)] <- max_go_rt

        #get rt at stop_prob_resp percentile
        nth_rt = quantile(go_trials_replace$response_time, probs = stop_prob_resp, names = FALSE)

      } else {

        # get go trial rt at stop_prob_resp percentile
        nth_rt = quantile(go_trials$response_time, probs = stop_prob_resp, names = FALSE)
      }

      #calculate ssrt with integration method
      ssrt_int = nth_rt - ssd_mean

    } else {
      racehorse_check = 0 # fail to meet racehorse assumptions

      # don't calculate ssrt
      ssrt_mean = NA
      ssrt_int = NA
    }

    summary_row <-
      data.frame(
        sub = go_trials$sub[1],
        type = go_trials$type[1],
        run_num = go_trials$run_num[1],
        commerical_cond = go_trials$run_cond[1],
        img_cat = go_trials$img_cat[1],

        # add summary metrics
        racehorse_check = racehorse_check,
        n_stop_trials = n_stop_trials,
        n_go_trials = n_go_trials,
        go_rt_mean = go_rt_mean,
        n_go_cor = n_go_cor,
        go_correct_rt_mean = go_correct_rt_mean,
        n_go_error = n_go_error,
        go_error_rt_mean = go_error_rt_mean,
        n_go_miss = n_go_miss,
        prop_stop_resp = prop_stop_resp,
        us_rt_mean = us_rt_mean,
        ssd_mean = ssd_mean,
        ssrt_mean = ssrt_mean,
        ssrt_int = ssrt_int
      )

    return(summary_row)
  }

  # create output dataframes
  summary_bycond_df <- data.frame()

  # create output dataframes
  summary_byrun_df <- data.frame()

  # create output dataframes
  summary_byblock_df <- data.frame()

  # for each sub (top-level list in data)
  for (i in 1:length(data)) {

    # extract subject's list of dataframes for func runs
    sst_data <- data[[i]][['func_data']]

    # sst_data should be a list of dataframes with 1 dataframe per run
    if (is.list(sst_data)) {

      # determine number of runs
      n_runs <- length(sst_data)

      for (run in 1:n_runs) {

        # extract foodview data for given run
        run_data <- sst_data[[run]]

        # convert response_time unit from seconds to milliseconds
        run_data$response_time <- run_data$response_time*1000

        # subset image rows
        run_jpeg_rows <- run_data[grep("jpeg", run_data$stim_file_name),]

        # if there are jpeg events and there are no responses
        if (nrow(run_jpeg_rows) > 0 & sum(!is.na(run_jpeg_rows$response_time))) { ## change this so we still get a row in DF if no responses?
        # if (nrow(run_jpeg_rows) > 0 ) { ## change this so we still get a row in DF if no responses?

          # get dataframe row of summary metrics
          run_summary_row <- get_summary_row(run_jpeg_rows)

          # add row to dataframe
          if (nrow(summary_byrun_df) == 0) {
            summary_byrun_df <- run_summary_row
          } else {
            summary_byrun_df <- dplyr::bind_rows(summary_byrun_df, run_summary_row)
          }

        }

        # for (block in unique(run_jpeg_rows$block)) {
        #
        #   # subset images in block
        #   block_rows <- run_jpeg_rows[run_jpeg_rows$block == block,]

        #   # get row of summary metrics
        #   block_summary_row <- get_summary_row(block_rows)
        #
        #   # add row to dataframe
        #   if (nrow(summary_byblock_df) == 0) {
        #     summary_byblock_df <- block_summary_row
        #   } else {
        #     summary_byblock_df <- dplyr::bind_rows(summary_byblock_df, block_summary_row)
        #   }
        #
        # }
      }
    }

  }

  return(list(summary_long_by_cond = summary_bycond_df,
              summary_long_by_block = summary_byblock_df))
}

