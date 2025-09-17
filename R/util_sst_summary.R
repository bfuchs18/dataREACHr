#' util_sst_summary: Get summary data from individual participants for t\the Stop Signal Task
#'
#' This function calculates summary performance data for an individual participant
#'
#'
#' @param ind_dat Processed individual dataset from rawdata for the Stop Signal Task task
#' @param format Format in which the summary data is returned: 'wide' will return a 1-row summary data.frame(); 'long' will return the summary session response data by session and screen.
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

  if (format == 'wide') {

    #### Create summary database ####

    # define function to get summary metrics for a given subset of data. ----
    get_summary_row <- function(jpeg_events){
      # get_summary_row (internal)
      # jpeg_events = dataframe of rows from SST events file(s) where stim_file_name contains "jpeg"
      # returns: a dataframe of 1 row containing summary values

      # subset trials by signal
      go_trials <- jpeg_events[jpeg_events$signal == 0,]
      stop_trials <- jpeg_events[jpeg_events$signal == 1,]

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
      prop_stop_fail = n_stop_fail / n_stop_trials  #proportion of of failed stop trials
      ssd_mean = mean(stop_trials$trueSSD) #average stop signal delay on stop trials

      # if only 1 block in subset data, get block number
      if (length(unique(jpeg_events$block)) == 1) {
        block_num = unique(jpeg_events$block)
      } else {
        block_num = NA
      }

      ## ISSUE: if/else below will error if there are no unsuccessful stop trials, causing us_rt_mean to be NaN -- what is racehorse_check in this case and should ssrt be calculated?
      if (n_stop_fail == 0) {

        if (is.na(block_num)) {
          print(paste("n_stop_fail is 0 for", go_trials$sub[1], "run", go_trials$run_num[1], "-- cannot check racehorse assumption"))
        } else {
          print(paste("n_stop_fail is 0 for", go_trials$sub[1], "run", go_trials$run_num[1], "block", block_num, "-- cannot check racehorse assumption"))
        }

        racehorse_check = NA
        # don't calculate ssrt ??
        ssrt_mean = NA
        ssrt_int = NA

      } else if ( go_rt_mean > us_rt_mean ){
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

          #get rt at prop_stop_fail percentile
          nth_rt = quantile(go_trials_replace$response_time, probs = prop_stop_fail, names = FALSE)

        } else {

          # get go trial rt at prop_stop_fail percentile
          nth_rt = quantile(go_trials$response_time, probs = prop_stop_fail, names = FALSE)
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
          block_num = block_num,
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
          prop_stop_fail = prop_stop_fail,
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


      # convert response_time unit from seconds to milliseconds
      combined_data$response_time <- combined_data$response_time*1000

      # for each data type (e.g., "beh", "func")
      for (type in unique(combined_data$type)) {

        # subset data by type
        type_data <- combined_data[combined_data$type == type,]

        # determine number of runs
        n_runs <- length(unique(type_data$run_num))

        # get summary metrics across runs, by condition ----

        # for each condition (toy, food)
        for (cond in c("toy", "food")) {

          # subset image rows for fiven cond
          cond_jpeg_rows <- type_data[grepl("jpeg", type_data$stim_file_name) & type_data$run_cond == cond,]

          # if there are jpeg events and there are no responses
          if (nrow(cond_jpeg_rows) > 0 & sum(!is.na(cond_jpeg_rows$response_time))) { ## change this so we still get a row in DF if no responses?
            # if (nrow(type_jpeg_rows) > 0 ) { ## change this so we still get a row in DF if no responses?

            # get dataframe row of summary metrics
            cond_summary_row <- get_summary_row(cond_jpeg_rows)
            cond_summary_row$run_num <- NA # replace with NA, as this would reflect the first run_num, but there are multiple when assessing by condition

            # add row to dataframe
            if (nrow(summary_bycond_df) == 0) {
              summary_bycond_df <- cond_summary_row
            } else {
              summary_bycond_df <- dplyr::bind_rows(summary_bycond_df, cond_summary_row)
            }

          }
        }


        # get summary metrics by run ----
        # note: runs are specific to a condition (food, toy), so do not need to also subset by condition
        for (run in 1:n_runs) {

          # extract foodview data for given run
          run_data <- type_data[type_data$run_num == run,]

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

          # get summary metrics by block ----
          for (block in unique(run_jpeg_rows$block)) {

            # subset images in block
            block_rows <- run_jpeg_rows[run_jpeg_rows$block == block,]

            ## change this so we still get a row in DF if no responses?
            if (nrow(block_rows) > 0 & sum(!is.na(block_rows$response_time))) {

              # get row of summary metrics
              block_summary_row <- get_summary_row(block_rows)

              # add row to dataframe
              if (nrow(summary_byblock_df) == 0) {
                summary_byblock_df <- block_summary_row
              } else {
                summary_byblock_df <- dplyr::bind_rows(summary_byblock_df, block_summary_row)
              }
            }
          }
        }
      }
    }

  } else {
    #### process long data ####

    # extract data for candy
    sum_dat <- ind_dat[ind_dat['block'] == 1 & ind_dat['ses_n_resp'] != 0, c('participant_id', 'session_id', 'screen', 'schedule', 'ses', 'ses_time', 'ses_n_resp', 'ses_reinforcer', 'ses_avg_resp', 'ses_avg_reinforcer', 'ses_blocks', 'ses_nonresp_blocks', 'reinforcer')]
  }



  return(sum_dat)
}

