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

  # create output dataframes
  summary_bycond_df <- data.frame()

  # create output dataframes
  summary_byblock_df <- data.frame()

  # for each sub (top-level list in data)
  for (i in 1:length(data)) {

    # extract subject's list of dataframes for func runs
    sst_data <- data[[i]][['func_data']]

    # Summarize data by block in long format  -----

    # determine number of runs
    n_runs <- length(sst_data)

    for (run in 1:n_runs) {

      # extract foodview data for given run
      run_data <- sst_data[[run]]

      # subset image rows
      run_jpeg_rows <- run_data[grep("jpeg", run_data$stim_file_name),]

      rownames(run_jpeg_rows) <- NULL

      # for each block
      for (block in unique(run_jpeg_rows$block)) {

        # subset images in block
        block_rows <- run_jpeg_rows[run_jpeg_rows$block == block,]

        block_summary_row <-
          data.frame(
            sub = block_rows$sub[1],
            type = block_rows$type[1],
            run_num = block_rows$run_num[1],
            block_num = block,
            commerical_cond = block_rows$run_cond[1],
            img_cat = block_rows$img_cat[1],

            # add summary metrics
            racehorse_check = NA,
            n_stop_trials = sum(block_rows$signal == 1),
            n_go_trials = sum(block_rows$signal == 0),
            go_rt = mean(block_rows$response_time, na.rm = TRUE),
            n_go_cor = sum(block_rows$signal == 0 & block_rows$correct == 4),
            go_cor_rt = mean(block_rows[block_rows$signal == 0 & block_rows$correct == 4,]$response_time),
            n_go_error = sum(block_rows$correct == 2),
            go_error_rt = mean(block_rows[block_rows$correct == 2,]$response_time),
            n_go_miss = sum(block_rows$correct == 1),
            stop_prob_resp = NA,
            us_rt = NA,
            ssd = NA,
            ssrt_mean = NA,
            ssrt_int = NA
          )

        # add row to dataframe
        if (nrow(summary_byblock_df) == 0) {
          summary_byblock_df <- block_summary_row
        } else {
          summary_byblock_df <- dplyr::bind_rows(summary_byblock_df, block_summary_row)
        }

      }
    }
  }

  return(list(summary_long_by_cond = summary_bycond_df,
              summary_long_by_block = summary_byblock_df))
}

