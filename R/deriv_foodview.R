#' deriv_foodview: Generate derivative databases for Foodview Task analyses
#'
#' This function generates FoodView derivative databases from participant-level FoodView data
#'
#' @param data a list of list of dataframes. The top-level list represents individual subjects, and each subject has a sublist of dataframes. Each dataframe contains events data for a specific run of the foodview task for a given sub. A suitable list is returned by proc_task, or can be gathered from files in bids/rawdata
#' @param file_list a list filenames, where 1 filename is full path to processed foodview events file for 1 run for 1 sub (exported by util_task_foodview). data OR file_list is required.
#' @return a list with: 1) summary_long_by_cond = a long dataframe with summary data by commerical_condition (metrics calculated across runs), 2) summary_long_by_block = a long dataframe with summary data by block
#'
#' @examples
#'
#' \dontrun{
#'
#' # process task data
#' base_dir = "/Users/baf44/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/"
#' task_data <- proc_task(base_wd = base_dir, return_data = TRUE)
#'
#' # create deriv database from data RETURNED by proc_task
#' food_view_summary <- deriv_foodview(data = task_data$foodview)
#'
#' # create deriv database from files EXPORTED by proc_task, using a list of file names
#' file_list <- list.files(file.path(base_dir, "bids", "rawdata"), pattern = "foodview.*events\\.tsv$", recursive = TRUE, full.names = TRUE) # include files across all subs and runs
#' food_view_summary <- deriv_foodview(file_list = file_list)
#' }
#' @export

deriv_foodview <- function(data, file_list) {


  #### Check args #####

  # user must enter data OR file_list argument

  data_arg <- methods::hasArg(data)
  file_list_arg <- methods::hasArg(file_list)

  # if neither arg entered, exit
  if ( sum(data_arg, file_list_arg) == 0 ) {
    stop("Must enter data or file_list argument")

    # if both args entered, exit
  } else if ( sum(data_arg, file_list_arg) == 2 ) {
    stop("Must enter data OR file_list argument - pick one!")

  } # add checks of datatype for data (list of dataframes) and file_list (list of strings) ??


  #### If data_list arg used, create data from data_list #####
  # data is a list of dataframes named with sub_str

  if (isTRUE(file_list_arg)) {

    data <- list()
    for (file in file_list){

      # get sub_str ('sub-XXX')
      sub_str <- substr(basename(file), 1, 7)

      # get run number
      run_str <- substr(basename(file), 29, 34)

      # save subject's dataframe to data, named with sub_str
      data[[sub_str]][[run_str]] <- read.table(file, sep='\t', header = TRUE, na.strings = 'n/a')
    }

  }

  #### Create summary database ####
  # create output dataframes
  summary_bycond_df <- data.frame()

  # create output dataframes
  summary_byblock_df <- data.frame()

  # for each sub (top-level list in data)
  for (i in 1:length(data)) {

    # extract subject's list of dataframes
    foodview_data <- data[[i]]

    # make one dataframe with all run data
    combined_df <- dplyr::bind_rows(foodview_data)

    # sub
    sub = combined_df$sub[1]

    # on trials with responses times of 0, set response as 0 and RT as NA -- response times of 0 are not considered real responses
    combined_df$response[combined_df$response_time == 0] <- 0
    combined_df$response_time[combined_df$response_time == 0] <- NA

    # on trials with responses of -99, set RT as NA -- -99 indicates responses other than 1 or 2 in first 2 subs (3, 6) due to trigger
    combined_df$response_time[combined_df$response == -99] <- NA

    # # assess response_time outliers
    # average_rt = mean(combined_df[combined_df$response == 1 | combined_df$response == 2,]$response_time, na.rm = TRUE)
    # outliers = boxplot.stats(combined_df[combined_df$response == 1 | combined_df$response == 2,]$response_time)$out
    # if (length(outliers) > 0) {
    #   if (sum(outliers < average_rt) > 0) {
    #     print(sub)
    #     print(outliers)
    #     print(max(outliers))
    #   }
    # }

    # Summarize data by advertisement condition in long format ----

    # subset image rows from combined_df
    jpeg_rows <- combined_df[grep("jpeg", combined_df$stim_file_name),]

    for (cond in c("food", "toy")) {

      # subset images in condition
      cond_jpeg_rows <- jpeg_rows[jpeg_rows$commercial_cond == cond,]

      n_image = nrow(cond_jpeg_rows)
      p_resp = nrow(cond_jpeg_rows[cond_jpeg_rows$response == 1 | cond_jpeg_rows$response == 2,]) / n_image
      p_want_of_resp = nrow(cond_jpeg_rows[cond_jpeg_rows$response == 1,]) / nrow(cond_jpeg_rows[cond_jpeg_rows$response == 1 | cond_jpeg_rows$response == 2,])
      avg_rt = mean(as.numeric(cond_jpeg_rows[cond_jpeg_rows$response == 1 | cond_jpeg_rows$response == 2,]$response_time))

      hed_rows <- cond_jpeg_rows[cond_jpeg_rows$food_ed == "high",]
      hed_n_image = nrow(hed_rows)
      hed_p_resp = nrow(hed_rows[hed_rows$response == 1 | hed_rows$response == 2,]) / hed_n_image
      hed_p_want_of_resp = nrow(hed_rows[hed_rows$response == 1,]) / nrow(hed_rows[hed_rows$response == 1 | hed_rows$response == 2,])
      hed_avg_rt = mean(as.numeric(hed_rows[hed_rows$response == 1 | hed_rows$response == 2,]$response_time))

      led_rows <- cond_jpeg_rows[cond_jpeg_rows$food_ed == "low",]
      led_n_image = nrow(led_rows)
      led_p_resp = nrow(led_rows[led_rows$response == 1 | led_rows$response == 2,]) / led_n_image
      led_p_want_of_resp = nrow(led_rows[led_rows$response == 1,]) / nrow(led_rows[led_rows$response == 1 | led_rows$response == 2,])
      led_avg_rt = mean(as.numeric(led_rows[led_rows$response == 1 | led_rows$response == 2,]$response_time))

      sweet_rows <- cond_jpeg_rows[cond_jpeg_rows$food_taste == "sweet",]
      sweet_n_image = nrow(sweet_rows)
      sweet_p_resp = nrow(sweet_rows[sweet_rows$response == 1 | sweet_rows$response == 2,]) / sweet_n_image
      sweet_p_want_of_resp = nrow(sweet_rows[sweet_rows$response == 1,]) / nrow(sweet_rows[sweet_rows$response == 1 | sweet_rows$response == 2,])
      sweet_avg_rt = mean(as.numeric(sweet_rows[sweet_rows$response == 1 | sweet_rows$response == 2,]$response_time))

      savory_rows <- cond_jpeg_rows[cond_jpeg_rows$food_taste == "savory",]
      savory_n_image = nrow(savory_rows)
      savory_p_resp = nrow(savory_rows[savory_rows$response == 1 | savory_rows$response == 2,]) / savory_n_image
      savory_p_want_of_resp = nrow(savory_rows[savory_rows$response == 1,]) / nrow(savory_rows[savory_rows$response == 1 | savory_rows$response == 2,])
      savory_avg_rt = mean(as.numeric(savory_rows[savory_rows$response == 1 | savory_rows$response == 2,]$response_time))

      # summarize data across task for condition
      cond_summary_row <-
        data.frame(
          sub = sub,
          commerical_cond = cond,

          n_image = n_image,
          p_resp = p_resp,
          p_want_of_resp = p_want_of_resp,
          avg_rt = avg_rt,

          hed_n_image = hed_n_image,
          hed_p_resp = hed_p_resp,
          hed_p_want_of_resp = hed_p_want_of_resp,
          hed_avg_rt = hed_avg_rt,

          led_n_image = led_n_image,
          led_p_resp = led_p_resp,
          led_p_want_of_resp = led_p_want_of_resp,
          led_avg_rt = led_avg_rt,

          sweet_n_image = sweet_n_image,
          sweet_p_resp = sweet_p_resp,
          sweet_p_want_of_resp = sweet_p_want_of_resp,
          sweet_avg_rt = sweet_avg_rt,

          savory_n_image = savory_n_image,
          savory_p_resp = savory_p_resp,
          savory_p_want_of_resp = savory_p_want_of_resp,
          savory_avg_rt = savory_avg_rt
        )


      # add row to dataframe
      if (nrow(summary_bycond_df) == 0) {
        summary_bycond_df <- cond_summary_row
      } else {
        summary_bycond_df <- dplyr::bind_rows(summary_bycond_df, cond_summary_row)
      }
    }

    # Summarize data by block in long format  -----

    for (run in unique(combined_df$run_num)) {

      # subset foodview data from combined_df for given run
      run_data <- combined_df[combined_df$run_num == run,]

      # subset image rows
      run_jpeg_rows <- run_data[grep("jpeg", run_data$stim_file_name),]

      rownames(run_jpeg_rows) <- NULL

      # Get unique block combos
      unique_combos <- unique(run_jpeg_rows[,c('commercial_cond','food_ed','food_taste')])

      # Add a "block" column with an integer for each unique combination
      run_jpeg_rows <- run_jpeg_rows %>%
        dplyr::mutate(block = match(
          interaction(commercial_cond, food_ed, food_taste),
          interaction(
            unique_combos$commercial_cond,
            unique_combos$food_ed,
            unique_combos$food_taste
          )
        ))

      # for each block
      for (block in unique(run_jpeg_rows$block)) {

        # subset images in block
        block_rows <- run_jpeg_rows[run_jpeg_rows$block == block,]

        n_image = nrow(block_rows)
        n_resp = sum(block_rows$response == 1 | block_rows$response == 2)
        n_want = sum(block_rows$response == 1)

        block_summary_row <-
          data.frame(
            sub = block_rows$sub[1],
            run_num = block_rows$run_num[1],
            block_num = block,
            commerical_cond = block_rows$commercial_cond[1],
            food_ed = block_rows$food_ed[1],
            food_taste = block_rows$food_taste[1],
            n_image = n_image,
            n_resp = n_resp,
            n_want = n_want,
            p_resp = n_resp / n_image,
            p_want_of_resp = n_want / n_resp,
            avg_rt = mean(block_rows$response_time, na.rm = TRUE)
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

