#' deriv_rrv: Generate derivative databases for RRV analyses
#'
#' This function generates RRV derivative databases from participant-level RRV files
#'
#' @param data a list of dataframes, where 1 dataframe contains processed RRV data for 1 sub (output of util_task_rrv). A suitable list of dataframes is returned by proc_task, or can be gathered from files in bids/rawdata
#' @param return_data If return_data is set to TRUE, will return a list including:
#'  1) a dataframe with long summary data (by reinforcer schedule)
#'  2) a dataframe with overall summary data
#'

deriv_rrv <- function(data) {


  # create output dataframes

#  summary_colnames <- c("participant_id", "pmax_responded_food", "pmax_completed_food", "pmax_responded_toy", "pmax_completed_toy", "rrv_food_responded", "rrv_food_completed", "total_responses_food", "total_responses_toy", "mean_response_rate_food", "mean_response_rate_toy")
#  long_summary_colnames <- c("participant_id", "reinforcer", "schedule", "responses", "reinforcers", "total_time", "response_rate")

  summary <- data.frame()

  for (df_num in 1:length(data)) {

    # extract dataframe
    rrv_data <- data[[df_num]]
    print(rrv_data$participant_id[1])

    # TO DO: figure how how to define these values if no responses/completed sessions -- 0 ?? NA ??
    # get maximum session with responses for each reinforcer
    max_responded_session_food = ifelse(length(rrv_data[rrv_data$reinforcer == "Candy" & rrv_data$session_responses > 0, ]$session) == 0, 0, max(rrv_data[rrv_data$reinforcer == "Candy" & rrv_data$session_responses > 0, ]$session))
    max_responded_session_toy = ifelse(length(rrv_data[rrv_data$reinforcer == "Toy" & rrv_data$session_responses > 0, ]$session) == 0, 0, max(rrv_data[rrv_data$reinforcer == "Toy" & rrv_data$session_responses > 0, ]$session))

    # get maximum sessions completed (i.e., 5 reinforcers acquired) for each reinforcer
    max_completed_session_food = ifelse(length(rrv_data[rrv_data$reinforcer == "Candy" & rrv_data$session_reinforcers == 5, ]$session) == 0, 0, max(rrv_data[rrv_data$reinforcer == "Candy" & rrv_data$session_reinforcers == 5, ]$session))
    max_completed_session_toy = ifelse(length(rrv_data[rrv_data$reinforcer == "Toy" & rrv_data$session_reinforcers == 5, ]$session) == 0, 0, max(rrv_data[rrv_data$reinforcer == "Toy" & rrv_data$session_reinforcers == 5, ]$session))


    # define session to schedule mapping
    session_mapping = list("0" = 0,
                           "1" = 4,
                           "2" = 8,
                           "3" = 16,
                           "4" = 32,
                           "5" = 64,
                           "6" = 128,
                           "7" = 256,
                           "8" = 512,
                           "9" = 1024)

    # define function to get the schedule from session
    get_schedule <- function(session_number) {
      return(session_mapping[[as.character(session_number)]])
    }

    # get pmax (schedules)
    pmax_responded_food = get_schedule(max_responded_session_food)
    pmax_completed_food = get_schedule(max_completed_session_food)
    pmax_responded_toy = get_schedule(max_responded_session_toy)
    pmax_completed_toy = get_schedule(max_completed_session_toy)

    # calculate rrv
    rrv_food_responded = pmax_responded_food / (pmax_responded_food + pmax_responded_toy)
    rrv_food_completed = pmax_completed_food / (pmax_completed_food + pmax_completed_toy)

    # total responses
    total_responses_food = sum(rrv_data[rrv_data$reinforcer == "Candy" & rrv_data$block == 1, ]$session_responses)
    total_responses_toy = sum(rrv_data[rrv_data$reinforcer == "Toy" & rrv_data$block == 1, ]$session_responses)

    # create row for summary
    summary_row <-
      data.frame(
        participant_id = rrv_data$participant_id[1],
        pmax_responded_food = pmax_responded_food,
        pmax_completed_food = pmax_completed_food,
        pmax_responded_toy = pmax_responded_toy,
        pmax_completed_toy = pmax_completed_toy,
        rrv_food_responded = rrv_food_responded,
        rrv_food_completed = rrv_food_completed,
        total_responses_food = total_responses_food,
        total_responses_toy = total_responses_toy,
        mean_response_rate_food = NA,
        mean_response_rate_toy = NA
      )

    # add row to summary
    if (df_num == 1) {
      summary_dat <- summary_row
    } else {
      summary_dat <- dplyr::bind_rows(summary_dat, summary_row)
    }

    # process long data

    # for reinforcer

      # for session

  }

  return(summary_dat)
}

