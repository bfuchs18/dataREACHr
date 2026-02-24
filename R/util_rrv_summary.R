#' util_rrv_summary: Get summary data from individual participants for tthe Reinforcing Value of Food task
#'
#' This function calculates summary performance data for an individual participant
#'
#'
#' @param ind_dat Processed individual dataset from rawdata for the the Reinforcing Value of Food task task
#' @param format Format in which the summary data is returned: 'wide' will return a 1-row summary data.frame(); 'long' will return the summary session response data by session and screen.
#'
#' @return a data.frame with summary data based on format requested
#'
#' @examples
#'
#' # process task data for the Food Choice Task
#' rrv_summary_beh <- util_rrv_summary(ind_dat)
#'
#' \dontrun{
#' }
#'
#'
#' @export

util_rrv_summary <- function(ind_dat, format) {

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

  #### Create summary data ####

  # TO DO: figure how how to define these values if no responses/completed sessions -- 0 ?? NA ??
  # get maximum session with responses for each reinforcer
  max_resp_ses_food <- ifelse(length(ind_dat[ind_dat['reinforcer'] == 'Candy' & ind_dat['ses_n_resp'] > 0, 'ses']) == 0, 0, max(ind_dat[ind_dat['reinforcer'] == 'Candy' & ind_dat['ses_n_resp'] > 0, 'ses']))
  max_resp_ses_toy <- ifelse(length(ind_dat[ind_dat['reinforcer'] == 'Toy' & ind_dat['ses_n_resp'] > 0, 'ses']) == 0, 0, max(ind_dat[ind_dat['reinforcer'] == 'Toy' & ind_dat['ses_n_resp'] > 0, 'ses']))

  # get maximum sessions completed (i.e., 5 reinforcers acquired) for each reinforcer
  max_comp_ses_food <- ifelse(length(ind_dat[ind_dat['reinforcer'] == 'Candy' & ind_dat['ses_reinforcer'] == 5, 'ses']) == 0, 0, max(ind_dat[ind_dat['reinforcer'] == 'Candy' & ind_dat['ses_reinforcer'] == 5, 'ses']))
  max_comp_ses_toy <- ifelse(length(ind_dat[ind_dat['reinforcer'] == 'Toy' & ind_dat['ses_reinforcer'] == 5, 'ses']) == 0, 0, max(ind_dat[ind_dat['reinforcer'] == 'Toy' & ind_dat['ses_reinforcer'] == 5,'ses']))


  # define session to schedule mapping
  session_mapping <- list('0' = 0,
                          '1' = 4,
                          '2' = 8,
                          '3' = 16,
                          '4' = 32,
                          '5' = 64,
                          '6' = 128,
                          '7' = 256,
                          '8' = 512,
                          '9' = 1024)

  # define function to get the schedule from session
  get_schedule <- function(session_number) {
    return(session_mapping[[as.character(session_number)]])
  }

  # get pmax (schedules)
  pmax_resp_food <- get_schedule(max_resp_ses_food)
  pmax_complete_food <- get_schedule(max_comp_ses_food)
  pmax_resp_toy <- get_schedule(max_resp_ses_toy)
  pmax_complete_toy <- get_schedule(max_comp_ses_toy)

  # calculate rrv
  rrv_food_resp <- pmax_resp_food / (pmax_resp_food + pmax_resp_toy)
  rrv_food_complete <- pmax_complete_food / (pmax_complete_food + pmax_complete_toy)

  # total responses
  total_resp_food <- sum(ind_dat[ind_dat['reinforcer'] == 'Candy' & ind_dat['block'] == 1, 'ses_n_resp'], na.rm = TRUE)
  total_resp_toy <- sum(ind_dat[ind_dat['reinforcer'] == 'Toy' & ind_dat['block'] == 1, 'ses_n_resp'], na.rm = TRUE)

  # total reinforcers
  total_reinforcers_food <- sum(ind_dat[ind_dat['reinforcer'] == 'Candy' & ind_dat['block'] == 1, 'ses_n_resp'], na.rm = TRUE)
  total_reinforcers_toy <- sum(ind_dat[ind_dat['reinforcer'] == 'Toy' & ind_dat['block'] == 1, 'ses_n_resp'], na.rm = TRUE)

  # total time
  ### Issue: sessions where no responses were made can still have time associated with them. Result of task left running? E.g., sub-023. How to determine true end of task?
  total_time_food = sum(ind_dat[ind_dat['reinforcer'] == 'Candy' & ind_dat['block'] == 1 & !is.na(ind_dat['ses_n_resp']) & ind_dat['ses_n_resp'] != 0, 'ses_time']) / 60
  total_time_toy = sum(ind_dat[ind_dat['reinforcer'] == 'Toy' & ind_dat['block'] == 1 & !is.na(ind_dat['ses_n_resp']) & ind_dat['ses_n_resp'] != 0, 'ses_time']) / 60

  # response rate
  mean_resp_rate_food <- total_resp_food / total_time_food
  mean_resp_rate_toy <- total_resp_toy / total_time_toy

  # create row for summary
  sum_dat <-
    data.frame(
      participant_id = ind_dat[1, 'participant_id'],
      session_id = ind_dat[1, 'session_id'],
      pmax_resp_food = pmax_resp_food,
      pmax_complete_food = pmax_complete_food,
      pmax_resp_toy = pmax_resp_toy,
      pmax_complete_toy = pmax_complete_toy,
      rrv_food_resp = rrv_food_resp,
      rrv_food_complete = rrv_food_complete,
      total_resp_food = total_resp_food,
      total_resp_toy = total_resp_toy,
      total_reinforcers_food = total_reinforcers_food,
      total_reinforcers_toy = total_reinforcers_toy,
      total_time_food = total_time_food,
      total_time_toy = total_time_toy,
      mean_resp_rate_food = mean_resp_rate_food,
      mean_resp_rate_toy = mean_resp_rate_toy
    )

  } else {
    #### process long data ####

    # extract data for candy
    sum_dat <- ind_dat[ind_dat['block'] == 1 & ind_dat['ses_n_resp'] != 0, c('participant_id', 'session_id', 'screen', 'schedule', 'ses', 'ses_time', 'ses_n_resp', 'ses_reinforcer', 'ses_avg_resp', 'ses_avg_reinforcer', 'ses_blocks', 'ses_nonresp_blocks', 'reinforcer')]
  }



  return(sum_dat)
}

