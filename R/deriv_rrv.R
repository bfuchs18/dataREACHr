#' deriv_rrv: Generate derivative databases for RRV analyses
#'
#' This function generates RRV derivative databases from participant-level RRV data
#'
#' @param data a list of dataframes, where 1 dataframe contains processed RRV data for 1 sub (output of util_task_rrv). data OR file_list is required.
#' @param file_list a list filenames, where 1 filename is full path to processed RRV data for 1 sub (exported by util_task_rrv). data OR file_list is required.
#' @return a list with: 1) a dataframe with long summary data (by reinforcer schedule) and 2) a dataframe with overall summary data
#'
#' @examples
#'
#' \dontrun{
#'
#' # process task data - this returns and exports processed RRV data
#' base_dir = "/Users/baf44/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/"
#' task_data <- proc_task(base_wd = base_dir, return_data = TRUE)
#'
#' # create deriv database from data RETURNED by proc_task
#' rrv_summary <- deriv_rrv(data = task_data$rrv)
#'
#' # create deriv database from files EXPORTED by proc_task, using a list of file names
#' file_list <- list.files(file.path(base_dir, "bids", "rawdata"), pattern = "task-rrv_beh.tsv", recursive = TRUE, full.names = TRUE)
#' rrv_summary <- deriv_rrv(file_list = file_list)
#'
#' }
#' @export

deriv_rrv <- function(data, file_list) {


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

      # save subject's dataframe to data, named with sub_str
      data[[sub_str]] <- read.table(file, sep='\t', header = TRUE, na.strings = 'n/a')
    }

  }

  #### Create summary data ####
  # create output dataframes

#  summary_colnames <- c("participant_id", "pmax_responded_food", "pmax_completed_food", "pmax_responded_toy", "pmax_completed_toy", "rrv_food_responded", "rrv_food_completed", "total_responses_food", "total_responses_toy", "mean_response_rate_food", "mean_response_rate_toy")
#  long_summary_colnames <- c("participant_id", "reinforcer", "schedule", "responses", "reinforcers", "total_time", "response_rate")

  summary <- data.frame()

  for (df_num in 1:length(data)) {

    # extract dataframe
    rrv_data <- data[[df_num]]

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

    # total reinforcers
    total_reinforcers_food = sum(rrv_data[rrv_data$reinforcer == "Candy" & rrv_data$block == 1, ]$session_reinforcers)
    total_reinforcers_toy = sum(rrv_data[rrv_data$reinforcer == "Toy" & rrv_data$block == 1, ]$session_reinforcers)

    # total time
    ### Issue: sessions where no responses were made can still have time associated with them. Result of task left running? E.g., sub-023. How to determine true end of task?
#    total_time_food = sum(rrv_data[rrv_data$reinforcer == "Candy" & rrv_data$block == 1, ]$session_time)
#    total_time_toy = sum(rrv_data[rrv_data$reinforcer == "Toy" & rrv_data$block == 1, ]$session_time)

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
        total_reinforcers_food = total_reinforcers_food,
        total_reinforcers_toy = total_reinforcers_toy,
        mean_response_rate_food = NA,
        mean_response_rate_toy = NA
      )

    # add summary_row to summary
    if (df_num == 1) {
      summary_dat <- summary_row
    } else {
      summary_dat <- dplyr::bind_rows(summary_dat, summary_row)
    }

    #### process long data ####

    # extract data for candy
    long_rows <- rrv_data[rrv_data$block == 1, c("participant_id", "reinforcer", "session", "session_time", "schedule", "session_blocks", "session_nonresp_blocks", "session_responses", "session_reinforcers", "session_average_responses", "session_average_reinforcers")]

    # add long_rows to long_summary
    if (df_num == 1) {
      long_summary_dat <- long_rows
    } else {
      long_summary_dat <- dplyr::bind_rows(long_summary_dat, long_rows)
    }

  }

  return(list(summary = summary_dat,
              summary_long = long_summary_dat))
}

