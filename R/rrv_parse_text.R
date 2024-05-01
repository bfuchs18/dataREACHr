#' rrv_parse_text: Parse RRV txt files when csv not available
#'
#' This function exports game.csv and summary.csv files from the RRV .txt file. This is for use when game.csv and summary.csv were not automatically generated. CSV files will export into the directory with the .txt file
#' Function will also return game.csv and summary.csv in dataframes
#'
#' @param rrv_file string with full path to rrv file
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = FALSE
#'
#' @examples
#'
#' \dontrun{
#'
#' # process task data for the Food View Task
#' file_name = "/Users/baf44/projects/Keller_Marketing/ParticipantData/untouchedRaw/rrv_task/REACH_060/060.txt"
#' rrv_csv <- rrv_parse_text(sub = 60, rrv_file = file_name)
#'
#' }
#'
#'
#' @export

rrv_parse_text <- function(rrv_file, overwrite = FALSE, return_data = FALSE) {

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('rrv_parse_text.R has not been thoroughly tested on Windows systems. Contact Bari at baf44@psu.edu if there are errors')
  }

  #### create empty dataframes to save data to ####

  ## for game.csv

  ## created vector with column names
  game_columns= c("ID",	"screen",	"reinforcer",	"type",	"session",	"total_time",	"schedule",	"time_block",	"responses",	"reinforcers", 	"total_responses",	"total_reinforcers",	"average_responses",	"average_reinforcers")

  ## pass this vector length to ncol parameter
  game_df = data.frame(matrix(nrow = 0, ncol = length(game_columns)))

  ## assign column names to game_df
  colnames(game_df) = game_columns

  ## for summary.csv

  ## created vector with column names
  summary_columns= c("ID",	"screen",	"reinforcer",	"type",	"session",	"total_time",	"schedule",	"total_time_blocks",	"total_non_response_time_blocks",	"total_responses",	"total_reinforcers",	"average_responses",	"average_reinforcers")

  ## pass this vector length to ncol parameter
  summary_df = data.frame(matrix(nrow = 0, ncol = length(summary_columns)))

  ## assign column names to game_df
  colnames(summary_df) = summary_columns

  #### parse text file ####

  # read in lines from text file
  file_lines <- readLines(rrv_file)

  # abort function if "Chronological Report" is not contained within the text file -- suggests this is not an RRV output text file
  if (!any(grepl("Chronological Report", file_lines))) {
    print(paste(rrv_file, "does not contain information needed to parse. Aborting rrv_parse_text()"))
    return()
  }

  # get subject label used in data collection
  report_line <- file_lines[1]
  sub <- unlist(strsplit(report_line, ":"))[2]

  # Use grep to find lines containing the string "Report"
  session_start_lines <- grep("Chronological Report", file_lines)

  # count the number of sessions
  n_sessions <- length(session_start_lines)

  for (session_number in 1:n_sessions) {

    # get session start
    start_line = session_start_lines[session_number]

    # get session end
    if (session_number < n_sessions) {
      end_line = session_start_lines[session_number+1]-1
    } else {
      end_line = length(file_lines)
    }

    # extract session lines
    session_lines = file_lines[start_line:end_line]

    #### Extract session-level data ####
    Type = unlist(strsplit(session_lines[grep("Type", session_lines)], "\t"))[3]
    Screen1 = unlist(strsplit(session_lines[grep("Screen1", session_lines)], "\t"))[3]
    Goal1 = unlist(strsplit(session_lines[grep("Goal1", session_lines)], "\t"))[3]
    TimeTaken1 = unlist(strsplit(session_lines[grep("Time Taken1", session_lines)], "\t"))[3]
    Screen2 = unlist(strsplit(session_lines[grep("Screen2", session_lines)], "\t"))[3]
    Goal2 = unlist(strsplit(session_lines[grep("Goal2", session_lines)], "\t"))[3]
    TimeTaken2 = unlist(strsplit(session_lines[grep("Time Taken2", session_lines)], "\t"))[3]

    # replace null values with empty string
    TimeTaken1 = ifelse(is.null(TimeTaken1), "", TimeTaken1)
    TimeTaken2 = ifelse(is.null(TimeTaken2), "", TimeTaken2)

    #### Extract Total Responses By Screen ####

    total_response_start_line <- grep("Total Responses", session_lines)
    avg_response_start_line <- grep("Average Responses", session_lines)
    total_response_lines <- session_lines[(total_response_start_line+1):(avg_response_start_line-2)]

    # set defaults to NA
    total_resp_screen1 <- NA
    total_reinforcers_screen1 <- NA
    total_resp_screen2 <- NA
    total_reinforcers_screen2 <- NA

    for (line in 1:length(total_response_lines)) {

      line_string = total_response_lines[[line]]

      # Split the string by whitespace
      line_parts <- unlist(strsplit(line_string, "\\s+"))
      line_screen <- line_parts[3]

      if (line_screen == "1") {
        total_resp_screen1 <- line_parts[1]
        total_reinforcers_screen1 <- line_parts[2]
      }

      if (line_screen == "2") {
        total_resp_screen2 <- line_parts[1]
        total_reinforcers_screen2 <- line_parts[2]
      }
    }

    #### Extract Average Responses By Screen ####

    timeblock1_start_line <- grep("Time Block : 1$", session_lines) # the $ means end of line, so that Time Block : 10, etc. does not count
    avg_response_lines <- session_lines[(avg_response_start_line+1):(timeblock1_start_line-2)]

    # set defaults to NA
    avg_resp_screen1 <- NA
    avg_resp_screen2 <- NA
    avg_reinforcers_screen1 <- NA
    avg_reinforcers_screen2 <- NA

    for (line in 1:length(avg_response_lines)) {

      line_string = avg_response_lines[[line]]

      # Split the string by whitespace
      line_parts <- unlist(strsplit(line_string, "\\s+"))
      line_screen <- line_parts[3]

      if (line_screen == "1") {
        avg_resp_screen1 <- line_parts[1]
        avg_reinforcers_screen1 <- line_parts[2]
      }

      if (line_screen == "2") {
        avg_resp_screen2 <- line_parts[1]
        avg_reinforcers_screen2 <- line_parts[2]
      }
    }

    ### Extract info for each time block ###

    # Use grep to find lines containing the string "Time Block"
    timeblock_start_lines <- grep("Time Block", session_lines)

    # count the number of timeblocks
    n_timeblocks <- length(timeblock_start_lines)

    # initialize list to append timeblock lines to
    timeblock_list <- list()

    # set summary values to 0
    total_timeblocks_screen1 <- 0
    total_timeblocks_screen2 <- 0
    total_nonresp_timeblocks_screen1 <- 0
    total_nonresp_timeblocks_screen2 <- 0

    # extract data within timeblocks
    for (timeblock_number in 1:n_timeblocks) {

      # get line where time block data starts
      tb_data_start_line = timeblock_start_lines[timeblock_number] + 2

      # get session end
      if (timeblock_number < n_timeblocks) {
        tb_end_line = timeblock_start_lines[timeblock_number+1]-2
      } else {
        tb_end_line = length(session_lines) - 1
      }

      # extract timeblock lines
      timeblock_lines = session_lines[tb_data_start_line:tb_end_line]

      ### Extract info for each reinforcer ###

      # extract number of data lines -- this corresponds to the number of reinforcers selected
      n_reinforcers = length(timeblock_lines)

      # for each reinforcer
      for (reinforcer_number in 1:n_reinforcers) {

        reinforcer_string = timeblock_lines[[reinforcer_number]]

        # Split the string by whitespace
        parts <- strsplit(reinforcer_string, "\\s+")

        # Extract reinforcer-level data
        Responses = parts[[1]][1]
        Reinforcers = parts[[1]][2]
        Screen = parts[[1]][3]
        reinforcer_cat = ifelse(Screen == "1", Goal1, ifelse(Screen == "2", Goal2, NA))

        # Increment summary values
        if (Screen == 1 ) {
          total_timeblocks_screen1 = total_timeblocks_screen1 + 1

          if (Responses == 0) {
            total_nonresp_timeblocks_screen1 = total_nonresp_timeblocks_screen1 + 1
          }

        } else if (Screen == 2) {
          total_timeblocks_screen2 = total_timeblocks_screen2 + 1

          if (Responses == 0) {
            total_nonresp_timeblocks_screen2 = total_nonresp_timeblocks_screen2 + 1
          }
        }

        # create row for game.csv
        game_data_row <-
          data.frame(
            ID = sub, #task-level
            screen = Screen, #reinforcer-level
            reinforcer = reinforcer_cat, #reinforcer-level
            type = ifelse(Type == "Slot Machine Game with Reinforcers", "slot machine", NA), #task-level
            session = session_number, #session-level
            total_time = ifelse(Screen == "1", TimeTaken1, ifelse(Screen == "2", TimeTaken2, NA)), #session x reinforcer level
            schedule = ifelse(Screen == "1", Screen1, ifelse(Screen == "2", Screen2, NA)), # session level
            time_block = timeblock_number, #time-block level
            resposes = Responses, #reinforcer-level
            reinforcers = Reinforcers,
            total_responses = ifelse(Screen == "1", total_resp_screen1, ifelse(Screen == "2", total_resp_screen2, NA)), #session x reinforcer level
            total_reinforcers = ifelse(Screen == "1", total_reinforcers_screen1, ifelse(Screen == "2", total_reinforcers_screen2, NA)), #session x reinforcer level
            average_responses = ifelse(Screen == "1", avg_resp_screen1, ifelse(Screen == "2", avg_resp_screen2, NA)), #session x reinforcer level
            average_reinforcers = ifelse(Screen == "1", avg_reinforcers_screen1, ifelse(Screen == "2", avg_reinforcers_screen2, NA)) #session x reinforcer level
          )

        # append row to game_df dataframe
        game_df <- rbind(game_df, game_data_row)

      }

    }

    # create summary dataframe
    for (screen_number in c("1", "2")) {

      # create row for summary.csv
      summary_data_row <-
        data.frame(
          ID = sub, #task-level
          screen = screen_number, #reinforcer-level
          reinforcer = ifelse(screen_number == "1", Goal1, ifelse(screen_number == "2", Goal2, NA)), #reinforcer-level
          type = ifelse(Type == "Slot Machine Game with Reinforcers", "slot machine", NA), #task-level
          session = session_number, #session-level
          total_time = ifelse(screen_number == "1", TimeTaken1, ifelse(screen_number == "2", TimeTaken2, NA)), #session x reinforcer level
          schedule = ifelse(screen_number == "1", Screen1, ifelse(screen_number == "2", Screen2, NA)), # session level
          total_time_blocks = ifelse(screen_number == "1", total_timeblocks_screen1, ifelse(screen_number == "2", total_timeblocks_screen2, NA)),
          total_nonresp_timeblocks = ifelse(screen_number == "1", total_nonresp_timeblocks_screen1, ifelse(screen_number == "2", total_nonresp_timeblocks_screen2, NA)),
          total_responses = ifelse(screen_number == "1", total_resp_screen1, ifelse(screen_number == "2", total_resp_screen2, NA)), #session x reinforcer level
          total_reinforcers = ifelse(screen_number == "1", total_reinforcers_screen1, ifelse(screen_number == "2", total_reinforcers_screen2, NA)), #session x reinforcer level
          average_responses = ifelse(screen_number == "1", avg_resp_screen1, ifelse(screen_number == "2", avg_resp_screen2, NA)), #session x reinforcer level
          average_reinforcers = ifelse(screen_number == "1", avg_reinforcers_screen1, ifelse(screen_number == "2", avg_reinforcers_screen2, NA)) #session x reinforcer level
        )

      # append row to game_df dataframe
      summary_df <- rbind(summary_df, summary_data_row)
    }

  }


  ##### Export CSVs ####

  # get data to export into
  export_dir <- dirname(rrv_file)

  # set export file paths
  game_path <- paste0(export_dir, slash, "game_parsed.csv")
  summary_path <- paste0(export_dir, slash, "summary_parsed.csv")

  if ( isTRUE(overwrite) | !file.exists(game_path) ) {
    write.csv(
      game_df,
      game_path,
      quote = FALSE,
      row.names = FALSE
    )
  }

  if ( isTRUE(overwrite) | !file.exists(summary_path) ) {
    write.csv(
      summary_df,
      summary_path,
      quote = FALSE,
      row.names = FALSE
    )
  }
  ##### Return dataframes ####

  if (isTRUE(return_data)){
    return(list(game_df = game_df, summary_df = summary_df))
  }

}
