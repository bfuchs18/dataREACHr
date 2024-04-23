#' rrv_parse_text: Parse RRV txt files when csv not available
#'
#' This function formats and organizes Food View task data from bids/sourcedata into bids/rawdata for a given subject
#'
#'
#' @param sub subject label used in sub-label. Leading zeros not required
#' @param rrv_file string with full path to rrv file
#' @param overwrite logical indicating if data should be overwritten in /rawdata. Default = FALSE
#' @param return_data logical indicating if data should be returned. Default = FALSE
#' @return If return_data is set to TRUE, will return XXX
#'
#' @examples
#'
#' \dontrun{
#'
#' # process task data for the Food View Task
#' file_name = "~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/b-childfoodlab_Shared/Active_Studies/MarketingResilienceRO1_8242020/ParticipantData/untouchedRaw/rrv_task/060.txt"
#' rrv_csv <- rrv_parse_text(sub = 60, rrv_file = file_name)
#'
#' }
#'
#'
#' @export

rrv_parse_text <- function(sub, rrv_file, overwrite = FALSE, return_data = TRUE) {

  #### create empty dataframe to save data to ####

  ## created vector with column names
  columns= c("ID",	"screen",	"reinforcer",	"type",	"session",	"total_time",	"schedule",	"time_block",	"responses",	"reinforcers", 	"total_responses",	"total_reinforcers",	"average_responses",	"average_reinforcers")

  ## pass this vector length to ncol parameter
  game_df = data.frame(matrix(nrow = 0, ncol = length(columns)))

  ## assign column names to game_df
  colnames(game_df) = columns

  #### parse text file ####

  # read in lines from text file
  file_lines <- readLines(file_name)

  # Use grep to find lines containing the string "Report"
  session_start_lines <- grep("Chronological Report", file_lines)

  # count the number of sessions
  n_sessions <- length(session_start_lines)

  # initialize list to append section lines to
  section_list <- list()

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
    print("session")
    print(session_number)
    print(avg_response_start_line)
    print(timeblock1_start_line)
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

        # create row to append to dataframe
        data_row <-
          data.frame(
            ID = "sub", #task-level
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
        game_df <- rbind(game_df, data_row)

      }

    }
  }

}
