#' rrv_parse_text: Generate tabular data from rrv text files
#'
#' This function generates and returns 1 dataframe with data from input RRV file
#'
#'
#' @param rrv_file (character string) full path to rrv file
#' @param participant_id (character string) bids-compliant subject ID
#'
#' @examples
#'
#' \dontrun{
#'
#' # process task data for the Food View Task
#' file_name = "/Users/baf44/projects/Keller_Marketing/ParticipantData/untouchedRaw/rrv_task/REACH_060/rrv_060.txt"
#' rrv_csv <- rrv_parse_text(rrv_file = file_name, participant_id = "sub-060")
#'
#' }
#'
#'
#' @export

rrv_parse_text <- function(rrv_file, participant_id) {

  #### Argument check ####

  participant_arg <- methods::hasArg(participant_id)
  if (isTRUE(participant_arg)) {
    if (!is.character(participant_id)) {
      stop("participant_arg must be entered as a string")
    }
  } else if (isFALSE(participant_arg)) {
    stop("participant_arg must be entered as a string")
  }

  file_arg <- methods::hasArg(rrv_file)
  if (isTRUE(file_arg)) {
    if (!is.character(rrv_file)) {
      stop("rrv_file must be entered as a string")
    }
  } else if (isFALSE(file_arg)) {
    stop("participant_arg must be entered as a string")
  }

  #### IO setup ####
  if (.Platform$OS.type == "unix") {
    slash <- '/'
  } else {
    slash <- "\\"
    print('rrv_parse_text.R has not been thoroughly tested on Windows systems. Contact Bari at baf44@psu.edu if there are errors')
  }

  #### create empty dataframe to save data to ####

  ## created vector with column names
  rrv_data_columns= c("participant_id", "ID",	"screen",	"reinforcer",	"type",	"session",	"session_time",	"schedule",	"block",	"block_responses",	"block_reinforcers", "session_blocks", "session_nonresp_blocks",  "session_responses",	"session_reinforcers",	"session_average_responses",	"session_average_reinforcers")

  ## pass this vector length to ncol parameter
  rrv_data = data.frame(matrix(nrow = 0, ncol = length(rrv_data_columns)))

  ## assign column names to rrv_data
  colnames(rrv_data) = rrv_data_columns

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

    # format TimeTaken: replace null values with empty string, remove "seconds" or "second"
    TimeTaken1 = ifelse(grepl("second", TimeTaken1), as.numeric(gsub(" seconds| second", "", TimeTaken1)), ifelse(is.null(TimeTaken1), NA, NA))
    TimeTaken2 = ifelse(grepl("second", TimeTaken2), as.numeric(gsub(" seconds| second", "", TimeTaken2)), ifelse(is.null(TimeTaken2), NA, NA))

    #### Extract Total Responses By Screen ####

    total_response_start_line <- grep("Total Responses", session_lines)
    avg_response_start_line <- grep("Average Responses", session_lines)
    total_response_lines <- session_lines[(total_response_start_line+1):(avg_response_start_line-2)]

    # set defaults to NA
    total_resp_screen1 <- NA
    session_reinforcers_screen1 <- NA
    total_resp_screen2 <- NA
    session_reinforcers_screen2 <- NA

    for (line in 1:length(total_response_lines)) {

      line_string = total_response_lines[[line]]

      # Split the string by whitespace
      line_parts <- unlist(strsplit(line_string, "\\s+"))
      line_screen <- line_parts[3]

      if (line_screen == "1") {
        total_resp_screen1 <- line_parts[1]
        session_reinforcers_screen1 <- line_parts[2]
      }

      if (line_screen == "2") {
        total_resp_screen2 <- line_parts[1]
        session_reinforcers_screen2 <- line_parts[2]
      }
    }

    #### Extract Average Responses By Screen ####

    block1_start_line <- grep("Time Block : 1$", session_lines) # the $ means end of line, so that Time Block : 10, etc. does not count
    avg_response_lines <- session_lines[(avg_response_start_line+1):(block1_start_line-2)]

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
    block_start_lines <- grep("Time Block", session_lines)

    # count the number of blocks
    n_blocks <- length(block_start_lines)

    # initialize list to append block lines to
    block_list <- list()

    # extract data within blocks
    for (block_number in 1:n_blocks) {

      # get line where time block data starts
      tb_data_start_line = block_start_lines[block_number] + 2

      # get session end
      if (block_number < n_blocks) {
        tb_end_line = block_start_lines[block_number+1]-2
      } else {
        tb_end_line = length(session_lines) - 1
      }

      # extract block lines
      block_lines = session_lines[tb_data_start_line:tb_end_line]

      ### Extract info for each reinforcer ###

      # extract number of data lines -- this corresponds to the number of reinforcers selected
      n_reinforcers = length(block_lines)

      # for each reinforcer
      for (reinforcer_number in 1:n_reinforcers) {

        reinforcer_string = block_lines[[reinforcer_number]]

        # Split the string by whitespace
        parts <- strsplit(reinforcer_string, "\\s+")

        # Extract reinforcer-level data
        Responses = parts[[1]][1]
        Reinforcers = parts[[1]][2]
        Screen = parts[[1]][3]
        reinforcer_cat = ifelse(Screen == "1", Goal1, ifelse(Screen == "2", Goal2, NA))

        # create row for game.csv
        rrv_data_row <-
          data.frame(
            participant_id = participant_id, #user input to function
            ID = sub, #task-level
            screen = as.integer(Screen), #reinforcer-level
            reinforcer = reinforcer_cat, #reinforcer-level
            type = ifelse(Type == "Slot Machine Game with Reinforcers", "slot machine", NA), #task-level
            session = session_number, #session-level
            session_time = ifelse(Screen == "1", TimeTaken1, ifelse(Screen == "2", TimeTaken2, NA)), #session x reinforcer level
            schedule = ifelse(Screen == "1", Screen1, ifelse(Screen == "2", Screen2, NA)), # session level
            block = block_number, #time-block level
            block_responses = as.integer(Responses), #reinforcer-level
            block_reinforcers = as.integer(Reinforcers),
            session_blocks = NA,
            session_nonresp_blocks = NA,
            session_responses = ifelse(Screen == "1", as.integer(total_resp_screen1), ifelse(Screen == "2", as.integer(total_resp_screen2), NA)), #session x reinforcer level
            session_reinforcers = ifelse(Screen == "1", as.integer(session_reinforcers_screen1), ifelse(Screen == "2", as.integer(session_reinforcers_screen2), NA)), #session x reinforcer level
            session_average_responses = ifelse(Screen == "1", as.numeric(avg_resp_screen1), ifelse(Screen == "2", as.numeric(avg_resp_screen2), NA)), #session x reinforcer level
            session_average_reinforcers = ifelse(Screen == "1", as.numeric(avg_reinforcers_screen1), ifelse(Screen == "2", as.numeric(avg_reinforcers_screen2), NA)) #session x reinforcer level
          )

        # append row to rrv_data dataframe
        rrv_data <- rbind(rrv_data, rrv_data_row)

      }

    }

    # determine session_blocks and session_nonresp_blocks (summary values)
    for (session in unique(rrv_data$session)){

      for (screen_number in c("1", "2")) {

        # subset rows for session and screen
        subset <- rrv_data[rrv_data$session == session & rrv_data$screen == screen_number, ]

        # determine number of blocks within session for given screen (number of rows)
        session_blocks = nrow(subset)

        # determine number of non-response blocks within session for given screen
        session_nonresp_blocks = sum(subset$block_responses == 0)

        #add values to dataframe
        rrv_data$session_blocks[rrv_data$session == session & rrv_data$screen == screen_number ] <- session_blocks
        rrv_data$session_nonresp_blocks[rrv_data$session == session & rrv_data$screen == screen_number ] <- session_nonresp_blocks
      }

    }
  }


  ##### Return dataframes ####

  return(rrv_data)

}
