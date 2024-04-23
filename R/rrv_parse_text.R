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
  columns= c("ID",	"screen",	"reinforcer",	"type",	"session",	"total time",	"schedule",	"time block",	"responses",	"reinforcers", 	"total", "responses",	"total reinforcers",	"average responses",	"average reinforcers")

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

  for (session in 1:n_sessions) {

    # get session start
    start_line = session_start_lines[session]

    # get session end
    if (session < n_sessions) {
      end_line = session_start_lines[session+1]-1
    } else {
      end_line = length(file_lines)
    }

    # extract session lines
    session_lines = file_lines[start_line:end_line]

    # append to session data
    section_list <- append(section_list, list(session_lines))
  }

  # # example: access contents for session 1:
  # section_list[[1]]

  # could combine this with the for loop above
  for (session_number in 1:n_sessions){

    # Save contents for the session
    session_lines = section_list[[session_number]]

    # Extract session info

    # Extract info for each time block

    ## Use grep to find lines containing the string "Time Block"
    timeblock_start_lines <- grep("Time Block", session_lines)

    # count the number of timeblocks
    n_timeblocks <- length(timeblock_start_lines)

    # initialize list to append timeblock lines to
    timeblock_list <- list()

    # extract data within timeblocks
    for (timeblock in 1:n_timeblocks) {

      # get timeblocks start
      tb_start_line = timeblock_start_lines[timeblock]

      # get session end
      if (timeblock < n_timeblocks) {
        tb_end_line = timeblock_start_lines[timeblock+1]-2
      } else {
        tb_end_line = length(session_lines) - 1
      }

      # extract timeblock lines
      timeblock_lines = session_lines[tb_start_line:tb_end_line]

      # append to timeblock data
      timeblock_list <- append(timeblock_list, list(timeblock_lines))
    }
  }

}
