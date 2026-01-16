#' util_rrv_parse_text: Generate tabular data from rrv text files
#'
#' This function generates and returns 1 dataframe with data from input RRV file
#'
#'
#' @param rrv_file (character string) full path to rrv file
#' @inheritParams util_copy_to_source
#'
#' @examples
#'
#' \dontrun{
#'
#' # process task data for the RRV Task
#' rrv_csv <- util_rrv_parse_text(rrv_file = file_name, sub_str = 'sub-060')
#'
#' }
#'
#'
#' @export

util_rrv_parse_text <- function(rrv_file, sub_str) {

  #### Argument check ####

  participant_arg <- methods::hasArg(sub_str)
  if (isTRUE(participant_arg)) {
    if (!is.character(sub_str)) {
      stop('participant_arg must be entered as a string')
    }
  } else if (isFALSE(participant_arg)) {
    stop('participant_arg must be entered as a string')
  }

  file_arg <- methods::hasArg(rrv_file)
  if (isTRUE(file_arg)) {
    if (!is.character(rrv_file)) {
      stop('rrv_file must be entered as a string')
    }
  } else if (isFALSE(file_arg)) {
    stop('participant_arg must be entered as a string')
  }

  #### parsing from lines ####

  proc_rrv_lines <- function(line, resp_lines) {

    line_string = resp_lines[[line]]

    # Split the string by whitespace
    line_parts <- unlist(strsplit(line_string, '\\s+'))
    line_screen <- as.numeric(line_parts[3])

    n_resp <- as.numeric(line_parts[1])
    reinforcer <- as.numeric(line_parts[2])

    return(data.frame(screen = line_screen, n_resp = n_resp, reinforcer = reinforcer))

  }

  #### parsing reinforcer responses from block ####

  proc_rrv_reinforcers <- function(reinforcer_num, n_reinforcers, block_num, block_lines) {

    reinforcer_string = block_lines[[reinforcer_num]]

    # Split the string by whitespace
    parts <- strsplit(reinforcer_string, '\\s+')

    # Extract reinforcer-level data
    resp = as.numeric(parts[[1]][1])
    reinforcer = as.numeric(parts[[1]][2])
    screen = as.numeric(parts[[1]][3])

    return(data.frame(block = block_num, screen = screen, block_resp = resp, reinforcer = reinforcer))
  }


  ## processing block data ####
  proc_rrv_blocks <- function(block_num, n_blocks, block_start_lines, ses_lines) {

    # get line where time block data starts
    tb_data_start_line = block_start_lines[block_num] + 2

    # get block end
    if (block_num < n_blocks) {
      tb_end_line = block_start_lines[block_num+1]-2
    } else {
      tb_end_line = length(ses_lines) - 1
    }

    # extract block lines
    block_lines = ses_lines[tb_data_start_line:tb_end_line]

    ### Extract info for each reinforcer ###

    # extract number of data lines -- this corresponds to the number of reinforcers selected
    n_reinforcers = length(block_lines)

    # for each reinforcer
    reinforcer_data <- t(sapply(1:n_reinforcers, function(x) proc_rrv_reinforcers(reinforcer_num = x, n_reinforcers = n_reinforcers, block_num = block_num, block_lines = block_lines)))

    return(reinforcer_data)

  }

  proc_noresp_block <- function(screen_num, rrv_data) {
    # subset rows for ses and screen
    subset <- rrv_data[rrv_data$screen == screen_num, ]

    # determine number of blocks within ses for given screen (number of rows)
    ses_blocks = nrow(subset)

    # determine number of non-response blocks within ses for given screen
    ses_nonresp_blocks = sum(subset$resp == 0)

    return(data.frame(screen = screen_num, ses_blocks = ses_blocks, ses_nonresp_blocks = ses_nonresp_blocks))
  }

  #### process session data ####
  proc_rrv_ses <- function(ses_num, n_ses, file_lines) {

    # get ses start
    start_line = ses_start_lines[ses_num]

    # get ses end
    if (ses_num < n_ses) {
      end_line = ses_start_lines[ses_num+1]-1
    } else {
      end_line = length(file_lines)
    }

    # extract ses lines
    ses_lines = file_lines[start_line:end_line]

    #### Extract ses_-level data ####
    type = unlist(strsplit(ses_lines[grep('Type', ses_lines)], '\t'))[3]
    screen1 = unlist(strsplit(ses_lines[grep('Screen1', ses_lines)], '\t'))[3]
    goal1 = unlist(strsplit(ses_lines[grep('Goal1', ses_lines)], '\t'))[3]
    time_taken1 = unlist(strsplit(ses_lines[grep('Time Taken1', ses_lines)], '\t'))[3]
    screen2 = unlist(strsplit(ses_lines[grep('Screen2', ses_lines)], '\t'))[3]
    goal2 = unlist(strsplit(ses_lines[grep('Goal2', ses_lines)], '\t'))[3]
    time_taken2 = unlist(strsplit(ses_lines[grep('Time Taken2', ses_lines)], '\t'))[3]

    # format TimeTaken: replace null values with empty string, remove 'seconds' or 'second'
    time_taken1 = ifelse(grepl('second', time_taken1), as.numeric(gsub(' seconds| second', '', time_taken1)), ifelse(is.null(time_taken1), NA, NA))
    time_taken2 = ifelse(grepl('second', time_taken2), as.numeric(gsub(' seconds| second', '', time_taken2)), ifelse(is.null(time_taken2), NA, NA))

    ## Extract Total Responses By Screen

    total_resp_start_line <- grep('Total Responses', ses_lines)
    avg_resp_start_line <- grep('Average Responses', ses_lines)
    total_resp_lines <- ses_lines[(total_resp_start_line+1):(avg_resp_start_line-2)]

    total_resp_data <- as.data.frame(t(sapply(seq(1, length(total_resp_lines)), function(x) proc_rrv_lines(line = x, resp_lines = total_resp_lines))))
    names(total_resp_data) <- c('screen', 'ses_n_resp', 'ses_reinforcer')

    total_resp_data <- sapply(total_resp_data, function(x) as.numeric(x))

    ## Extract Average Responses By Screen

    block1_start_line <- grep('Time Block : 1$', ses_lines) # the $ means end of line, so that Time Block : 10, etc. does not count
    avg_resp_lines <- ses_lines[(avg_resp_start_line+1):(block1_start_line-2)]

    avg_resp_data <- as.data.frame(t(sapply(seq(1, length(avg_resp_lines)), function(x) proc_rrv_lines(line = x, resp_lines = avg_resp_lines))))
    names(avg_resp_data) <- c('screen', 'ses_avg_resp', 'ses_avg_reinforcer')

    avg_resp_data <- sapply(avg_resp_data, function(x) as.numeric(x))

    ### Extract info for each time block ###

    # Use grep to find lines containing the string 'Time Block'
    block_start_lines <- grep('Time Block', ses_lines)

    # count the number of blocks
    n_blocks <- length(block_start_lines)

    # extract data within blocks
    block_data <- do.call(rbind.data.frame, t(sapply(seq(1:n_blocks), function(x) proc_rrv_blocks(block_num = x, n_blocks = n_blocks, block_start_lines = block_start_lines, ses_lines = ses_lines), simplify = FALSE)))

    if(nrow(block_data) == 1){
      block_data <- as.data.frame(sapply(block_data, function(x) as.numeric(x), simplify = FALSE))
    } else {
      block_data <- as.data.frame(sapply(block_data, function(x) as.numeric(x)))
    }

    block_data['reinforcer'] <- ifelse(block_data[['screen']] == '1', goal1, ifelse(block_data[['screen']] == '2', goal2, NA))

    block_data['ses_time'] <- ifelse(block_data[['screen']] == '1', time_taken1, ifelse(block_data[['screen']] == '2', time_taken2, NA))

    block_data['schedule'] <- ifelse(block_data[['screen']] == '1', screen1, ifelse(block_data[['screen']] == '2', screen2, NA))

    rrv_data <- merge(block_data, total_resp_data, by = 'screen', all = TRUE)
    rrv_data <- merge(rrv_data, avg_resp_data, by = 'screen', all = TRUE)

    rrv_data['type'] <- ifelse(type == 'Slot Machine Game with Reinforcers', 'slot machine', NA)

    rrv_data['ses'] <- ses_num

    nblock_data <- as.data.frame(t(sapply(unique(rrv_data[['screen']]), function(x) proc_noresp_block(screen_num = x, rrv_data))))
    nblock_data <- sapply(nblock_data, function(x) as.numeric(x))

    rrv_data <- merge(rrv_data, nblock_data, by = 'screen', all = TRUE)

    return(rrv_data)
  }

  #### parse text file ####
  # read in lines from text file
  file_lines <- readLines(rrv_file)

  # abort function if 'Chronological Report' is not contained within the text file -- suggests this is not an RRV output text file
  if (!any(grepl('Chronological Report', file_lines))) {
    print(paste(rrv_file, 'does not contain information needed to parse. Aborting util_rrv_parse_text()'))
    return()
  }

  # get subject label used in data collection
  report_line <- file_lines[1]
  sub <- unlist(strsplit(report_line, ':'))[2]

  # Use grep to find lines containing the string 'Report'
  ses_start_lines <- grep('Chronological Report', file_lines)

  # count the number of ses_s
  n_ses <- length(ses_start_lines)

  ses_dat <- do.call(rbind.data.frame, t(sapply(seq(1:n_ses), function(x) proc_rrv_ses(ses_num = x, n_ses = n_ses, file_lines = file_lines), simplify = FALSE)))

  # add participant info
  ses_dat['sub'] <- sub_str

  # reorder columns
  ses_dat <- ses_dat[c('sub', 'screen', 'schedule', 'type',	'ses', 'ses_time', 'ses_n_resp', 'ses_reinforcer', 'ses_avg_resp', 'ses_avg_reinforcer', 'ses_blocks', 'ses_nonresp_blocks',  'block', 'reinforcer', 'block_resp')]

  ##### Return dataframe ####

  return(ses_dat)

}
