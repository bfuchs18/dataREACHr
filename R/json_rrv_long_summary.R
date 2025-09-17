#' json_rrv_long_summary: Generates a json file for the Relative Reinforcing Value of Food task long summary data by session and screen
#'
#' This function generates a json file for cleaned Relative Reinforcing Value of Food task long summary data by session and screen
#'
#' @return A string with data stored in JSON format containing meta-data for the Relative Reinforcing Value of Food task for long summary data by session and screen
#'
#'
#' @export

json_rrv_long_summary <- function() {

  rrv_long_summary_list <- list(
    'FileLevelMetadata' = list(
      Description = "Relative Reinforcing Value of Food Task behavior summarized by session and screen",
      Sources = "bids::rawdata/sub*/ses-1/beh/sub*rrv_events.tsv"),
    'MeasurementToolMetadata' = list(
      Description = 'Relative Reinforcing Value of Food Task',
      Reference = 'Epstein, Leonard H., Suzanne M. Wright, Rocco A. Paluch, John Leddy, Larry W. Hawk Jr, Jodie L. Jaroni, Frances G. Saad, Susan Crystal-Mansour, and Caryn Lerman. "Food hedonics and reinforcement as determinants of laboratory food intake in smokers." Physiology & Behavior 81, no. 3 (2004): 511-517. doi: 10.1016/j.physbeh.2004.02.015',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/15135024/',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    screen = list( Description = 'Screen display',
                   Levels = list ('1' = 'screen 1',
                                  '2' = 'screen 2')),
    schedule = list( Description = 'Reinforcement schedule',
                     Levels = list ('Variable Response Based 1:4' = '1 reinforcer earned per 4 responses',
                                    'Variable Response Based 1:8' = '1 reinforcer earned per 8 responses')),
    type = list( Description = 'type of game',
                 Levels = list ('slot machine' = 'slot machine')),
    ses = list( Description = 'RRV task session number. Each session corresponds to a different reinforcement schedule'),
    ses_time = list( Description = 'Time spent within a session for a given reinforcer/screen',
                     Unit = "seconds"),
    ses_n_resp = list( Description = 'Number of responses made within a session for a given reinforcer'),
    ses_reinforcer = list( Description = 'Number of reinforcers earned within a session for a given reinforcer'),
    ses_avg_resp = list( Description = 'average number of total responses per minute'),
    ses_avg_reinforcer = list( Description = 'average number of reinforcers per minute'),
    ses_blocks = list( Description = 'Number of blocks within a session for a given reinforcer'),
    ses_nonresp_blocks = list( Description = 'Number of blocks with 0 responses within a session for a given reinforcer'),
    reinforcer = list( Description = 'Reinforcer',
                       Levels = list ('Candy' = 'candy',
                                      'Toy' = 'toy'))
    )


  # convert formatting to JSON
  rrv_long_summary_json <- RJSONIO::toJSON(rrv_long_summary_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(rrv_long_summary_json, asText = TRUE))){
    print('RRV long summary derivative JSON file may be invalid')
  }


  return(rrv_long_summary_json)

}
