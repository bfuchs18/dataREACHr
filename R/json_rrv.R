#' json_rrv: Generates a json file for RRV data in bids/rawdata
#'
#' This function generates a json file for RRV data in bids/rawdata
#'
#' @return A string with data stored in JSON format containing meta-data for the RRV task
#'
#'
#' @export

json_rrv <- function() {

  rrv_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    ID = list( Description = 'ID entered into RRV software for data collection'),
    screen = list( Description = '',
                   Levels = list ('1' = 'screen 1',
                                  '2' = 'screen 2')),
    reinforcer = list( Description = 'Reinforcer',
                     Levels = list ('Candy' = 'candy',
                                    'Toy' = 'toy')),
    type = list( Description = 'type of game',
                 Levels = list ('slot machine' = 'slot machine')),
    session = list( Description = 'RRV task session'),
    total_time = list( Description = 'Time spent within a session for a given reinforcer/screen',
                       Unit = "seconds"),
    schedule = list( Description = 'Reinforcement schedule',
                  Levels = list ('Variable Response Based 1:4' = 'description',
                                 'Variable Response Based 1:8' = 'description')),
    block = list( Description = 'Block number within a given session for a given reinforcer/screen'),
    responses = list( Description = 'Number of responses for a given block'),
    reinforcers = list( Description = 'Numer of reinforcers earned for a given block'),
    total_responses = list( Description = 'Total numer of responses within a session for a given reinforcer/screen'),
    total_reinforcers = list( Description = 'Total numer of reinforcers within a session for a given reinforcer/screen'),
    average_responses = list( Description = '??'),
    average_reinforcers = list( Description = '??'),
    total_blocks = list( Description = 'Total number of blocks within a session for a given reinforcer/screen'),
    total_nonresp_blocks = list( Description = 'Total number of blocks with 0 responses within a session for a given reinforcer/screen')
    )


  # convert formatting to JSON
  rrv_json <- RJSONIO::toJSON(rrv_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(rrv_json, asText = TRUE))){
    print('rrv JSON file may be invalid')
  }

  return(rrv_json)

}
