#' json_dexa: Generates a json file for the dexa
#'
#' This function generates a json file for XXXX
#' @return A string with data stored in JSON format containing meta-data for dexa data
#'
#'
#' @export

json_dexa <- function() {

  dexa_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    dexa1 = list( Description = 'add'),
    dexa_total = list( Description = '',
                       Derivative = TRUE))

  # convert formatting to JSON
  dexa_json <- RJSONIO::toJSON(dexa_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(dexa_json, asText = TRUE))){
    print('dexa JSON file may be invalid')
  }

  return(dexa_json)

}
