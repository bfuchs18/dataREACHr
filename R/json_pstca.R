#' json_ptsca: Generates a json file for the XX
#'
#' This function generates a json file for the XX
#'
#' @return A string with data stored in JSON format containing meta-data for the XX
#'
#'
#' @export

json_ptsca <- function() {

  ptsca_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    ptsca_form_date = list( Description = 'Date (YYYY-MM-DD) PTSCA form was completed on redcap'),
    ptsca1 = list( Description = '')
    )

  # convert formatting to JSON
  ptsca_json <- RJSONIO::toJSON(ptsca_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(ptsca_json, asText = TRUE))){
    print('ptsca JSON file may be invalid')
  }

  return(ptsca_json)

}
