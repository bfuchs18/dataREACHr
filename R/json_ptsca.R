#' json_pstca: Generates a json file for the XX
#'
#' This function generates a json file for the XX
#'
#' @return A string with data stored in JSON format containing meta-data for the XX
#'
#'
#' @export

json_pstca <- function() {

  pstca_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    pstca_form_date = list( Description = 'Date (YYYY-MM-DD) pstca form was completed on redcap'),
    pstca1 = list( Description = '')
    )

  # convert formatting to JSON
  pstca_json <- RJSONIO::toJSON(pstca_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(pstca_json, asText = TRUE))){
    print('pstca JSON file may be invalid')
  }

  return(pstca_json)

}
