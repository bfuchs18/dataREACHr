#' json_kbas: Generates a json file for the kbas
#'
#' This function generates a json file for XXXX
#' @return A string with data stored in JSON format containing meta-data for the KBAS
#'
#'
#' @export

json_kbas <- function() {

  kbas_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Kid\'s Brand Awareness Survey. Participants were provided the following instructions: "Now we are going to play a game. For this game, we want you to match the correct picture with the brand logo or character. Some of the pictures will be of foods and others will be of toys. There are no wrong answers to this game. We just want to find out how many different foods and toy brands you can recognize. Does that make sense?"',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    kbas_form_date = list( Description = 'Date (YYYY-MM-DD) the KBAS was completed on Redcap'),
    kbas1 = list( Description = 'add'),
    kbas_total = list( Description = '',
                      Derivative = TRUE))

  # convert formatting to JSON
  kbas_json <- RJSONIO::toJSON(kbas_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(kbas_json, asText = TRUE))){
    print('kbas JSON file may be invalid')
  }

  return(kbas_json)

}
