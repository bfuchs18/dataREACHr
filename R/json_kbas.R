#' json_kbas: Generates a json file for the Kid's Brand Awareness Survey
#'
#' This function generates a json file for Kid's Brand Awareness Survey
#' @return A string with data stored in JSON format containing meta-data for the KBAS
#'
#'
#' @export

json_kbas <- function() {

  kbas_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Kid\'s Brand Awareness Survey. This survey was developed in the Keller Lab for Project REACH. Two versions of the survey were administered (counterbalanced) which varied by item order: version A items begin with "va_" whereas version B items begin with "vb_".Participants were provided the following instructions: "Now we are going to play a game. For this game, we want you to match the correct picture with the brand logo or character. Some of the pictures will be of foods and others will be of toys. There are no wrong answers to this game. We just want to find out how many different foods and toy brands you can recognize. Does that make sense?"'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    kbas_form_date = list( Description = 'Date (YYYY-MM-DD) the KBAS was completed on Redcap'),
    kbas_version = list( Description = 'Version of KBAS adminisered. Version assignment required having >45 responses for a given version and no responses for the alternative version',
                         Derivative = TRUE),
    kbas_food_score = list( Description = 'KBAS food score: number of correct responses to food items. Derived in redcap',
                         Derivative = TRUE),
    kbas_toy_score = list( Description = 'KBAS toy score: number of correct responses to toy items. Derived in redcap',
                         Derivative = TRUE))

  # convert formatting to JSON
  kbas_json <- RJSONIO::toJSON(kbas_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(kbas_json, asText = TRUE))){
    print('kbas JSON file may be invalid')
  }

  return(kbas_json)

}
