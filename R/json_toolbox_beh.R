#' json_toolbox_beh: Generates a json file for the NIH toolbox behavioral response data (data in rawdata/beh)
#'
#' This function generates a json file for cleaned NIH toolbox assessment (response) data
#'
#' @return A string with data stored in JSON format containing meta-data for NIH toolbox assessment (response) data
#'
#'
#' @export

json_toolbox_beh <- function() {

  toolbox_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = ''),
    session_id = list( Description = ''),
    PIN = list( Description = ''),
    DeviceID = list( Description = ''),
    Assessment.Name = list( Description = ''),
    InstOrdr = list( Description = ''),
    InstSctn = list( Description = ''),
    ItmOrdr = list( Description = ''),
    Inst = list( Description = ''),
    Locale = list( Description = ''),
    ItemID = list( Description = ''),
    Response = list( Description = ''),
    Score = list( Description = ''),
    Theta = list( Description = ''),
    TScore = list( Description = ''),
    SE = list( Description = ''),
    DataType = list( Description = ''),
    Position = list( Description = ''),
    ResponseTime = list( Description = ''),
    DateCreated = list( Description = ''),
    InstStarted = list( Description = ''),
    InstEnded = list( Description = ''),
    App.Version = list( Description = ''),
    iPad.Version = list( Description = ''),
    Firmware.Version = list( Description = ''),
    registration_age = list( Description = ''),
    registration_education = list( Description = ''),
    registration_mothers_education = list( Description = ''),
    registration_gender = list( Description = ''),
    registration_handedness = list( Description = ''),
    registration_race = list( Description = ''),
    registration_ethnicity = list( Description = '')
  )


  # convert formatting to JSON
  toolbox_json <- RJSONIO::toJSON(toolbox_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(toolbox_json, asText = TRUE))){
    print('NIH toolbox beh JSON file may be invalid')
  }

  return(toolbox_json)

}
