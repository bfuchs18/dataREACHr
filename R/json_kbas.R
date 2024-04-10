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
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
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