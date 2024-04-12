#' json_participants: Generates a json file for participants.tsv
#'
#' This function generates a json file for participants.tsv
#'
#' @return A string with data stored in JSON format containing meta-data for participants.tsv
#'
#'
#' @export

json_participants <- function() {

  participants_list <- list(
    participant_id = list( Description = 'participant id number'),
    sex = list( Description = 'Parent-reported child sex'),
    ethnicity = list( Description = 'Parent-reported child ethnicity'),
    race = list( Description = 'Parent-reported child race'),
    education_mom_v1 = list( Description = 'Parent-reported maternal education on visit 1'),
    education_mom_v5 = list( Description = 'Parent-reported maternal education on visit 5'),
    income_v1 = list( Description = 'Parent-reported family income on visit 1'),
    income_v5 = list( Description = 'Parent-reported family income on visit 5'),
    child_protocol_1_date = list( Description = 'Date of child visit protocol 1 (YYYY-MM-DD). This was always the first visit of the study'),
    child_protocol_2_date = list( Description = 'Date of child visit protocol 2 (YYYY-MM-DD)'),
    child_protocol_3_date = list( Description = 'Date of child visit protocol 3 (YYYY-MM-DD)'),
    child_protocol_4_date = list( Description = 'Date of child visit protocol 4 (YYYY-MM-DD)'),
    child_protocol_5_date = list( Description = 'Date of child visit protocol 5 (YYYY-MM-DD). This was always the final visit of the study'),
    child_protocol_1_age = list( Description = 'Child age at child visit protocol 1',
                   Unit = 'years',
                   Derivative = TRUE),
    child_protocol_1_age = list( Description = 'Child age at child visit protocol 2',
                   Unit = 'years',
                   Derivative = TRUE),
    child_protocol_1_age = list( Description = 'Child age at child visit protocol 3',
                   Unit = 'years',
                   Derivative = TRUE),
    child_protocol_1_age = list( Description = 'Child age at child visit protocol 4',
                   Unit = 'years',
                   Derivative = TRUE),
    child_protocol_1_age = list( Description = 'Child age at child visit protocol 5',
                   Unit = 'years',
                   Derivative = TRUE)
    )

  # convert formatting to JSON
  participants_json <- RJSONIO::toJSON(participants_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(participants_json, asText = TRUE))){
    print('Participants JSON file may be invalid')
  }

  return(participants_json)

}
