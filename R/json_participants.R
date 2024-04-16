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
    risk_status = list( Description = 'Familial risk status for obesity based on maternal_bmi_v1',
                        Levels = list('low-risk' = 'maternal bmi at visit 1: 18.5-25',
                                      'high-risk' = 'maternal bmi at visit 1: >= 30.0'),
                        Derivative = TRUE),
    sex = list( Description = 'Parent-reported child sex'),
    ethnicity = list( Description = 'Parent-reported child ethnicity'),
    race = list( Description = 'Parent-reported child race'),
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
                   Derivative = TRUE),
    demo_education_mom_v1 = list( Description = 'Parent-reported maternal education on visit 1. Can also be found in household.tsv (demo_education_mom where session_id = "ses-1") '),
    demo_education_mom_v5 = list( Description = 'Parent-reported maternal education on visit 5. Can also be found in household.tsv (demo_education_mom where session_id = "ses-2")'),
    demo_income_v1 = list( Description = 'Parent-reported family income on visit 1. Can also be found in household.tsv (demo_income where session_id = "ses-1")'),
    demo_income_v5 = list( Description = 'Parent-reported family income on visit 5. Can also be found in household.tsv (demo_income where session_id = "ses-2")'),
    maternal_bmi_v1 = list( Description = 'Maternal BMI at visit 1',
                            Unit = 'kg/m^2'),
    maternal_anthro_method_v1 = list( Description = 'Method used to determine maternal_bmi_v1',
                                      Levels = list('measured' = 'BMI calculated from height and weight measurements on visit 1',
                                                    'reported' = 'BMI calculated from parent-reported height and weight values on visit 1.')),
    child_protocol_order = list( Description = 'Child protocol visit order based on child_protocol dates',
                                 Derivative = TRUE),
    child_protocol_1_date = list( Description = 'Date of child visit protocol 1 (YYYY-MM-DD). This was always the first visit of the study'),
    child_protocol_2_date = list( Description = 'Date of child visit protocol 2 (YYYY-MM-DD)'),
    child_protocol_3_date = list( Description = 'Date of child visit protocol 3 (YYYY-MM-DD)'),
    child_protocol_4_date = list( Description = 'Date of child visit protocol 4 (YYYY-MM-DD)'),
    child_protocol_5_date = list( Description = 'Date of child visit protocol 5 (YYYY-MM-DD). This was always the final visit of the study')
    )

  # convert formatting to JSON
  participants_json <- RJSONIO::toJSON(participants_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(participants_json, asText = TRUE))){
    print('Participants JSON file may be invalid')
  }

  return(participants_json)

}
