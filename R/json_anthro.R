#' json_anthro: Generates a json file for anthropometric data
#'
#' This function generates a json file for anthropometric data
#'
#' @return A string with data stored in JSON format containing meta-data anthropometric data
#'
#'
#' @export

json_anthro <- function() {

  anthro_list <- list(
    participant_id = list( Description = 'participant id number'),
    visit = list( Description = 'participant id number'),
    session_id = list( Description = 'session description'),
    child_height_1_cm = list( Description = 'child height measurement 1',
                              Unit = "cm"),
    child_height_2_cm = list( Description = 'child height measurement 2',
                              Unit = "cm"),
    child_weight_1_kg = list( Description = 'child weight measurement 1',
                              Unit = "kg"),
    child_weight_2_kg = list( Description = 'child weight measurement 2',
                              Unit = "kg"),
    child_height_average = list( Description = 'average of child_height_1_cm and child_height_2_cm',
                                 Unit = "cm",
                                 Derivative = TRUE),
    child_average_weight = list( Description = 'average of child_weight_1_kg and child_weight_2_kg',
                                 Unit = "kg",
                                 Derivative = TRUE),
    parent1_sex = list( Description = 'sex of parent with measured height and weight'),
    parent1_height_1_cm = list( Description = 'parent height measurement 1',
                                Unit = "cm"),
    parent1_height_2_cm = list( Description = 'parent height measurement 2',
                                Unit = "cm"),
    parent1_weight_1_kg = list( Description = 'parent weight measurement 1',
                                Unit = "kg"),
    parent1_weight_2_kg = list( Description = 'parent weight measurement 2',
                                Unit = "kg"),
    parent1_height_average_cm = list( Description = 'average of parent1_height_1_cm and parent1_height_2_cm',
                                      Unit = "cm",
                                      Derivative = TRUE),
    parent1_weight_average_kg = list( Description = 'average of parent1_weight_1_kg and parent1_weight_2_kg',
                                      Unit = "kg",
                                      Derivative = TRUE),
    parent1_bmi_redcap_calc = list( Description = '',
                                    Derivative = TRUE),
    child_bmi = list( Description = 'Child BMI',
                      Derivative = TRUE),
    child_bmi_p = list( Description = 'Child BMI-for-age percentile based on the LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Calculated in R using childsds package',
                        Derivative = TRUE),
    child_bmi_z = list( Description = 'Child BMI-for-age z-score baesd on the LMS Parameters for the Centers for Disease Control and Prevention 2000 Growth Charts. Calculated in R using childsds package',
                        Derivative = TRUE)
  )

  # convert formatting to JSON
  anthro_json <- RJSONIO::toJSON(anthro_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(anthro_json, asText = TRUE))){
    print('MRI visit JSON file may be invalid')
  }

  return(anthro_json)

}
