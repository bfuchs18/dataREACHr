#' json_v1_anthro: Generates a json file for visit 1 anthropometrics data
#'
#' This function generates a json file for isit 1 anthropometrics data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v1_anthro <- function() {

  v1_anthro_list <- list(
    hild_height_1_cm = list( Description = 'child height measurement 1',
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
    child_weight_average = list( Description = 'average of child_weight_1_kg and child_weight_2_kg',
                                 Unit = "kg",
                                 Derivative = TRUE),
    parent1_sex = list( Description = 'Sex of parent being measured',
                        Levels = list ('female' = 'female',
                                       'male' = 'male')),
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
                                      Derivative = TRUE)
  )

  # convert formatting to JSON
  v1_anthro_json <- RJSONIO::toJSON(v1_anthro_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v1_anthro_json, asText = TRUE))){
    print('Anthropometrics data for visit 1 JSON file may be invalid')
  }

  return(v1_anthro_json)
}
