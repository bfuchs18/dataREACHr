#' json_v1_food_paradigm: Generates a json file for visit 1 food paradigm notes and data
#'
#' This function generates a json file for visit 1 food paradigm notes and data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v1_food_paradigm <- function() {

  v1_food_paradigm_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    test_meal_book = list( Description = 'Book the child selected to listen to during the test meal'),
    test_meal_start_time = list( Description = 'Meal start time',
                                 Unit = "hh:mm"),
    test_meal_15_min_check = list( Description = 'Child was informed that there are 15 minutes remaining to eat their meal:',
                                   Levels = list ('1' =	'Yes',
                                                  '0'	= 'No')),
    test_meal_end_time = list( Description = 'Meal end time',
                               Unit = "hh:mm"),
    test_meal_duration = list( Description = 'Meal duration. Derived in redcap from test_meal_start_time and test_meal_end_time',
                               Derivative = TRUE),
    test_meal_notes = list( Description = 'Researcher notes about meal protocol'),
    meal_prep_notes = list( Description = 'Researcher notes about meal food preparation')
  )

  # convert formatting to JSON
  v1_food_paradigm_json <- RJSONIO::toJSON(v1_food_paradigm_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v1_food_paradigm_json, asText = TRUE))){
    print('V1 food paradigm JSON file may be invalid')
  }

  return(v1_food_paradigm_json)

}
