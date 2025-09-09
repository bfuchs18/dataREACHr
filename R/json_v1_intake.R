#' json_v1_intake: Generates a json file for intake data for V1
#'
#' This function generates a json file for intake data for V1
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v1_intake <- function() {

  v1_intake_list <- list(
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_protocol = list( Description = 'child visit protocol number (does not necessarilty reflect visit order. See participants.tsv for child visit protocol dates)',
                           Levels = list ('1' =	'Child visit protocol 1',
                                          '3' =	'Child visit protocol 3',
                                          '4' =	'Child visit protocol 4',
                                          '5'	= 'Child visit protocol 5')),
    visit_date = list( Description = 'Date of visit',
                       Unit = 'YYYY-MM-DD'),
    bread_pre_w_o_plate = list( Description = 'Pre-meal weight of bread (grilled cheese component) without plate',
                                Unit = "grams"),
    butter_pre_w_o_plate = list( Description = 'Pre-meal weight of butter (grilled cheese component) on bread slice A without plate',
                                 Unit = "grams"),
    butter_2_pre_w_o_plate = list( Description = 'Pre-meal weight of butter (grilled cheese component) on bread slice B without plate',
                                   Unit = "grams"),
    cheese_pre_w_o_plate = list( Description = 'Pre-meal weight of cheese (grilled cheese component) without plate',
                                 Unit = "grams"),

    grilled_cheese_pre_w_o_plate = list( Description = 'Pre-meal weight of grilled cheese without plate',
                                         Unit = "grams"),
    grilled_cheese_pre_w_plate = list( Description = 'Pre-meal weight of grilled cheese with plate',
                                       Unit = "grams"),
    grilled_cheese_post_w_plate = list( Description = 'Post-meal weight of grilled cheese with plate',
                                        Unit = "grams"),

    tender_pre_w_o_plate = list( Description = 'Pre-meal weight of chicken tender without plate',
                                 Unit = "grams"),
    tender_pre_w_plate = list( Description = 'Pre-meal weight of chicken tender with plate',
                               Unit = "grams"),
    tender_post_w_plate = list( Description = 'Post-meal weight of chicken tender with plate',
                                Unit = "grams"),

    carrot_pre_w_o_plate = list( Description = 'Pre-meal weight of carrots without plate',
                                 Unit = "grams"),
    carrot_pre_w_plate = list( Description = 'Pre-meal weight of carrots with plate',
                               Unit = "grams"),
    carrot_post_w_plate = list( Description = 'Post-meal weight of carrots with plate',
                                Unit = "grams"),

    chips_pre_w_o_plate = list( Description = 'Pre-meal weight of potato chips without plate',
                                Unit = "grams"),
    chips_pre_w_plate = list( Description = 'Pre-meal weight of potato chips with plate',
                              Unit = "grams"),
    chips_post_w_plate = list( Description = 'Post-meal weight of potato chips with plate',
                               Unit = "grams"),

    fruit_pre_w_o_plate = list( Description = 'Pre-meal weight of fruit cocktail without plate',
                                Unit = "grams"),
    fruit_pre_w_plate = list( Description = 'Pre-meal weight of fruit cocktail with plate',
                              Unit = "grams"),
    fruit_post_w_plate = list( Description = 'Post-meal weight of fruit cocktail with plate',
                               Unit = "grams"),

    water_pre_w_o_plate = list( Description = 'Pre-meal weight of water without container',
                                Unit = "grams"),
    water_pre_w_plate = list( Description = 'Pre-meal weight of water with container',
                              Unit = "grams"),
    water_post_w_plate = list( Description = 'Post-meal weight of water with container',
                               Unit = "grams"),

    ranch_pre_w_o_plate = list( Description = 'Pre-meal weight of ranch dressing without container',
                                Unit = "grams"),
    ranch_pre_w_plate = list( Description = 'Pre-meal weight of ranch dressing with container',
                              Unit = "grams"),
    ranch_post_w_plate = list( Description = 'Post-meal weight of ranch dressing with container',
                               Unit = "grams"),

    ketchup_pre_w_o_plate = list( Description = 'Pre-meal weight of ketchup without container',
                                Unit = "grams"),
    ketchup_pre_w_plate = list( Description = 'Pre-meal weight of ketchup with container',
                              Unit = "grams"),
    ketchup_post_w_plate = list( Description = 'Post-meal weight of ketchup with container',
                               Unit = "grams")
  )

  # convert formatting to JSON
  v1_intake_json <- RJSONIO::toJSON(v1_intake_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v1_intake_json, asText = TRUE))){
    print('Intake for visit 1 JSON file may be invalid')
  }

  return(v1_intake_json)

}
