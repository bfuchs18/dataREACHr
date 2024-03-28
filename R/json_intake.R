#' json_intake: Generates a json file for intake data
#'
#' This function generates a json file for intake data
#'
#' @return A json file documenting intake data
#'
#'
#' @export

json_intake <- function() {

  intake_list <- list(
    participant_id = list( Description = 'participant id number'),
    visit = list( Description = 'participant id number'),
    session_id = list( Description = ''),

    bread_pre_w_o_plate = list( Description = ''),
    butter_pre_w_o_plate = list( Description = ''),
    butter_2_pre_w_o_plate = list( Description = ''),
    cheese_pre_w_o_plate = list( Description = ''),

    grilled_cheese_pre_w_o_plate = list( Description = ''),
    grilled_cheese_pre_w_plate = list( Description = ''),
    grilled_cheese_post_w_plate = list( Description = ''),
    grilled_cheese_grams_consumed = list( Description = '',
                                          Derivative = TRUE),

    tender_pre_w_o_plate = list( Description = ''),
    tender_pre_w_plate = list( Description = ''),
    tender_post_w_plate = list( Description = ''),
    tender_grams_consumed = list( Description = '',
                                  Derivative = TRUE),

    carrot_pre_w_o_plate = list( Description = ''),
    carrot_pre_w_plate = list( Description = ''),
    carrot_post_w_plate = list( Description = ''),
    carrot_grams_consumed = list( Description = '',
                                  Derivative = TRUE),

    chips_pre_w_o_plate = list( Description = ''),
    chips_pre_w_plate = list( Description = ''),
    chips_post_w_plate = list( Description = ''),
    chips_grams_consumed = list( Description = '',
                                 Derivative = TRUE),

    fruit_pre_w_o_plate = list( Description = ''),
    fruit_pre_w_plate = list( Description = ''),
    fruit_post_w_plate = list( Description = ''),
    fruit_grams_consumed = list( Description = '',
                                 Derivative = TRUE),

    water_pre_w_o_plate = list( Description = ''),
    water_pre_w_plate = list( Description = ''),
    water_post_w_plate = list( Description = ''),
    water_grams_consumed = list( Description = '',
                                 Derivative = TRUE),

    ranch_pre_w_o_plate = list( Description = ''),
    ranch_pre_w_plate = list( Description = ''),
    ranch_post_w_plate = list( Description = ''),
    ranch_grams_consumed = list( Description = '',
                                 Derivative = TRUE),

    meal_grams_consumed = list( Description = '',
                                Derivative = TRUE),

    pre_ad_meal_fullness = list( Description = ''),
    pre_meal_fullness = list( Description = ''),
    post_meal_fullness = list( Description = ''),
    pre_ad_eah_fullness = list( Description = ''),
    pre_eah_fullness = list( Description = ''),
    post_eah_fullness = list( Description = ''),

    ad_cond_eah = list( Description = ''), # include this

    brownie_pre_w_o_plate = list( Description = ''),
    brownie_pre_w_plate = list( Description = ''),
    brownie_post_w_plate = list( Description = ''),
    brownie_grams_consumed = list( Description = '',
                                   Derivative = TRUE),

    corn_chip_pre_w_o_plate = list( Description = ''),
    corn_chip_pre_w_plate = list( Description = ''),
    corn_chip_post_w_plate = list( Description = ''),
    corn_chip_grams_consumed = list( Description = '',
                                     Derivative = TRUE),

    kiss_pre_w_o_plate = list( Description = ''),
    kiss_pre_w_plate = list( Description = ''),
    kiss_post_w_plate = list( Description = ''),
    kiss_grams_consumed = list( Description = '',
                                Derivative = TRUE),

    ice_cream_pre_w_o_plate = list( Description = ''),
    ice_cream_pre_w_plate = list( Description = ''),
    ice_cream_post_w_plate = list( Description = ''),
    ice_cream_grams_consumed = list( Description = '',
                                     Derivative = TRUE),

    oreo_pre_w_o_plate = list( Description = ''),
    oreo_cream_pre_w_plate = list( Description = ''),
    oreo_cream_post_w_plate = list( Description = ''),
    oreo_cream_grams_consumed = list( Description = '',
                                      Derivative = TRUE),

    popcorn_pre_w_o_plate = list( Description = ''),
    popcorn_cream_pre_w_plate = list( Description = ''),
    popcorn_cream_post_w_plate = list( Description = ''),
    popcorn_cream_grams_consumed = list( Description = '',
                                         Derivative = TRUE),

    pretzel_pre_w_o_plate = list( Description = ''),
    pretzel_cream_pre_w_plate = list( Description = ''),
    pretzel_cream_post_w_plate = list( Description = ''),
    pretzel_cream_grams_consumed = list( Description = '',
                                         Derivative = TRUE),

    skittle_pre_w_o_plate = list( Description = ''),
    skittle_cream_pre_w_plate = list( Description = ''),
    skittle_cream_post_w_plate = list( Description = ''),
    skittle_cream_grams_consumed = list( Description = '',
                                         Derivative = TRUE),

    starburst_pre_w_o_plate = list( Description = ''),
    starburst_cream_pre_w_plate = list( Description = ''),
    starburst_cream_post_w_plate = list( Description = ''),
    starburst_cream_grams_consumed = list( Description = '',
                                           Derivative = TRUE),

    water_eah_pre_w_o_plate = list( Description = ''),
    water_eah_cream_pre_w_plate = list( Description = ''),
    water_eah_cream_post_w_plate = list( Description = ''),
    water_eah_cream_grams_consumed = list( Description = '',
                                           Derivative = TRUE),

    eah_grams_consumed = list( Description = '',
                               Derivative = TRUE),

    ad_cond_meal = list( Description = '') # include this?
  )

  # convert formatting to JSON
  intake_json <- RJSONIO::toJSON(intake_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(intake_json, asText = TRUE))){
    print('Intake visit JSON file may be invalid')
  }

  return(intake_json)

}
