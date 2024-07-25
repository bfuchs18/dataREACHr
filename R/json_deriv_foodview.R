#' json_deriv_foodview: Generates json file for the derivative Foodview data
#'
#' This function XXXXX
#'
#' @return A list of 2 strings (labels: XX, XX) with data stored in JSON format containing meta-data the summary Foodview database
#'
#'
#' @export

json_deriv_foodview <- function() {

  foodview_bycond_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '' ,
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    commercial_cond = list( Description = '',
                                Levels = list("toy" = "",
                                              "food" = "")),
    n_image = list(Description = 'number of image events for given commercial_cond',
                    Derivative = TRUE),
    p_resp = list(Description = 'proportion of image events with response out of the total number of image events',
                    Derivative = TRUE),
    p_want_of_resp = list(Description = 'proportion of items a child reported wanting out of the total number of responses they provided',
                          Derivative = TRUE),
    avg_rt = list(Description = 'average reaction time',
                  unit = "seconds",
                  Derivative = TRUE),
    hed_n_image = list(Description = 'number of high ED image events for given commercial_cond',
                  Derivative = TRUE),
    hed_p_resp = list(Description = '',
                  Derivative = TRUE),
    hed_p_want_of_resp = list( Description = '',
                  Derivative = TRUE),
    hed_avg_rt = list(Description = '',
                  Derivative = TRUE),
    led_n_image = list(Description = 'number of low ED image events for given commercial_cond',
                  Derivative = TRUE),
    led_p_resp = list(Description = '',
                  Derivative = TRUE),
    led_p_want_of_resp = list( Description = '',
                    Derivative = TRUE),
    led_avg_rt = list( Description = '',
                    Derivative = TRUE),
    sweet_n_image = list( Description = 'number of sweet image events for given commercial_cond',
                    Derivative = TRUE),
    sweet_p_want_of_resp = list( Description = '',
                    Derivative = TRUE),
    sweet_avg_rt = list( Description = '',
                         Derivative = TRUE),
    savory_n_image = list( Description = 'number of savory image events for given commercial_cond',
                         Derivative = TRUE),
    savory_p_resp = list( Description = '',
                         Derivative = TRUE),
    savory_p_want_of_resp = list( Description = '',
                         Derivative = TRUE),
    savory_avg_rt = list( Description = '',
                         Derivative = TRUE)
  )


  # rrv_summary_long_list <- list(
  #   'MeasurementToolMetadata' = list(
  #     Description = '',
  #     Reference = '',
  #     TermURL = ''),
  #   participant_id = list( Description = 'participant id number'),
  #   reinforcer = list( Description = 'Reinforcer',
  #                      Levels = list ('Candy' = 'candy',
  #                                     'Toy' = 'toy')),
  #   session = list( Description = 'RRV task session number. Each session corresponds to a different reinforcement schedule'),
  #   session_time = list( Description = 'Time spent within a session for a given reinforcer/screen',
  #                        Unit = "seconds"),
  #   schedule = list( Description = 'Reinforcement schedule',
  #                    Levels = list ('Variable Response Based 1:4' = '1 reinforcer earned per 4 responses',
  #                                   'Variable Response Based 1:8' = '1 reinforcer earned per 8 responses')),
  #   session_responses = list( Description = 'Number of responses made within a session for a given reinforcer'),
  #   session_reinforcers = list( Description = 'Number of reinforcers earned within a session for a given reinforcer'),
  #   session_blocks = list( Description = 'Number of blocks within a session for a given reinforcer'),
  #   session_nonresp_blocks = list( Description = 'Number of blocks with 0 responses within a session for a given reinforcer'),
  #   session_average_responses = list( Description = '??'),
  #   session_average_reinforcers = list( Description = '??')
  # )

  # convert formatting to JSON
  foodview_bycond_json <- RJSONIO::toJSON(foodview_bycond_list, pretty = TRUE)
  # rrv_summary_long_json <- RJSONIO::toJSON(rrv_summary_long_list, pretty = TRUE)
  #
  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodview_bycond_json, asText = TRUE))){
    print('foodview_bycond_json JSON file may be invalid')
  }

  # if (isFALSE(RJSONIO::isValidJSON(rrv_summary_long_json, asText = TRUE))){
  #   print('RRV long summary derivative JSON file may be invalid')
  # }

  return(list(foodview_bycond_json = foodview_bycond_json))

}
