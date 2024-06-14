#' json_deriv_rrv: Generates a json file for the derivative RRV data
#'
#' This function generates a json file the summary RRV outcomes reported in derivatives/beh_databases
#'
#' @return A string with data stored in JSON format containing meta-data the summary RRV database
#'
#'
#' @export

json_deriv_rrv <- function() {

  rrv_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '' ,
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    pmax_responded_food = list( Description = 'Maximum schedule with responses for food reinforcer',
                                Levels = list("0" = "no responses made",
                                              "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                              "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                              "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                              "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                              "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                              "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                              "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                              "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                              "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                                Derivative = TRUE),
    pmax_completed_food = list( Description = 'Maximum schedule completed (i.e., 5 reinforcers recieved) for food reinforcer',
                                Levels = list("0" = "no schedule completed",
                                              "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                              "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                              "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                              "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                              "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                              "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                              "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                              "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                              "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                                Derivative = TRUE),
    pmax_responded_toy = list( Description = 'Maximum schedule with responses for toy reinforcer',
                               Levels = list("0" = "no responses made",
                                             "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                             "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                             "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                             "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                             "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                             "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                             "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                             "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                             "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                               Derivative = TRUE),
    pmax_completed_toy = list( Description = 'Maximum schedule completed (i.e., 5 reinforcers recieved) for toy reinforcer',
                               Levels = list("0" = "no schedule completed",
                                             "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                             "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                             "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                             "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                             "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                             "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                             "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                             "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                             "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)"),
                               Derivative = TRUE),
    rrv_food_responded = list( Description = 'Relative reinforcing value of food, based on pmax_responded: pmax_responded_food/(pmax_responded_food + pmax_responded_toy)',
                                Derivative = TRUE),
    rrv_food_completed = list( Description = 'Relative reinforcing value of food, based on pmax_completed: pmax_completed_food/(pmax_completed_food + pmax_completed_toy)',
                                Derivative = TRUE),
    total_responses_food = list( Description = 'Total number of responses across all schedules for food reinforcer',
                               Derivative = TRUE),
    total_responses_toy = list( Description = 'Total number of responses across all schedules for toy reinforcer',
                               Derivative = TRUE),
    mean_response_rate_food = list( Description = 'Mean response rate (responses / min) for food reinforcer',
                                Derivative = TRUE),
    mean_response_rate_toy = list( Description = 'Mean response rate (responses / min) for toy reinforcer',
                                    Derivative = TRUE)
    )

  # convert formatting to JSON
  rrv_json <- RJSONIO::toJSON(rrv_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(rrv_json, asText = TRUE))){
    print('RRV derivative JSON file may be invalid')
  }

  return(rrv_json)

}
