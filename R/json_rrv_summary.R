#' json_rrv_summary: Generates a json file for the Relative Reinforcing Value of Food task summary data
#'
#' This function generates a json file for cleaned Relative Reinforcing Value of Food task summary data
#'
#' @return A string with data stored in JSON format containing meta-data for the Relative Reinforcing Value of Food task summary data
#'
#'
#' @export

json_rrv_summary <- function() {

  rrv_summary_list <- list(
    'FileLevelMetadata' = list(
      Description = "Relative Reinforcing Value of Food Task behavior summarized across the whole task",
      Sources = "bids::rawdata/sub*/ses-1/beh/sub*rrv_events.tsv"),
    'MeasurementToolMetadata' = list(
      Description = 'Relative Reinforcing Value of Food Task',
      Reference = 'Epstein, Leonard H., Suzanne M. Wright, Rocco A. Paluch, John Leddy, Larry W. Hawk Jr, Jodie L. Jaroni, Frances G. Saad, Susan Crystal-Mansour, and Caryn Lerman. "Food hedonics and reinforcement as determinants of laboratory food intake in smokers." Physiology & Behavior 81, no. 3 (2004): 511-517. doi: 10.1016/j.physbeh.2004.02.015',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/15135024/',
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    pmax_resp_food = list( Description = 'Maximum schedule with responses for food reinforcer',
                                Levels = list("0" = "no responses made",
                                              "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                              "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                              "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                              "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                              "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                              "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                              "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                              "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                              "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)")),
    pmax_complete_food = list( Description = 'Maximum schedule completed (i.e., 5 reinforcers recieved) for food reinforcer',
                                Levels = list("0" = "no schedule completed",
                                              "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                              "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                              "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                              "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                              "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                              "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                              "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                              "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                              "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)")),
    pmax_resp_toy = list( Description = 'Maximum schedule with responses for toy reinforcer',
                               Levels = list("0" = "no responses made",
                                             "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                             "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                             "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                             "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                             "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                             "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                             "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                             "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                             "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)")),
    pmax_complete_toy = list( Description = 'Maximum schedule completed (i.e., 5 reinforcers recieved) for toy reinforcer',
                               Levels = list("0" = "no schedule completed",
                                             "4" = "1:4 schedule (1 reinforcer per 4 responses)",
                                             "8" = "1:8 schedule (1 reinforcer per 8 responses)",
                                             "16" = "1:16 schedule (1 reinforcer per 16 responses)",
                                             "32" = "1:32 schedule (1 reinforcer per 32 responses)",
                                             "64" = "1:64 schedule (1 reinforcer per 64 responses)",
                                             "128" = "1:128 schedule (1 reinforcer per 128 responses)",
                                             "256" = "1:256 schedule (1 reinforcer per 256 responses)",
                                             "512" = "1:512 schedule (1 reinforcer per 512 responses)",
                                             "1024" = "1:1024 schedule (1 reinforcer per 1024 responses)")),
    rrv_food_resp = list( Description = 'Relative reinforcing value of food, based on pmax_resp: pmax_resp_food/(pmax_resp_food + pmax_resp_toy)'),
    rrv_food_complete = list( Description = 'Relative reinforcing value of food, based on pmax_completed: pmax_complete_food/(pmax_complete_food + pmax_complete_toy)'),
    total_resp_food = list( Description = 'Total number of responses across all schedules for food reinforcer'),
    total_resp_toy = list( Description = 'Total number of responses across all schedules for toy reinforcer'),
    total_reinforcers_food = list( Description = 'Total number of reinforcers earned across all schedules for food reinforcer'),
    total_reinforcers_toy = list( Description = 'Total number of reinforcers earned across all schedules for toy reinforcer'),
    total_time_food = list( Description = 'Total number of reinforcers earned across all schedules for food reinforcer',
                            Units = 'min'),
    total_time_toy = list( Description = 'Total number of reinforcers earned across all schedules for toy reinforcer',
                           Units = 'min'),
    mean_resp_rate_food = list( Description = 'Mean response rate for food reinforcer',
                                    Units = 'responses/min'),
    mean_resp_rate_toy = list( Description = 'Mean response rate for toy reinforcer',
                                   Units = 'responses/min')
    )


  # convert formatting to JSON
  rrv_summary_json <- RJSONIO::toJSON(rrv_summary_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(rrv_summary_json, asText = TRUE))){
    print('RRV summary derivative JSON file may be invalid')
  }


  return(rrv_summary_json)

}
