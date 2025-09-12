#' json_foodview_byblock: Generates json file for the derivative Food View Task data
#'
#' This function generates a json file for Food View summary data reported in derivatives/func-beh by block
#'
#' @return A JSON containing meta-data the summary Food View Task database
#'
#'
#' @export

json_foodview_byblock <- function() {

  foodview_byblock_list <- list(
    'FileLevelMetadata' = list(
      Description = "Foodview task behavior summarized by block",
      Sources = "bids::rawdata/sub*/ses-1/func/sub*foodview*events.tsv",
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    run_num = list( Description = 'Food View Task run number',
                    Levels = list ('1' = 'run 1',
                                   '2' = 'run 2',
                                   '3' = 'run 3',
                                   '4' = 'run 4')),
    block_num = list( Description = 'Block number within a run',
                    Levels = list ('1' = 'block 1',
                                   '2' = 'block 2',
                                   '3' = 'block 3',
                                   '4' = 'block 4')),
    commercial_cond = list( Description = 'commercial condition',
                            Levels = list ('food' = 'food',
                                           'toy' = 'toy')),
    food_ed = list( Description = 'block energy density category',
                    Levels = list ('low' = 'low energy density foods',
                                   'high' = 'high energy density foods')),
    food_taste = list( Description = 'block taste category',
                       Levels = list ('sweet' = 'sweet foods',
                                      'savory' = 'savory foods')),
    n_image = list( Description = 'Number of images in a block'),
    n_resp = list( Description = 'Number of responses in given block. For a response to counted, the response_time had to be >0ms.'),
    n_want = list( Description = 'Number of want responses in given block. For a response to counted, the response_time had to be >0ms.'),
    p_resp = list( Description = 'Proportion of responses out of images in a given block. For a response to counted, the response_time had to be >0ms.'),
    p_want = list( Description = 'Proportion of "want" responses out of all responses in a given block. For a response to counted, the response_time had to be >0ms.'),
    avg_rt = list(Description = 'Average reaction time for responses in given block. For a response to counted, the response_time had to be >0ms.',
                  Unit = "ms"),
    med_rt = list(Description = 'Median reaction time for responses in given block. For a response to counted, the response_time had to be >0ms.',
                  Unit = "ms")
  )

  # convert and return JSONS ----

  # convert formatting to JSON
  foodview_byblock_json <- RJSONIO::toJSON(foodview_byblock_list, pretty = TRUE)


  if (isFALSE(RJSONIO::isValidJSON(foodview_byblock_json, asText = TRUE))){
    print('foodview_byblock_json JSON file may be invalid')
  }

  return(foodview_byblock_json)

}
