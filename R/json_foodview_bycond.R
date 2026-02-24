#' json_foodview_bycond: Generates json file for the derivative Food View Task data
#'
#' This function generates a json file for Food View summary data reported in derivatives/func-beh by cond
#'
#' @return A JSON containing meta-data the summary Food View Task database
#'
#'
#' @export

json_foodview_bycond <- function() {

  # JSON for dataframe by condition ----
  foodview_bycond_list <- list(
    'FileLevelMetadata' = list(
      Description = "Foodview task behavior summarized by commerical condition across all blocks/runs in the task",
      Sources = "bids::rawdata/sub*/ses-1/func/sub*foodview*events.tsv",
      DatasetType = 'derivative'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    n_image = list(Description = 'number of image events (i.e., the presentation of a food item where a child is to respond to the question "do you want this item?"'),
    n_resp = list( Description = 'Number of responses in given block. For a response to counted, the response_time had to be >0ms.'),
    n_want = list( Description = 'Number of want responses in given block. For a response to counted, the response_time had to be >0ms.'),
    p_resp = list( Description = 'proportion of image events with response out of the total number of image events. For a response to counted, the response_time had to be >0ms.'),
    p_want_of_resp = list(Description = 'proportion of items a child reported wanting out of the total number of responses they provided. For a response to counted, the response_time had to be >0ms'),
    avg_rt = list( Description = 'average reaction time for responses to all items. For a response to counted, the response_time had to be >0ms.',
                  unit = "ms"),
    med_rt = list( Description = 'median reaction time for responses to all items. For a response to counted, the response_time had to be >0ms.',
                   unit = "ms"),

    hed_n_image = list(Description = 'number of high ED image events'),
    hed_n_resp = list( Description = 'Number of high ED events with responses in given block. For a response to counted, the response_time had to be >0ms.'),
    hed_n_want = list( Description = 'Number of want responses in given high ED block. For a response to counted, the response_time had to be >0ms.'),
    hed_p_resp = list(Description = 'proportion of high ED events with a response out of the total number of high ED events. For a response to counted, the response_time had to be >0ms.'),
    hed_p_want_of_resp = list( Description = 'proportion of high ED items a child reported wanting out of the total number of high ED responses they provided. For a response to counted, the response_time had to be >0ms.'),
    hed_avg_rt = list(Description = 'average reaction time for responses to items in high ED food blocks. For a response to counted, the response_time had to be >0ms.',
                      unit = "ms"),
    hed_med_rt = list( Description = 'median reaction time for responses to all items in high ED food blocks. For a response to counted, the response_time had to be >0ms.',
                   unit = "ms"),

    led_n_image = list(Description = 'number of low ED image events for given commercial_cond'),
    led_n_resp = list( Description = 'Number of low ED events with responses in given block. For a response to counted, the response_time had to be >0ms.'),
    led_n_want = list( Description = 'Number of want responses in given low ED block. For a response to counted, the response_time had to be >0ms.'),
    led_p_resp = list(Description = 'proportion of low ED events with a response out of the total number of low ED events. For a response to counted, the response_time had to be >0ms.'),
    led_p_want_of_resp = list( Description = 'proportion of low ED items a child reported wanting out of the total number of low ED responses they provided. For a response to counted, the response_time had to be >0ms.'),
    led_avg_rt = list( Description = 'average reaction time for responses to items in low ED food blocks. For a response to counted, the response_time had to be >0ms.',
                       unit = "ms"),
    led_med_rt = list( Description = 'median reaction time for responses to all items in low ED food blocks. For a response to counted, the response_time had to be >0ms.',
                       unit = "ms"),

    sweet_n_image = list( Description = 'number of sweet image events for given commercial_cond'),
    sweet_n_resp = list( Description = 'Number of sweet events with responses in given block. For a response to counted, the response_time had to be >0ms.'),
    sweet_n_want = list( Description = 'Number of want responses in given sweet block. For a response to counted, the response_time had to be >0ms.'),
    sweet_p_want_of_resp = list( Description = 'proportion of sweet items a child reported wanting out of the total number of sweet responses they provide. For a response to counted, the response_time had to be >0ms.'),
    sweet_avg_rt = list( Description = 'average reaction time for responses to items in sweet food blocks. For a response to counted, the response_time had to be >0ms.',
                         unit = "ms"),
    sweet_med_rt = list( Description = 'median reaction time for responses to all items in sweet food blocks. For a response to counted, the response_time had to be >0ms.',
                       unit = "ms"),

    savory_n_image = list( Description = 'number of savory image events for given commercial_cond'),
    savory_n_resp = list( Description = 'Number of savory events with responses in given block. For a response to counted, the response_time had to be >0ms.'),
    savory_n_want = list( Description = 'Number of want responses in given savory block. For a response to counted, the response_time had to be >0ms.'),
    savory_p_resp = list( Description = 'proportion of savory events with a response out of the total number of savory events. For a response to counted, the response_time had to be >0ms.'),
    savory_p_want_of_resp = list( Description = 'proportion of savory items a child reported wanting out of the total number of savory responses they provide. For a response to counted, the response_time had to be >0ms.'),
    savory_avg_rt = list( Description = 'average reaction time for responses to items in savory food blocks. For a response to counted, the response_time had to be >0ms.',
                          unit = "ms"),
    savory_med_rt = list( Description = 'median reaction time for responses to all items in savory food blocks. For a response to counted, the response_time had to be >0ms.',
                         unit = "ms")
  )


  # convert formatting to JSON
  foodview_bycond_json <- RJSONIO::toJSON(foodview_bycond_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodview_bycond_json, asText = TRUE))){
    print('foodview_bycond_json JSON file may be invalid')
  }



  return(foodview_bycond_json)

}
