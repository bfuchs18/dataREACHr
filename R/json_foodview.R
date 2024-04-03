#' json_foodview: Generates a json file for the food view fmri events (data in rawdata/func)
#'
#' This function generates a json file for cleaned food view task data collected during fmri (onsets, responses)
#'
#' @return A json file documenting the XXX
#'
#'
#' @export

json_foodview <- function() {

  foodview_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    onset = list( Description = '',
                  Unit = seconds),
    duration = list( Description = 'stimulus duration. Calculated by subtracting the onset for the subsequent stimulus from the onset of the present stimulus',
                     Derivative = TRUE,
                     Unit = seconds),
    sub = list( Description = ''),
    run = list( Description = ''),
    commercial_cond = list( Description = 'commercial condition',
                            Levels = list ('food' = 'food',
                                           'toy' = 'toy')),
    stim_file = list( Description = ''),
    response = list( Description = 'response',
                     Levels = list ('0' = 'no response',
                                    '1' = '',
                                    '2' = '')),
    response_time = list( Description = 'response time',
                          Unit = "seconds"),
    food_ed = list( Description = 'food image energy density category',
                 Levels = list ('low' = 'low energy density',
                                'high' = 'high energy density')),
    food_taste = list( Description = 'food image taste category',
                  Levels = list ('sweet' = 'sweet',
                                 'savory' = 'savory')),
    response_time = list( Description = 'response time',
                          Unit = "seconds")
  )


  # convert formatting to JSON
  foodview_json <- RJSONIO::toJSON(foodview_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(foodview_json, asText = TRUE))){
    print('food view events JSON file may be invalid')
  }

  return(foodview_json)

}
