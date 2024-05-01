#' json_toolbox_phenotype: Generates a json file for the NIH toolbox behavioral score data (saved in data in bids/phentype)
#'
#' This function generates a json file for cleaned NIH toolbox score data
#'
#' @return A string with data stored in JSON format containing meta-data for NIH toolbox score data
#'
#'
#' @export

json_toolbox_phenotype <- function() {

  toolbox_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = ''),
    session_id = list( Description = ''),
    FLANKER_Test_Ages = list( Description = ''),
    CARDSORT_Test_Ages = list( Description = ''),
    LISTSORT_Test_Ages = list( Description = ''),
    FLANKER_RawScore = list( Description = ''),
    CARDSORT_RawScore = list( Description = ''),
    LISTSORT_RawScore = list( Description = ''),
    # FLANKER_Theta = list( Description = ''),
    # CARDSORT_Theta = list( Description = ''),
    # LISTSORT_Theta = list( Description = ''),
    # FLANKER_TScore = list( Description = ''),
    # CARDSORT_TScore = list( Description = ''),
    # LISTSORT_TScore = list( Description = ''),
    # FLANKER_SE = list( Description = ''),
    # CARDSORT_SE = list( Description = ''),
    # LISTSORT_SE = list( Description = ''),
    FLANKER_ItmCnt = list( Description = ''),
    CARDSORT_ItmCnt = list( Description = ''),
    LISTSORT_ItmCnt = list( Description = ''),
    FLANKER_DateFinished = list( Description = ''),
    CARDSORT_DateFinished = list( Description = ''),
    LISTSORT_DateFinished = list( Description = ''),
    FLANKER_Computed.Score = list( Description = ''),
    CARDSORT_Computed.Score = list( Description = ''),
    LISTSORT_Computed.Score = list( Description = ''),
    FLANKER_Uncorrected.Standard.Score = list( Description = ''),
    CARDSORT_Uncorrected.Standard.Score = list( Description = ''),
    LISTSORT_Uncorrected.Standard.Score = list( Description = ''),
    FLANKER_Age.Corrected.Standard.Score = list( Description = ''),
    CARDSORT_Age.Corrected.Standard.Score = list( Description = ''),
    LISTSORT_Age.Corrected.Standard.Score = list( Description = ''),
    FLANKER_National.Percentile..age.adjusted. = list( Description = ''),
    CARDSORT_National.Percentile..age.adjusted. = list( Description = ''),
    LISTSORT_National.Percentile..age.adjusted. = list( Description = ''),
    FLANKER_Fully.Corrected.T.score = list( Description = ''),
    CARDSORT_Fully.Corrected.T.score = list( Description = ''),
    LISTSORT_Fully.Corrected.T.score = list( Description = '')

  )


  # convert formatting to JSON
  toolbox_json <- RJSONIO::toJSON(toolbox_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(toolbox_json, asText = TRUE))){
    print('NIH toolbox beh JSON file may be invalid')
  }

  return(toolbox_json)

}
