#' json_sleeplog: Generates a json file for the External Food Cue Responsiveness Scale
#'
#' This function generates a json file for the scored External Food Cue Responsiveness Scale and raw participant responses.
#' This function provides accurate json files ONLY if data is processed using score_sleeplog function in dataprepr and is only accurate for data collected in Study REACH
#'
#' @return A string with data stored in JSON format containing meta-data for the External Food Cue Responsiveness Scale
#'
#'
#' @export

json_sleeplog <- function() {

  sleeplog_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'External Food Cue Responsiveness Scale. Participants were provided the following instructions: "Please answer the following statements with "My Child..... Never, Rarely, Sometimes , Often or Always""',
      Reference = 'Masterson TD, Gilbert-Diamond D, Lansigan RK, Kim SJ, Schiffelbein JE, Emond JA. Measurement of external food cue responsiveness in preschool-age children: Preliminary evidence for the use of the external food cue responsiveness scale. Appetite. 2019;139:119-126. doi:10.1016/j.appet.2019.04.024',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/31047939/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),

    date_mon = list( Description = ''),
    date_tu = list( Description = ''),
    date_wed = list( Description = ''),
    date_th = list( Description = ''),
    date_fri = list( Description = ''),
    date_sat = list( Description = ''),
    date_sun = list( Description = ''),
    bedtime_mon = list( Description = 'Q1. What time did you go to bed?: '),
    bedtime_tu = list( Description = 'Q1. What time did you go to bed?: '),
    bedtime_wed = list( Description = 'Q1. What time did you go to bed?: '),
    bedtime_th = list( Description = 'Q1. What time did you go to bed?: '),
    bedtime_fri = list( Description = 'Q1. What time did you go to bed?: '),
    bedtime_sat = list( Description = 'Q1. What time did you go to bed?: '),
    bedtime_sun = list( Description = 'Q1. What time did you go to bed?: '),
    asleep_mon = list( Description = 'Q2. What time did you try to go to sleep: '),
    asleep_tu = list( Description = 'Q2. What time did you try to go to sleep:'),
    asleep_wed = list( Description = 'Q2. What time did you try to go to sleep: '),
    asleep_th = list( Description = 'Q2. What time did you try to go to sleep: '),
    asleep_fri = list( Description = 'Q2. What time did you try to go to sleep: '),
    asleep_sat = list( Description = 'Q2. What time did you try to go to sleep: '),
    asleep_sun = list( Description = 'Q2. What time did you try to go to sleep: '),
    times_mon = list( Description = ''),
    times_mon = list( Description = ''),
    times_tu = list( Description = ''),
    times_wed = list( Description = ''),
    times_th = list( Description = ''),
    times_fri = list( Description = ''),
    times_sat = list( Description = ''),
    times_sun = list( Description = ''),
    waso_mon = list( Description = ''),
    waso_tu = list( Description = ''),
    waso_wed = list( Description = ''),
    waso_th = list( Description = ''),
    waso_fri = list( Description = ''),
    waso_sat = list( Description = ''),
    waso_sun = list( Description = ''),
    awake_mon = list( Description = ''),
    awake_tu = list( Description = ''),
    awake_wed = list( Description = ''),
    awake_th = list( Description = ''),
    awake_fri = list( Description = ''),
    awake_sat = list( Description = ''),
    awake_sun = list( Description = ''),
    out_on_mon = list( Description = ''),
    out_on_tu = list( Description = ''),
    out_on_wed = list( Description = ''),
    out_on_th = list( Description = ''),
    out_on_fri = list( Description = ''),
    out_on_sat = list( Description = ''),
    out_on_sun = list( Description = ''),
    rating_mon = list( Description = ''),
    rating_tu = list( Description = ''),
    rating_wed = list( Description = ''),
    rating_th = list( Description = ''),
    rating_fri = list( Description = ''),
    rating_sat = list( Description = ''),
    rating_sun = list( Description = ''),
    comment_mon = list( Description = ''),
    comment_tu = list( Description = ''),
    comment_wed = list( Description = ''),
    comment_th = list( Description = ''),
    comment_fri = list( Description = ''),
    comment_sat = list( Description = ''),
    comment_sun = list( Description = '')
    )

  # convert formatting to JSON
  sleeplog_json <- RJSONIO::toJSON(sleeplog_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(sleeplog_json, asText = TRUE))){
    print('sleeplog JSON file may be invalid')
  }

  return(sleeplog_json)

}
