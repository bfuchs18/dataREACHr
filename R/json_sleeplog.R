#' json_sleeplog: Generates a json file for the Sleep Log
#'
#' This function generates a json file for the Sleep Log
#'
#' @return A string with data stored in JSON format containing meta-data for the Sleep Log
#'
#'
#' @export

json_sleeplog <- function() {

  sleeplog_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    date_night1 = list( Description = 'Night 1 date in the morning (YYYY-MM-DD)'),
    date_night2 = list( Description = 'Night 2 date in the morning(YYYY-MM-DD)'),
    date_night3 = list( Description = 'Night 3 date in the morning(YYYY-MM-DD)'),
    date_night4 = list( Description = 'Night 4 date in the morning(YYYY-MM-DD)'),
    date_night5 = list( Description = 'Night 5 date in the morning(YYYY-MM-DD)'),
    date_night6 = list( Description = 'Night 6 date in the morning(YYYY-MM-DD)'),
    date_night7 = list( Description = 'Night 7 date in the morning (YYYY-MM-DD)'),
    bedtime_night1 = list( Description = 'Q1. What time did you go to bed?: Night 1'),
    bedtime_night2 = list( Description = 'Q1. What time did you go to bed?: Night 2'),
    bedtime_night3 = list( Description = 'Q1. What time did you go to bed?: Night 3'),
    bedtime_night4 = list( Description = 'Q1. What time did you go to bed?: Night 4'),
    bedtime_night5 = list( Description = 'Q1. What time did you go to bed?: Night 5'),
    bedtime_night6 = list( Description = 'Q1. What time did you go to bed?: Night 6'),
    bedtime_night7 = list( Description = 'Q1. What time did you go to bed?: Night 7'),
    attempt_night1 = list( Description = 'Q2. What time did you try to go to sleep: Night 1'),
    attempt_night2 = list( Description = 'Q2. What time did you try to go to sleep: Night 2'),
    attempt_night3 = list( Description = 'Q2. What time did you try to go to sleep: Night 3'),
    attempt_night4 = list( Description = 'Q2. What time did you try to go to sleep: Night 4'),
    attempt_night5 = list( Description = 'Q2. What time did you try to go to sleep: Night 5'),
    attempt_night6 = list( Description = 'Q2. What time did you try to go to sleep: Night 6'),
    attempt_night7 = list( Description = 'Q2. What time did you try to go to sleep: Night 7'),
    asleep_night1 = list( Description = 'Q3. What time did you fall asleep: Night 1'),
    asleep_night2 = list( Description = 'Q3. What time did you fall asleep: Night 2'),
    asleep_night3 = list( Description = 'Q3. What time did you fall asleep: Night 3'),
    asleep_night4 = list( Description = 'Q3. What time did you fall asleep: Night 4'),
    asleep_night5 = list( Description = 'Q3. What time did you fall asleep: Night 5'),
    asleep_night6 = list( Description = 'Q3. What time did you fall asleep: Night 6'),
    asleep_night7 = list( Description = 'Q3. What time did you fall asleep: Night 7'),
    times_night1 = list( Description = 'Q4. How many times did you wake up during the night?: Night 1'),
    times_night2 = list( Description = 'Q4. How many times did you wake up during the night?: Night 2'),
    times_night3 = list( Description = 'Q4. How many times did you wake up during the night?: Night 3'),
    times_night4 = list( Description = 'Q4. How many times did you wake up during the night?: Night 4'),
    times_night5 = list( Description = 'Q4. How many times did you wake up during the night?: Night 5'),
    times_night6 = list( Description = 'Q4. How many times did you wake up during the night?: Night 6'),
    times_night7 = list( Description = 'Q4. How many times did you wake up during the night?: Night 7'),
    waso_night1 = list( Description = 'Q5. In total, how long did those awakenings last?: Night 1'),
    waso_night2 = list( Description = 'Q5. In total, how long did those awakenings last?: Night 2'),
    waso_night3 = list( Description = 'Q5. In total, how long did those awakenings last?: Night 3'),
    waso_night4 = list( Description = 'Q5. In total, how long did those awakenings last?: Night 4'),
    waso_night5 = list( Description = 'Q5. In total, how long did those awakenings last?: Night 5'),
    waso_night6 = list( Description = 'Q5. In total, how long did those awakenings last?: Night 6'),
    waso_night7 = list( Description = 'Q5. In total, how long did those awakenings last?: Night 7'),
    awake_night1 = list( Description = 'Q6. What time did you wake up from sleep?: Night 1'),
    awake_night2 = list( Description = 'Q6. What time did you wake up from sleep?: Night 2'),
    awake_night3 = list( Description = 'Q6. What time did you wake up from sleep?: Night 3'),
    awake_night4 = list( Description = 'Q6. What time did you wake up from sleep?: Night 4'),
    awake_night5 = list( Description = 'Q6. What time did you wake up from sleep?: Night 5'),
    awake_night6 = list( Description = 'Q6. What time did you wake up from sleep?: Night 6'),
    awake_night7 = list( Description = 'Q6. What time did you wake up from sleep?: Night 7'),
    out_on_night1 = list( Description = 'Q7. What time did you get out of bed to start your day?: Night 1'),
    out_on_night2 = list( Description = 'Q7. What time did you get out of bed to start your day?: Night 2'),
    out_on_night3 = list( Description = 'Q7. What time did you get out of bed to start your day?: Night 3'),
    out_on_night4 = list( Description = 'Q7. What time did you get out of bed to start your day?: Night 4'),
    out_on_night5 = list( Description = 'Q7. What time did you get out of bed to start your day?: Night 5'),
    out_on_night6 = list( Description = 'Q7. What time did you get out of bed to start your day?: Night 6'),
    out_on_night7 = list( Description = 'Q7. What time did you get out of bed to start your day?: Night 7'),
    rating_night1 = list( Description = 'Q8. How well did you sleep?: Night 1'),
    rating_night2 = list( Description = 'Q8. How well did you sleep?: Night 2'),
    rating_night3 = list( Description = 'Q8. How well did you sleep?: Night 3'),
    rating_night4 = list( Description = 'Q8. How well did you sleep?: Night 4'),
    rating_night5 = list( Description = 'Q8. How well did you sleep?: Night 5'),
    rating_night6 = list( Description = 'Q8. How well did you sleep?: Night 6'),
    rating_night7 = list( Description = 'Q8. How well did you sleep?: Night 7'),
    comment_night1 = list( Description = 'Q9. Comments: Night 1'),
    comment_night2 = list( Description = 'Q9. Comments: Night 2'),
    comment_night3 = list( Description = 'Q9. Comments: Night 3'),
    comment_night4 = list( Description = 'Q9. Comments: Night 4'),
    comment_night5 = list( Description = 'Q9. Comments: Night 5'),
    comment_night6 = list( Description = 'Q9. Comments: Night 6'),
    comment_night7 = list( Description = 'Q9. Comments: Night 7')
    )

  # convert formatting to JSON
  sleeplog_json <- RJSONIO::toJSON(sleeplog_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(sleeplog_json, asText = TRUE))){
    print('sleeplog JSON file may be invalid')
  }

  return(sleeplog_json)

}
