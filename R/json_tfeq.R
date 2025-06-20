#' json_tfeq: Generates a json file for the Three-Factor Eating Questionnaire-R18
#'
#' This function generates a json file for the scored Three-Factor Eating Questionnaire-R18 and raw participant responses.
#'
#' @return A string with data stored in JSON format containing meta-data for the Three-Factor Eating Questionnaire-R18
#'
#'
#' @export

json_tfeq <- function() {

  tfeq_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Three-Factor Eating Questionnaire-R18. Participants were provided the following instructions: "For each item below, indicate how "true" or "false" the statement is by using the options below: Definitely False - Mostly False - Mostly True - Definitely True"',
      Reference = 'Karlsson J, Persson LO, Sjostrom L, Sullivan M. Psychometric properties and factor structure of the Three-Factor Eating Questionnaire (TFEQ) in obese men and women. Results from the Swedish Obese Subjects (SOS) study. Int J Obes Relat Metab Disord. 2000 Dec;24(12):1715-25. doi: 10.1038/sj.ijo.0801442. PMID: 11126230.',
      TermURL = 'https://pubmed.ncbi.nlm.nih.gov/11126230/'),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    visit_date = list( Description = 'Date of visit this parent-reported survey was completed',
                       Unit = 'YYYY-MM-DD'),
    tfeq1 = list( Description = 'When I smell a sizzling steak or juice piece of meat, I find it very difficult to keep from eating, even if I have just finished a meal.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq2 = list( Description = 'I deliberately take small helpings as a means of controlling my weight.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq3 = list( Description = 'When I feel anxious, I find myself eating.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq4 = list( Description = 'Sometimes when I start eating, I just can\'t seem to stop.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq5 = list( Description = 'Being with someone who is eating often makes me hungry enough to eat also.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq6 = list( Description = 'When I feel blue, I often overeat.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq7 = list( Description = 'When I see a real delicacy, I often get so hungry that I have to eat right away.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq8 = list( Description = 'I get so hungry that my stomach often seems like a bottomless pit.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq9 = list( Description = 'I am always hungry so it is hard for me to stop eating before I finish the food on my plate.',
                  Levels = list ('0' = 'Definitely false',
                                 '1' = 'Mostly false',
                                 '2' = 'Mostly true',
                                 '3' = 'Definitely true')),
    tfeq10 = list( Description = 'When I feel lonely, I console myself by eating.',
                   Levels = list ('0' = 'Definitely false',
                                  '1' = 'Mostly false',
                                  '2' = 'Mostly true',
                                  '3' = 'Definitely true')),
    tfeq11 = list( Description = 'I consciously hold back at meals in order to not gain weight.',
                   Levels = list ('0' = 'Definitely false',
                                  '1' = 'Mostly false',
                                  '2' = 'Mostly true',
                                  '3' = 'Definitely true')),
    tfeq12 = list( Description = 'I do not eat some foods because they make me fat.',
                   Levels = list ('0' = 'Definitely false',
                                  '1' = 'Mostly false',
                                  '2' = 'Mostly true',
                                  '3' = 'Definitely true')),
    tfeq13 = list( Description = 'I am always hungry enough to eat at any time.',
                   Levels = list ('0' = 'Definitely false',
                                  '1' = 'Mostly false',
                                  '2' = 'Mostly true',
                                  '3' = 'Definitely true')),
    tfeq14 = list( Description = 'How often do you feel hungry?',
                   Levels = list ('0' = 'Only at meal times',
                                  '1' = 'Sometimes between meals',
                                  '2' = 'Often between meals',
                                  '3' = 'Almost always')),
    tfeq15 = list( Description = 'How frequently do you avoid "stocking up" on tempting foods? ',
                   Levels = list ('0' = 'Almost never',
                                  '1' = 'Seldom',
                                  '2' = 'Usually',
                                  '3' = 'Almost always')),
    tfeq16 = list( Description = 'How likely are you to consciously eat less than you want?',
                   Levels = list ('0' = 'Unlikely',
                                  '1' = 'Slighly likely',
                                  '2' = 'Moderately likely',
                                  '3' = 'Very likely')),
    tfeq17 = list( Description = 'Do you go on eating binges though you are not hungry?',
                   Levels = list ('0' = 'Never',
                                  '1' = 'Rarely',
                                  '2' = 'Sometimes',
                                  '3' = 'At least once a week')),
    tfeq18 = list( Description = 'On a scale of of 1 to 8, where 1 means no restraint in eating (eating whatever you want, whenever you want it) and 8 means total restraint (constantly limiting food intake and never "giving in"), what number would you give yourself?',
                   Levels = list ('0' = '1',
                                  '1' = '2',
                                  '2' = '3',
                                  '3' = '4',
                                  '4' = '5',
                                  '5' = '6',
                                  '6' = '7',
                                  '7' = '8')),
    tfeq18_cr = list( Description = 'Cognitive restraint',
                    Derivative = TRUE),
    tfeq18_ue = list( Description = 'Uncontrolled eating',
                     Derivative = TRUE),
    tfeq18_ee = list( Description = 'Emotional eating',
                    Derivative = TRUE))

  # convert formatting to JSON
  tfeq_json <- RJSONIO::toJSON(tfeq_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(tfeq_json, asText = TRUE))){
    print('tfeq JSON file may be invalid')
  }

  return(tfeq_json)

}
