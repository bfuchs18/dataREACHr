#' json_sst_prescan: Generates a json file for the SST pre-scan behaviorald data (data in rawdata/beh)
#'
#' This function generates a json file for cleaned stop signal data collected during practice and behavioral task runs
#'
#' @return A json file documenting the XXX
#'
#'
#' @export

json_sst_prescan <- function() {

  sst_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Adapted Food Related Stop-Signal Task',
      Reference = 'Pearce, Alaina L., Kyle Hallisky, Barbara J. Rolls, Stephen J. Wilson, Emma Rose, Charles F. Geier, Hugh Garavan, and Kathleen L. Keller. "Children at high familial risk for obesity show executive functioning deficits prior to development of excess weight status." Obesity 31, no. 12 (2023): 2998-3007. https://doi.org/10.1002/oby.23892',
      TermURL = 'https://pmc.ncbi.nlm.nih.gov/articles/PMC10884994/'),
    participant_id = list( Description = 'Participant ID'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    type = list( Description = 'task type',
                 Levels = list ('Prac' = 'pratice task',
                                'Beh' = 'behavioral task',
                                'fMRI' = 'fmri task')),
    run_num = list( Description = 'behavioral SST run number (1-2)'),
    set = list( Description = 'set of stimuli used in run'),
    run_cond = list( Description = 'run condition, based on the type of commercials shown during the run',
                     Levels = list ('food' = 'food',
                                    'toy' = 'toy')),
    block = list( Description = 'block number within a run',
                  Levels = list ('1' = 'first block',
                                 '2' = 'second block')),
    stim_file_name = list( Description = 'basename of stimulus file'),
    img_cat = list( Description = 'food image category',
                    Levels = list ('sweet' = 'sweet',
                                   'savory' = 'savory')),
    go_stim = list( Description = 'go signal (napkin) side',
                    Levels = list ('1' = 'left side',
                                   '2' = 'right side')),
    signal = list( Description = 'Signal type on trial',
                   Levels = list ('0' = 'no stop signal given (go trial)',
                                  '1' = 'stop signal given (stop trial)')),
    reqSSD = list( Description = 'requested stop signal delay'),
    correct = list( Description = 'Response type',
                    Levels = list ('1' = 'missed go response',
                                   '2' = 'incorrect go response (go_stim != response)',
                                   '3' = 'failed stop trial',
                                   '4' = 'correct response (successful stop or correct go response)'),
                    Derivative = TRUE),
    response = list( Description = 'response',
                     Levels = list ('0' = 'no response',
                                    '1' = 'left side',
                                    '2' = 'right side')),
    rt = list( Description = 'response time',
                          Unit = "ms"),
    trueSSD = list( Description = 'true stop signal delay (signal onset - go onset)'))


  # convert formatting to JSON
  sst_json <- RJSONIO::toJSON(sst_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(sst_json, asText = TRUE))){
    print('sst pre-scan beh JSON file may be invalid')
  }

  return(sst_json)

}
