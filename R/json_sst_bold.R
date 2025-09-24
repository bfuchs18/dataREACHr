#' json_sst_bold: Generates a json file for the SST fmri events (data in rawdata/func)
#'
#' This function generates a json file for cleaned stop signal data collected during fmri (onsets, responses)
#'
#' @return A string with data stored in JSON format containing meta-data for the XXX
#'
#'
#' @export

json_sst_bold <- function() {

  sst_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Adapted Food Related Stop-Signal Task',
      Reference = 'Pearce, Alaina L., Kyle Hallisky, Barbara J. Rolls, Stephen J. Wilson, Emma Rose, Charles F. Geier, Hugh Garavan, and Kathleen L. Keller. "Children at high familial risk for obesity show executive functioning deficits prior to development of excess weight status." Obesity 31, no. 12 (2023): 2998-3007. https://doi.org/10.1002/oby.23892',
      TermURL = 'https://pmc.ncbi.nlm.nih.gov/articles/PMC10884994/'),
    onset = list( Description = 'stimuli onset time',
                  Units = 'sec'),
    duration = list( Description = 'stimuli duration',
                     Units = 'sec'),
    parparticipant_id = list( Description = 'Participant ID'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    run_num = list( Description = 'SST run number'),
    set = list( Description = 'run stimuli set (A-F)'),
    run_cond = list( Description = 'run condition, based on the type of commercials shown during the run',
                     Levels = list ('food' = 'food',
                                    'toy' = 'toy')),
    stim_file_name = list( Description = 'basename of stimulus file'),
    trial_num = list( Description = 'trial number'),
    type = list( Description = 'task type',
                 Levels = list ('Prac' = 'pratice task',
                                'Beh' = 'behavioral task',
                                'fMRI' = 'fmri task')),
    block = list( Description = 'block number within a run'),
    img_cat = list( Description = 'food image category',
                    Levels = list ('sweet' = 'sweet',
                                   'savory' = 'savory')),
    go_stim = list( Description = 'go signal (napkin) side',
                    Levels = list ('1' = 'left side',
                                   '2' = 'right side')),
    signal = list( Description = '',
                   Levels = list ('0' = 'no stop signal given (go trial)',
                                  '1' = 'stop signal given (stop trial)')),
    reqSSD = list( Description = 'requested stop signal delay'),
    correct = list( Description = '',
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
    trueSSD = list( Description = 'true stop signal delay (signal onset - go onset)')
  )


  # convert formatting to JSON
  sst_json <- RJSONIO::toJSON(sst_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(sst_json, asText = TRUE))){
    print('sst events JSON file may be invalid')
  }

  return(sst_json)

}
