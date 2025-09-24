#' json_sst_byblock: Generates json file for the derivative SST behavioral data by run and block
#'
#' This function generates a json file for SST summary data in long format by run and block
#'
#' @return a json file for SST summary data in long format by run and block
#'
#'
#' @export

json_sst_byblock <- function() {

  sst_byblock_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Adapted Food Related Stop-Signal Task',
      Reference = 'Pearce, Alaina L., Kyle Hallisky, Barbara J. Rolls, Stephen J. Wilson, Emma Rose, Charles F. Geier, Hugh Garavan, and Kathleen L. Keller. Children at high familial risk for obesity show executive functioning deficits prior to development of excess weight status. Obesity 31, no. 12 (2023): 2998-3007. https://doi.org/10.1002/oby.23892',
      TermURL = 'https://pmc.ncbi.nlm.nih.gov/articles/PMC10884994/'),
    'FileLevelMetadata' = list(
      Description = 'SST behavior summarized by run and block',
      Sources = list( 'bids::rawdata/sub*/ses-1/func/sub*sst*events.tsv', 'bids::rawdata/sub*/ses-1/beh/sub*sst*events.tsv'),
      DatasetType = 'derivative'),
    participant_id = list( Description = 'Participant ID'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),
    trial_type = list(Description = 'task type for each trial',
                      Levels = list ('beh' = 'behavioral task',
                                     'fmri' = 'fmri task')),
    run = list( Description = 'SST run number'),
    ad_cond = list( Description = 'advertisement condition, based on the type of commercials shown during the run',
                     Levels = list ('food' = 'food',
                                    'toy' = 'toy')),
    block = list( Description = 'SST block number within a run'),
    img_cat = list( Description = 'food image category',
                    Levels = list ('sweet' = 'sweet',
                                   'savory' = 'savory')),
    racehorse_check = list(Description = 'check of the racehorse assumption for the Stop-Signal Task: mean go reaction time (go_rt_mean) > mean reaction time during unsuccessful stops (us_rt_mean)',
                           Levels = list ('1' = 'met assumption',
                                          '0' = 'failed to meet assumption')),
    n_stop_trials = list(Description = 'number of stop trials'),
    n_go_trials = list(Description = 'number of go trials'),
    go_rt_mean = list(Description = 'average reaction time on go trials with responses',
                      Unit = 'ms'),
    n_go_cor = list(Description = 'number of correct go (left/right) responses'),
    go_correct_rt_mean = list(Description = 'average reaction time for correct go responses',
                              Unit = 'ms'),
    n_go_error = list(Description = 'number of go trial errors (left/right)'),
    go_error_rt_mean = list(Description = 'average reaction time on go trials with errors',
                            Unit = 'ms'),
    n_go_miss = list( Description = 'number of missed go trial responses'),
    prop_stop_fail = list( Description = 'proportion of of failed stop trials'),
    us_rt_mean = list( Description = 'average reaction time on unsucessful stop trials ',
                       Unit = 'ms'),
    ssd_mean = list(Description = 'average stop-signal delay',
                    Unit = 'ms'),
    ssrt_mean = list( Description = 'stop-signal reaction time calculated using the mean method (mean go rt - mean ssd). Only calcualted if racehorse assumption is met',
                       Unit = 'ms'),
    ssrt_int = list( Description = 'stop-signal reaction time calculated using the integration method (nth rt - mean ssd). Only calcualted if racehorse assumption is met',
                      Unit = 'ms')
  )

  # convert and return JSONS ----
  sst_byblock_json <- RJSONIO::toJSON(sst_byblock_list, pretty = TRUE)


  if (isFALSE(RJSONIO::isValidJSON(sst_byblock_json, asText = TRUE))){
    print('sst_byblock_json JSON file may be invalid')
  }

  return(sst_byblock_json)

}
