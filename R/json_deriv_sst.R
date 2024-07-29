#' json_deriv_sst: Generates json file for the derivative SST behavioral data
#'
#' This function generates 2 json files for SST summary data reported in derivatives/beh_databases: (1) summary metrics computed by ad condition, (2) summary metrics computed by block
#'
#' @return A list of 2 strings (labels: XX, sst_byblock_json) with data stored in JSON format containing meta-data the summary SST database
#'
#'
#' @export

json_deriv_sst <- function() {

  # JSON for dataframe by block ----

  sst_byblock_list <- list(
    'FileLevelMetadata' = list(
      Description = "SST behavior summarized by block",
      Sources = "bids::rawdata/sub*/ses-1/func/sub*sst*events.tsv"),
    sub = list( Description = 'participant id number'),
    run_num = list( Description = 'SST run number (1-6)'),
    block_num = list( Description = 'Block number within a run (1-2)'),
    commercial_cond = list( Description = 'commercial condition',
                            Levels = list ('food' = 'food',
                                           'toy' = 'toy')),
    img_cat = list( Description = 'block taste category',
                       Levels = list ('sweet' = 'sweet foods',
                                      'savory' = 'savory foods')),
    racehorse_check = list( Description = 'check of the racehorse assumption for the Stop-Signal Task (mean go reaction time > mean reaction time during unsuccessful stops)',
                            Levels = list ('1' = 'met assumption',
                                           '0' = 'failed to meet assumption'),
                            Derivative = TRUE),
    n_stop_trials = list( Description = 'number of stop trials',
                          Derivative = TRUE),
    n_go_trials = list( Description = 'number of go trials',
                        Derivative = TRUE),
    go_rt = list( Description = 'average reaction time on go trials with responses',
                  Unit = "",
                  Derivative = TRUE),
    n_go_cor = list( Description = 'number of correct go (left/right) responses',
                     Derivative = TRUE),
    go_cor_rt = list( Description = 'average reaction time for correct go responses',
                   Unit = "",
                   Derivative = TRUE),
    n_go_error = list( Description = 'number of go trial errors (left/right)',
                      Derivative = TRUE),
    go_error_rt = list( Description = 'average reaction time on go trials with errors',
                       Unit = "",
                       Derivative = TRUE),
    n_go_miss = list( Description = 'number of missed go trial responses',
                       Derivative = TRUE),
    stop_prob_resp = list( Description = 'probability of responding on stop trials; p(resp|stop signal)',
                       Derivative = TRUE),
    us_rt = list( Description = 'average reaction time on unsucessful stop trials ',
                       Unit = "",
                       Derivative = TRUE),
    ssd = list( Description = 'average stop-signal delay',
                       Unit = "",
                       Derivative = TRUE),
    ssrt_mean = list( Description = 'stop-signal reaction time calculated using the mean method (mean go rt - mean ssd). Only calcualted if racehorse assumption is met',
                       Unit = "",
                       Derivative = TRUE),
    ssrt_int = list( Description = 'stop-signal reaction time calculated using the integration method (nth rt - mean ssd). Only calcualted if racehorse assumption is met',
                      Unit = "",
                      Derivative = TRUE)
  )

  # convert and return JSONS ----

  # convert formatting to JSON
  # foodview_bycond_json <- RJSONIO::toJSON(foodview_bycond_list, pretty = TRUE)
  sst_byblock_json <- RJSONIO::toJSON(sst_byblock_list, pretty = TRUE)

  # double check
  # if (isFALSE(RJSONIO::isValidJSON(foodview_bycond_json, asText = TRUE))){
  #   print('foodview_bycond_json JSON file may be invalid')
  # }

  if (isFALSE(RJSONIO::isValidJSON(sst_byblock_json, asText = TRUE))){
    print('sst_byblock_json JSON file may be invalid')
  }

  return(list(
    #foodview_bycond_json = foodview_bycond_json,
              sst_byblock_json = sst_byblock_json))

}
