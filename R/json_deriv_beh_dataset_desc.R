#' json_deriv_beh_dataset_desc: Generate dataset_description JSON for beh derivatives
#'
#' This function generates a json file for derivatives/dataREACHr
#'
#' @return A string with data stored in JSON format containing meta-data for beh summary datasets derived by dataREACHr
#'
#'
#' @export

json_deriv_beh_dataset_desc <- function() {

  packageVersion("datareachr")

  description_list <- list(
    Name = 'Compiled and summary datasets for behavioral tasks',
    DatasetType = 'Derivative',
    GeneratedBy = list( Name = "dataREACHr",
                        Version = packageVersion("datareachr"),
                        Description = "Datasets generated with package functions: deriv_rrv.R, json_deriv_rrv.R",
                        URL = "https://github.com/bfuchs18/dataREACHr")
    )

  # convert formatting to JSON
  description_json <- RJSONIO::toJSON(description_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(description_json, asText = TRUE))){
    print('JSON file may be invalid')
  }

  return(description_json)

}
