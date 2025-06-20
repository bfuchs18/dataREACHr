#' write_redcap: Write selected data and json files from processed REDCap data
#'
#' This function:
#' \itemize{
#'    \item{1) Calls proc_redcap function to get clean and compiled data and metadata}
#'    \item{2) Exports all or select BIDS-compliant .tsv and .json files into bids/phenotype}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @inheritParams proc_tasks
#' @inheritParams proc_tasks
#' @param export list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data. Options include:
#' \itemize{
#'  \item{paticipants - BIDS specified participants.tsv file}
#'  \item{anthropometrics - height, weight, and computed anthropometric data}
#'  \item{demographics - compiled demographic data}
#'  \item{dxa - verified DXA data}
#'  \item{household - compiled demographicinformation about houshold}
#'  \item{infancy - compiled demographic information related to infancy}
#'  \item{intake - compiled intake data with computed intake values}
#'  \item{mri_visit - MRI visit information including Freddy and CAMS}
#'  \item{researcher_notes - all visit notes}
#'  \item{parent_updates - all visit updates}
#'  \item{audit - Alcohol Use Disorders Identification Test}
#'  \item{bes - Binge Eating Scale}
#'  \item{bisbas - Behavioral Inhibition System/Behavioral Activation System}
#'  \item{brief - Behavioral Rating Inventory of Executive Function-2}
#'  \item{cbq - Child Behavior Questionnaire}
#'  \item{cchip - Community Childhood Hunger Identification Project}
#'  \item{cebq - Children's Eating Behavior Questionnaire}
#'  \item{cfpq - Comprehensive Feeding Practices Questionnaire}
#'  \item{cfq - Child Feeding Questionnaire}
#'  \item{chaos - Confusion, Hubbub, and Order Scale}
#'  \item{class - *need*}
#'  \item{cshq - Children Sleep Habits Questionnaire}
#'  \item{debq - Dutch Eating Behavior Questionnaire}
#'  \item{efcr - External Food Cue Responsiveness Scale}
#'  \item{ffbs - Family Food Behavior Survey}
#'  \item{fsq - *need*}
#'  \item{hfi - Fulkerson Home Food Inventory}
#'  \item{hfias - Household Food Insecurity Access Scale}
#'  \item{hfssm - U.S. Household Food Security Survey Module}
#'  \item{kbas - Kid's Brand Awareness Scale}
#'  \item{lbc - Lifestyle Behavior Checklist}
#'  \item{loc - Loss of Control-Eating Questionnaire}
#'  \item{pmum - Problematic Media Use Measure *need*}
#'  \item{pptq - Pictorial Personality Traits Questionnaire for Children}
#'  \item{pss - Perceived Stress Scale}
#'  \item{pstca - *need*}
#'  \item{puberty - combination of Tanner and Pubertal Rating Scale}
#'  \item{pwlb - Parent Weight-Loss Behavior Questionnaire}
#'  \item{rank - Parent ranking of foods sources? *need*}
#'  \item{scpf - tructure and Control in Parent Feeding Questionnaire}
#'  \item{sic - Stress in Children Questionnaire *need*}
#'  \item{sleeplog - Week long sleep log}
#'  \item{spsrq - Sensitivity to Punishment and Sensitivity to Reward Questionnaire}
#'  \item{stq - Screen Time Questionnaire *need*}
#'  \item{tfeq - Three Factor Eating Questionnaire}
#' }
#'
#' @return Does not return anything
#'
#'
#' @examples
#'
#' \dontrun{
#' write_redcap(base_wd, overwrite = FALSE, data_list = 'all')
#'
#' }
#'
#' @importFrom utils tail write.table read.csv head
#'
#' @export

write_redcap <- function(base_wd, overwrite = FALSE, data_list = 'all') {

  #### Set up/initial checks #####

  # check that base_wd exist and is a string
  data_arg <- methods::hasArg(base_wd)

  if (isTRUE(data_arg)) {
    if (!is.character(base_wd)) {
      stop('base_wd must be entered as a string')
    } else if (!file.exists(base_wd)) {
      stop('base_wd entered, but file does not exist. Check base_wd string.')
    }
  } else if (isFALSE(data_arg)) {
    stop('base_wd must be entered as a string')
  }

  #### Get REDCap Data ####
  # get data from REDCap directly (only will work if have access and keys setup)
  Sys.setenv(reach_redcap_key = keyring::key_get('reach_redcap_key'))
  redcap_visit <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/',
                                        token = Sys.getenv('reach_redcap_key'))


  Sys.setenv(reach_de_redcap_key = keyring::key_get('reach-de_redcap_key'))
  redcap_de <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/',
                                     token = Sys.getenv('reach_de_redcap_key'))

  redcap_visit_data <- redcap_visit[['data']]
  redcap_visit_dict <- redcap_visit[['dictionary']]

  redcap_de_data <- redcap_de[['data']]
  redcap_de_dict <- redcap_de[['dictionary']]

  # remove '.factor'
  redcap_visit_data <- redcap_visit_data[, !grepl('.factor', names(redcap_visit_data))]
  redcap_de_data <- redcap_de_data[, !grepl('.factor', names(redcap_de_data))]

  # set paths for other directories
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')

  #### Process REDCap data ####
  proc_redcap_data <- proc_redcap(redcap_api = FALSE, redcap_visit_data, redcap_de_data)

  #### function to export data and metadata ####
  # loop through data_to_export and export data and meta-data
  for (i in 1:length(data_to_export)) {

    # Get the dataframe
    df <- data_to_export[[i]]

    # get the phenotype name
    phenotype_name <- names(data_to_export)[i]

    # define json function name
    json_func_name = paste0('json_', phenotype_name)

    # Get the json function by name
    json_func <- get(json_func_name)

    # Call the function
    json <- json_func()

    # add extensions to phenotype_name
    if (phenotype_name == 'participants') {
      # export participants into bids_wd
      filename_tsv <- file.path(bids_wd, 'participants.tsv')
      filename_json <- file.path(bids_wd, 'participants.json')
    } else {
      # export phenotype data into phenotype_wd
      filename_tsv <- file.path(phenotype_wd, paste0(phenotype_name, '.tsv'))
      filename_json <- file.path(phenotype_wd, paste0(phenotype_name, '.json'))
    }

    # write tsv
    if ( isTRUE(overwrite) | !file.exists(filename_tsv) ) {
      write.table(
        df,
        filename_tsv,
        quote = FALSE,
        sep = '\t',
        col.names = TRUE,
        row.names = FALSE,
        na = 'n/a' # use 'n/a' for missing values for BIDS compliance
      )
    }

    # write json
    if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
      write(json, filename_json)
    }
  }


  # export dataset_description.json
  filename_json <- file.path(phenotype_wd, 'dataset_description.json')
  json <- json_phe_dataset_desc(visit_data_path, data_de_path)
  if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
    write(json, filename_json)
  }

  #### Return Data ####
  if (isTRUE(return_data)) {
    return(list(
      input_data = list(visit_data = redcap_visit_data,
                        de_data = redcap_de_data),
      visit_data = list(
        child_v1_data = child_v1_data,
        child_v2_data = child_v2_data,
        child_v3_data = child_v3_data,
        child_v4_data = child_v4_data,
        child_v5_data = child_v5_data,
        parent_v1_data = parent_v1_data,
        parent_v2_data = parent_v2_data,
        parent_v3_data = parent_v3_data,
        parent_v4_data = parent_v4_data,
        parent_v5_data = parent_v5_data
      ),
      double_entry_data = processed_de_data,
      phenotype_data = data_to_export
    ))
  }
}

