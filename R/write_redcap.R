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
#' @param data_list list of strings matching the notes below to indicate the data to be written. Default = 'all' to export all data and metadata. Options include:
#' \itemize{
#'  \item{'participants' - BIDS specified participants.tsv file}
#'  \item{'anthropometrics' - height, weight, and computed anthropometric data}
#'  \item{'demographics' - compiled demographic data}
#'  \item{'dxa' - verified DXA data}
#'  \item{'household' - compiled demographicinformation about houshold}
#'  \item{'infancy' - compiled demographic information related to infancy}
#'  \item{'intake' - compiled intake data with computed intake values}
#'  \item{'mri_visit' - MRI visit information including Freddy and CAMS}
#'  \item{'parent_updates' - all visit updates}
#'  \item{'researcher_notes' - all visit notes}
#'  \item{'audit' - Alcohol Use Disorders Identification Test}
#'  \item{'bes' - Binge Eating Scale}
#'  \item{'bisbas' - Behavioral Inhibition System/Behavioral Activation System}
#'  \item{'brief2' - Behavioral Rating Inventory of Executive Function-2}
#'  \item{'cbq' - Child Behavior Questionnaire}
#'  \item{'cchip' - Community Childhood Hunger Identification Project}
#'  \item{'cebq' - Children's Eating Behavior Questionnaire}
#'  \item{'cfpq' - Comprehensive Feeding Practices Questionnaire}
#'  \item{'cfq' - Child Feeding Questionnaire}
#'  \item{'chaos' - Confusion, Hubbub, and Order Scale}
#'  \item{'class' - *need*}
#'  \item{'cshq' - Children Sleep Habits Questionnaire}
#'  \item{'debq' - Dutch Eating Behavior Questionnaire}
#'  \item{'efcr' - External Food Cue Responsiveness Scale}
#'  \item{'ffbs' - Family Food Behavior Survey}
#'  \item{'fsq' - *need*}
#'  \item{'hfi' - Fulkerson Home Food Inventory}
#'  \item{'hfias' - Household Food Insecurity Access Scale}
#'  \item{'hfssm' - U.S. Household Food Security Survey Module}
#'  \item{'kbas' - Kid's Brand Awareness Scale}
#'  \item{'lbc' - Lifestyle Behavior Checklist}
#'  \item{'loc' - Loss of Control-Eating Questionnaire}
#'  \item{'pmum' - Problematic Media Use Measure *need*}
#'  \item{'pptq' - Pictorial Personality Traits Questionnaire for Children}
#'  \item{'pss' - Perceived Stress Scale}
#'  \item{'pstca' - *need*}
#'  \item{'puberty' - combination of Tanner and Pubertal Rating Scale}
#'  \item{'pwlb' - Parent Weight-Loss Behavior Questionnaire}
#'  \item{'rank' - Parent ranking of foods sources? *need*}
#'  \item{'scpf' - tructure and Control in Parent Feeding Questionnaire}
#'  \item{'sic' - Stress in Children Questionnaire *need*}
#'  \item{'sleeplog' - Week long sleep log}
#'  \item{'spsrq' - Sensitivity to Punishment and Sensitivity to Reward Questionnaire}
#'  \item{'stq' - Screen Time Questionnaire *need*}
#'  \item{'tfeq' - Three Factor Eating Questionnaire}
#' }
#' @inheritParams write_tasks
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
#'
#' @export

write_redcap <- function(base_wd, overwrite = FALSE, data_list = 'all', return_data = FALSE) {

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

  # Make ID column bids compliant: Convert record_id to strings padded with zeros and add 'sub_'
  redcap_visit_data['record_id'] <- sprintf('sub-%03d', redcap_visit_data[['record_id']])

  # set paths for other directories
  bids_wd <- file.path(base_wd, 'bids')
  phenotype_wd <- file.path(base_wd, 'bids', 'phenotype')

  #### Process REDCap data ####
  proc_redcap_data <- proc_redcap(redcap_api = FALSE, redcap_visit_data, redcap_de_data)

  # quick fixes for notes where /n formatting got saved
  proc_redcap_data$intake$data[grepl('notes', names(proc_redcap_data$intake$data))] <- sapply(names(proc_redcap_data$intake$data)[grepl('notes', names(proc_redcap_data$intake$data))], function(x) gsub('\n', '', proc_redcap_data$intake$data[[x]]))

  proc_redcap_data$mri_visit$data[grepl('notes', names(proc_redcap_data$mri_visit$data))] <- sapply(names(proc_redcap_data$mri_visit$data)[grepl('notes', names(proc_redcap_data$mri_visit$data))], function(x) gsub('\n', '', proc_redcap_data$mri_visit$data[[x]]))

  proc_redcap_data$pstca$data[grepl('response|pstca_29i', names(proc_redcap_data$pstca$data))] <- sapply(names(proc_redcap_data$pstca$data)[grepl('response|pstca_29i', names(proc_redcap_data$pstca$data))], function(x) gsub('\n|- ', ' ', proc_redcap_data$pstca$data[[x]]))

  proc_redcap_data$fsq$data[grepl('resources|fsq_12', names(proc_redcap_data$fsq$data))] <- sapply(names(proc_redcap_data$fsq$data)[grepl('resources|fsq_12', names(proc_redcap_data$fsq$data))], function(x) gsub('\n|- ', ' ', proc_redcap_data$fsq$data[[x]]))

  #### function to export data and metadata ####

  data_list_options <- c('participants', 'anthropometrics', 'demographics', 'dxa', 'household', 'infancy', 'intake', 'mri_visit', 'parent_updates', 'researcher_notes', 'audit', 'bes', 'bisbas', 'brief2', 'cbq', 'cchip', 'cebq', 'cfpq', 'cfq', 'chaos', 'class', 'cshq', 'debq', 'efcr', 'ffbs', 'fsq', 'hfi', 'hfias', 'hfssm', 'kbas', 'lbc', 'loc', 'pmum', 'pptq', 'pss', 'pstca', 'puberty', 'pwlb', 'rank', 'scpf', 'sic', 'sleeplog', 'spsrq', 'stq', 'tfeq')

  if (length(data_list) == 1) {
    if (data_list == 'all'){
      data_list <- data_list_options
    }
  }

  # loop through data_to_export and export data and meta-data
  redcap_export <- function(data_str, overwrite){

    if (data_str %in% data_list_options){

      if (data_str == 'participants'){
        filename_tsv <- file.path(bids_wd, paste0(data_str, '.tsv'))
        filename_json <- file.path(bids_wd, paste0(data_str, '.json'))
      } else {
        filename_tsv <- file.path(phenotype_wd, paste0(data_str, '.tsv'))
        filename_json <- file.path(phenotype_wd, paste0(data_str, '.json'))
      }

      # write tsv
      if ( isTRUE(overwrite) | !file.exists(filename_tsv) ) {
        # use 'n/a' for missing values for BIDS compliance

        write.table(proc_redcap_data[[data_str]]$data, filename_tsv, quote = FALSE, sep = '\t', col.names = TRUE, row.names = FALSE, na = 'n/a')
      }

      # write json
      if ( isTRUE(overwrite) | !file.exists(filename_json) ) {
        write(proc_redcap_data[[data_str]]$meta, filename_json)
      }

    } else {
      print(paste0(data_str, ' is not one of the available data set options to print in write_redcap(). Please see help(write_redcap)'))
    }
  }

  write_redcap_output <- sapply(data_list, function(x) redcap_export(x, overwrite))

  #### Return Data ####
  if (isTRUE(return_data)) {
    return(proc_redcap_data)
  }
}

