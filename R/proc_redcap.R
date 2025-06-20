#' proc_redcap: Process raw data downloaded from Study REACH REDCap
#'
#' This function:
#' \itemize{
#'    \item{1) Reads REDCap data (visit and double-entry) using the REDCap API}
#'    \item{2) Calls util_ functions to clean and compile data in dataframes}
#'    \item{3) Calls json_ functions to create strings with meta-data stored in JSON format for each dataframe}
#'    \item{4) Compiles data repeated across visits and sessions}
#'}
#'
#' To use this function, the correct path must be used. The path must be the full path to the data file, including the file name.
#'
#' @param redcap_api (logical) execute REDCap API. Default = FALSE.
#' @param redcap_visit_data REDCap visit data from a prior API call
#' @param redcap_de_data REDCap double-entry data from a prior API call
#'
#' @return Will return a list including:
#' \itemize{
#'    \item{participants: list including data.frame with BIDS participants.tsv information and metadata}
#'    \item{questionnaires: a list of 37 individual questionnaire lists including a data.frame and metadata}
#'    \item{demographics}
#'    \item{anthropometrics}
#'    \item{intake}
#'    \item{mri_visit}
#'    \item{dexa}
#'    \item{researcher_notes}
#'    \item{parent_updates}
#'  }
#'
#' @examples
#'
#' \dontrun{
#' redcap_data <- proc_redcap(base_wd, overwrite = FALSE, overwrite_jsons = FALSE)
#'
#' }
#'
#' @export

proc_redcap <- function(redcap_api = FALSE, redcap_visit_data, redcap_de_data) {

  #### Set up/initial checks #####

  # check that data is passed if redcap_api = FALSE
  if (isFALSE(redcap_api)){

    # check that redcap_visit_data exist and is a data.frame
    visit_data_arg <- methods::hasArg(redcap_visit_data)

    if (isTRUE(visit_data_arg)) {
      if (!is.data.frame(redcap_visit_data)) {
        stop('redcap_visit_data must be a data.frame with recap_api = FALSE')
      }
    } else if (isFALSE(visit_data_arg)) {
      stop('redcap_visit_data must be a data.frame with recap_api = FALSE')
    }

    # check that redcap_de_data exist and is a data.frame
    de_data_arg <- methods::hasArg(redcap_de_data)

    if (isTRUE(de_data_arg)) {
      if (!is.data.frame(redcap_de_data)) {
        stop('redcap_de_data must be a data.frame with recap_api = FALSE')
      }
    } else if (isFALSE(de_data_arg)) {
      stop('redcap_de_data must be a data.frame with recap_api = FALSE')
    }

  } else {
    # get data from REDCap directly (only will work if have access and keys setup)
    Sys.setenv(reach_redcap_key = keyring::key_get('reach_redcap_key'))
    redcap_visit <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/',
                                          token = Sys.getenv('reach_redcap_key'))


    Sys.setenv(reach_de_redcap_key = keyring::key_get('reach-de_redcap_key'))
    redcap_de <- REDCapDM::redcap_data(uri = 'https://redcap.ctsi.psu.edu/api/',
                                       token = Sys.getenv('reach_de_redcap_key'))

    redcap_visit_data <- redcap_visit[['data']]
    redcap_visit_dict <- redcap_visit[['dictionary']]

    # remove '.factor'
    redcap_visit_data <- redcap_visit_data[, !grepl('.factor', names(redcap_visit_data))]
  }

  #### Extract visit data ####

  # Make ID column bids compliant: Convert record_id to strings padded with zeros and add 'sub_'
  redcap_visit_data['record_id'] <- sprintf('sub-%03d', redcap_visit_data[['record_id']])

  # # subset events and remove unnecessary columns
  redcap_long_wide <- function(event_name, data){

    #subset
    sub_dat <- data[data[['redcap_event_name']] == event_name, ]

    #remove empty columns
    sub_dat <- sub_dat[, !colSums(is.na(sub_dat)) == nrow(sub_dat)]

    #return
    return(sub_dat)
  }

  # Extract visit data
  child_visit_1_arm_1 <- redcap_long_wide('child_visit_1_arm_1', redcap_visit_data)
  parent_visit_1_arm_1 <- redcap_long_wide('parent_visit_1_arm_1', redcap_visit_data)
  child_visit_2_arm_1 <- redcap_long_wide('child_visit_2_arm_1', redcap_visit_data)
  parent_visit_2_arm_1 <- redcap_long_wide('parent_visit_2_arm_1', redcap_visit_data)
  child_visit_3_arm_1 <- redcap_long_wide('child_visit_3_arm_1', redcap_visit_data)
  parent_visit_3_arm_1 <- redcap_long_wide('parent_visit_3_arm_1', redcap_visit_data)
  child_visit_4_arm_1 <- redcap_long_wide('child_visit_4_arm_1', redcap_visit_data)
  parent_visit_4_arm_1 <- redcap_long_wide('parent_visit_4_arm_1', redcap_visit_data)
  child_visit_5_arm_1 <- redcap_long_wide('child_visit_5_arm_1', redcap_visit_data)
  parent_visit_5_arm_1 <- redcap_long_wide('parent_visit_5_arm_1', redcap_visit_data)

  #### Process visit data ####

  # make data.frame of dates, ages, and sex
  date_data <- util_redcap_dates(child_v1 = child_visit_1_arm_1, child_v2 = child_visit_2_arm_1, child_v3 = child_visit_3_arm_1, child_v4 = child_visit_4_arm_1, child_v5 = child_visit_5_arm_1, parent_v1 = parent_visit_1_arm_1)

  # visit survey data
  child_v1_data <- util_redcap_child_v1(child_visit_1_arm_1)
  parent_v1_data <- util_redcap_parent_v1(parent_visit_1_arm_1, date_data)
  child_v2_data <- util_redcap_child_v2(child_visit_2_arm_1)
  parent_v2_data <- util_redcap_parent_v2(parent_visit_2_arm_1, date_data)
  child_v3_data <- util_redcap_child_v3(child_visit_3_arm_1)
  parent_v3_data <- util_redcap_parent_v3(parent_visit_3_arm_1, date_data)
  child_v4_data <- util_redcap_child_v4(child_visit_4_arm_1)
  parent_v4_data <- util_redcap_parent_v4(parent_visit_4_arm_1, date_data)
  child_v5_data <- util_redcap_child_v5(child_visit_5_arm_1)
  parent_v5_data <- util_redcap_parent_v5(parent_visit_5_arm_1, date_data)


  #### Process double-entry data ####
  #processed_de_data <- util_redcap_de(redcap_de_data)

  #### Combine data across visits ####
  ## questionnaires
  merged_qs_list <- util_merge_questionnaires(child_v1_data, child_v4_data, child_v5_data, parent_v1_data, parent_v2_data, parent_v3_data, parent_v4_data, parent_v5_data)

  #updates json
  updates_json <- json_parent_updates()

  ## Merge intake-related data ---- will need to adujst once have double-entered data
  # merge intake-related data (paradigm info, liking data, wanting data, intake data, fullness data)
  # intake data from visit forms will output until double entry data is ready
  merged_intake <- util_merged_intake(child_v1_data, child_v3_data, child_v4_data, child_v5_data)

  intake_merge_json <- json_intake()

  ## Merge notes into notes database
  researcher_notes <- util_merged_visitnotes(child_v1_data$visit_data_child, child_v2_data$visit_data_child, child_v3_data$visit_data_child, child_v4_data$visit_data_child, child_v5_data$visit_data_child)

  notes_json <- json_researcher_notes()

  ## Anthro data
  merged_anthro <- util_merged_anthro(child_v1_data$anthro_data$data, child_v5_data$anthro_data$data,  merged_qs_list$household_all, date_data)

  anthro_merge_json <- json_anthropometrics()

  #### Generate demographics dataframe  ####
  merged_demo <- util_merged_demo(parent_v1_data$demo_data$data, merged_qs_list$household_all, merged_anthro, date_data)

  demographics_json <- json_demographics()

  #### Generate participants dataframe ####
  participants_data <- util_merged_participants(parent_v1_data$demo_data$data, merged_demo, date_data)

  participants_json <- json_participants()

  #### Data to return ####

  # list dataframes to return, where the name is the corresponding json function without 'json_'
  return(list(
    participants = list(data = participants_data, meta = participants_json),
    anthropometrics = list(data = merged_anthro, meta = anthro_merge_json),
    demographics = list(data = merged_demo, meta = demographics_json),
    #dxa = processed_de_data$dxa_data,
    household = list(data = merged_qs_list$household_all,
                     meta = parent_v1_data$household_data$meta),
    infancy = parent_v1_data[['infancy_data']],
    intake = list(data = merged_intake, meta = intake_merge_json),
    mri_visit = child_v2_data$mri_info,
    researcher_notes = list(data = researcher_notes, meta = notes_json),
    parent_updates = list(data = merged_qs_list$updates_all, meta = updates_json),
    audit = list(data = merged_qs_list$audit_all,
                 meta = parent_v4_data$audit_data$meta),
    bes = parent_v2_data[['bes_data']],
    bisbas = parent_v3_data[['bisbas_data']],
    breif = parent_v2_data[['brief_data']],
    cbq = list(data = merged_qs_list$cbq_all,
               meta = parent_v1_data$stq_data$meta),
    cchip = parent_v4_data[['cchip_data']],
    cebq = list(data = merged_qs_list$cebq_all,
                meta = parent_v1_data$cebq_data$meta),
    cfpq = list(data = merged_qs_list$cfpq_all,
                     meta = parent_v4_data$cfpq_data$meta),
    cfq = parent_v1_data[['cfq_data']],
    chaos = parent_v1_data[['chaos_data']],
    class = list(data = merged_qs_list$class_all,
                      meta = parent_v3_data$class_data$meta),
    cshq = list(data = merged_qs_list$cshq_all,
                meta = parent_v2_data$cshq_data$meta),
    debq = parent_v3_data[['debq_data']],
    efcr = parent_v1_data[['efcr_data']],
    ffbs = parent_v2_data[['ffbs_data']],
    fsq = parent_v2_data[['fsq_data']],
    hfi = parent_v4_data[['hfi_data']],
    hfias = parent_v4_data[['hfias_data']],
    hfssm = parent_v4_data[['hfssm_data']],
    kbas = list(data = merged_qs_list$kbas_all,
                meta = child_v1_data$kbas_data$meta),
    lbc = parent_v1_data[['lbc_data']],
    loc = list(data = merged_qs_list$loc_all,
                    meta = child_v4_data$loc_data$meta),
    pmum = list(data = merged_qs_list$pmum_all,
                     meta = parent_v4_data$pmum_data$meta),
    pptq = child_v4_data[['pptq_data']],
    pss = parent_v1_data[['pss_data']],
    pstca = list(data = merged_qs_list$pstca_all,
                 meta = parent_v3_data$pstca_data$meta),
    puberty = list(data = merged_qs_list$puberty_all,
                        meta = parent_v1_data$puberty_data$meta),
    pwlb = parent_v3_data[['pwlb_data']],
    rank = list(data = merged_qs_list$rank_all,
                     meta = parent_v1_data$rank_data$meta),
    scpf = parent_v3_data[['scpf_data']],
    sic = child_v4_data[['sic_data']],
    sleeplog = child_v3_data[['sleeplog_data']],
    spsrq = parent_v3_data[['spsrq_data']],
    stq = list(data = merged_qs_list$stq_all,
               meta = child_v1_data$stq_data$meta),
    tfeq = parent_v3_data[['tfeq_data']]
  ))
}

