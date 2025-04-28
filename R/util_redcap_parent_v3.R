#' util_redcap_parent_v3: Organize parent visit 3 data from REDCap
#'
#' This function organizes REDCap data from REDCap visit data, event parent_visit_3_arm_1
#'
#'
#' @param data data from REDCap event parent_visit_3_arm_1'
#' @inheritParams util_redcap_parent_v1
#'
#' @return Will return a list including:
#' \itemize{
#'  \item{clean raw and scored parent visit 1 datasets}
#'  \item{meta-data formated as json for each dataset}
#'  }
#'
#'  Returned data includes:
#'  \itemize{
#'    \item{visit_data_parent}
#'    \item{spsrq_scored}
#'    \item{pwlb_scored}
#'    \item{tfeq_scored}
#'    \item{class_data}
#'    \item{bisbas_scored}
#'    \item{pstca_data}
#'    \item{debq_scored}
#'    \item{scpf_scored}
#'  }
#' @examples
#'
#' # process REDCap data
#' parent_visit3_list <- util_redcap_parent_v3(data, date_data)
#'
#' \dontrun{
#' }
#'
#' @seealso [proc_redcap()]




util_redcap_parent_v3 <- function(data, date_data) {

  #### 1. Set up/initial checks #####

  # check that audit_data exist and is a data.frame
  data_arg <- methods::hasArg(data)

  if (isTRUE(data_arg)) {
    if (!is.data.frame(data)) {
      stop("data must be a data.frame")
    }
  } else if (isFALSE(data_arg)) {
  }

  # update name of participant ID column
  names(data)[names(data) == "record_id"] <- "participant_id"

  # add session column
  data['session_id'] <- 'ses-1'

  # merge with date data for V1
  data <- merge(data, date_data[c('participant_id', 'v1_date')], by = 'participant_id', all.x = TRUE)
  names(data)[names(data) == 'v1_date'] <- 'visit_date'
  data['visit_date'] <- lubridate::as_date(data[['visit_date']])


  #reduce columns and update names

  ## Update form Data ####
  visit_data_parent <- data[, grep("_id|update|visit_date", names(data))]

  # remove extra columns and re-order
  visit_data_parent <- visit_data_parent[, -grep("contact|moving", names(visit_data_parent))]
  visit_data_parent <- visit_data_parent[c('participant_id', 'session_id', 'visit_date', names(visit_data_parent)[grepl('update', names(visit_data_parent))])]

  ## SPSRQ Data ####
  spsrq_data <- data[, grepl('_id|spsrq|visit_date', names(data))]

  # remove extra columns and re-order
  spsrq_data <- spsrq_data[c('participant_id', 'session_id', 'visit_date', names(spsrq_data)[grepl('spsrq', names(spsrq_data))])]

  spsrq_scored <- dataprepr::score_spsrq(spsrq_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("visit_date"))

  ## PWLB Data ####
  pwlb_data <- data[, grepl('_id|pwlb|visit_date', names(data))]

  # remove extra columns and re-order
  pwlb_data <- pwlb_data[, -grep("missingcheck|timestamp", names(pwlb_data))]
  pwlb_data <- pwlb_data[c('participant_id', 'session_id', 'visit_date', names(pwlb_data)[grepl('pwlb', names(pwlb_data))])]

  pwlb_scored <- dataprepr::score_pwlb(pwlb_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("visit_date", "pwlb_24a"))

  ## TFEQ Data ####
  #note: REACH used tfeq-r18 (revised scale)
  tfeq_data <- data[, grepl('_id|tfeq|visit_date', names(data))]

  # remove extra columns and re-order
  tfeq_data <- tfeq_data[c('participant_id', 'session_id', 'visit_date', names(tfeq_data)[grepl('tfeq', names(tfeq_data))])]

  tfeq_scored <- dataprepr::score_tfeq18(tfeq_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("visit_date"))

  ## CLASS Data ####
  class_data <- data[, grepl('_id|class|visit_date', names(data))]

  # remove extra columns and re-order
  class_data <- class_data[, -grep("missingcheck|timestamp", names(class_data))] # remove extra columns
  class_data <- class_data[c('participant_id', 'session_id', 'visit_date', names(class_data)[grepl('class', names(class_data))])]

  # score? -- need to develop score script

  ## BISBAS Data ####
  bisbas_data <- data[, grepl('_id|bisbas|visit_date', names(data))]

  # remove extra columns and re-order
  bisbas_data <- bisbas_data[c('participant_id', 'session_id', 'visit_date', names(bisbas_data)[grepl('bisbas', names(bisbas_data))])]

  bisbas_scored <- dataprepr::score_bisbas(bisbas_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("visit_date"))

  ## pstca Data ####
  pstca_data <- data[, grepl('_id|pstca|ptsca|visit_date', names(data))]
  colnames(pstca_data) <- gsub("ptsca", "pstca", colnames(pstca_data)) # fix scale abbreviation in column names
  pstca_data$pstca_form_date <- lubridate::as_date(pstca_data$parental_strategies_to_teach_children_about_advert_timestamp) # add form date column
  pstca_data <- pstca_data[, -grep("missingcheck|timestamp", names(pstca_data))] # remove extra columns
  pstca_data <- pstca_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  # score -- need to develop score script

  ## DEBQ Data ####
  debq_data <- data[, grepl('_id|debq|dutch_eating_behavior_questionnaire_timestamp', names(data))]
  debq_data$debq_form_date <- lubridate::as_date(debq_data$dutch_eating_behavior_questionnaire_timestamp) # add form date column
  debq_data <- debq_data[, -grep("missingcheck|timestamp", names(debq_data))] # remove extra columns
  debq_data <- debq_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  debq_data$debq_20 <- NA # mark item 20 data as missing. debq question 20 was not administered in REACH -- debq question 23 was repeated in its place
  debq_scored <- dataprepr::score_debq(debq_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("debq_form_date"))

  ## SCPF Data ####
  scpf_data <- data[, grepl('_id|scpf|structure_and_control_in_parent_feeding_timestamp', names(data))]
  scpf_data$scpf_form_date <- lubridate::as_date(scpf_data$structure_and_control_in_parent_feeding_timestamp) # add form date column
  scpf_data <- scpf_data[, -grep("missingcheck|timestamp", names(scpf_data))] # remove extra columns
  scpf_data <- scpf_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) # relocate columns

  scpf_data$scpf_17 <- NA # mark item 17 data as missing -- the wrong question was administered here.
  scpf_scored <- dataprepr::score_scpf(scpf_data, base_zero = TRUE, id = 'participant_id', extra_scale_cols = c("scpf_form_date"))

  ## return data ####
  return(list(
    visit_data_parent = visit_data_parent,
    spsrq_data = spsrq_scored,
    pwlb_data = pwlb_scored,
    tfeq_data = tfeq_scored,
    class_data = class_data,
    bisbas_data = bisbas_scored,
    pstca_data = pstca_data,
    #      pstca_data = pstca_scored,
    debq_data = debq_scored,
    scpf_data = scpf_scored
  ))

}

