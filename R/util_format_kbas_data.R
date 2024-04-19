#' util_format_kbas_data: process kbas data (called within util_redcap_child_v1 and util_redcap_child_v5)
#'
#' This function process kbas data
#'
#'
#' @param kbas_data kbas extracted from data from REDCap events util_redcap_child_v1 and util_redcap_child_v5
#'
util_format_kbas_data <- function(kbas_data) {

  # relabel version a and b items
  names(kbas_data)[3:105] <- paste0("va_", names(kbas_data)[3:105]) # add va_ to begining of column names
  names(kbas_data)[107:208] <- paste0("vb_", names(kbas_data)[107:208]) # add vb_ to begining of column names
  names(kbas_data) <- gsub('\\_b$|\\_vb$', '', names(kbas_data)) # remove _b and _vb from end of column names

  # add version column
  ## define version items
  va_items <- colnames(kbas_data %>% dplyr::select(dplyr::starts_with("va")) %>% dplyr::select(-dplyr::contains("score")))
  vb_items <- colnames(kbas_data %>% dplyr::select(dplyr::starts_with("vb")) %>% dplyr::select(-dplyr::contains("score")))

  ## assign version -- requires having responses to a given version & no responses to the other version
  kbas_data$kbas_version <- ifelse(rowSums(!is.na(kbas_data[va_items])) > 45 & rowSums(is.na(kbas_data[vb_items])) == 50, "A",
                                   ifelse(rowSums(is.na(kbas_data[va_items])) > 45 & rowSums(!is.na(kbas_data[vb_items])) == 50, "B", NA))

  # add form date column -- using dplyr::if_else here because it preserves the type/class of inputs (i.e, dates)
  kbas_data$kbas_form_date <-
    dplyr::if_else(kbas_data$kids_brand_awareness_survey_version_a_timestamp != "",
                   lubridate::as_date(kbas_data$kids_brand_awareness_survey_version_a_timestamp),
                   lubridate::as_date(kbas_data$kids_brand_awareness_survey_version_b_timestamp)
    )

  # combine va and vb total scored columns
  kbas_data$kbas_food_score <- ifelse(kbas_data$kbas_version == "A", kbas_data$va_food_score_kbas,
                                      ifelse(kbas_data$kbas_version == "B", kbas_data$vb_food_score_kbas, NA))

  kbas_data$kbas_toy_score <- ifelse(kbas_data$kbas_version == "A", kbas_data$va_toy_score_kbas,
                                     ifelse(kbas_data$kbas_version == "B", kbas_data$vb_toy_score_kbas, NA))

  #remove columns
  kbas_data <- kbas_data[, !grepl('timestamp|va_toy_score|vb_toy_score|va_food_score|vb_food_score', names(kbas_data))]

  #reorder columns
  kbas_data <-
    kbas_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate(dplyr::contains("form_date"), .after = 2) %>% dplyr::relocate(dplyr::contains("kbas_version"), .after = 3) %>% dplyr::relocate(dplyr::contains("kbas_food_score"), .after = 4)  %>% dplyr::relocate(dplyr::contains("kbas_toy_score"), .after = 5)


  # return data
  return(kbas_data)

}
