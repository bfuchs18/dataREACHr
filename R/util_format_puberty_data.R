#' util_format_puberty_data: Prepare parent-reported puberty data for scoring (called within util_redcap_parent_v1.R and util_redcap_parent_v5.R)
#'
#' This function prepares parent-reported puberty data for scoring with dataprepr::score_puberty()
#'
#'
#' @param puberty_data Puberty data (parental_rating_scale_for_pubertal_development, Tanner) extracted from data from REDCap events parent_visit_1_arm_1 and parent_visit_5_arm_1'
#' @importFrom rlang .data
util_format_puberty_data <- function(puberty_data) {

  names(puberty_data) <- gsub('prs_sex', 'sex', names(puberty_data))

  # scoring prep: update puberty_data to adhere to dataprepr::score_pds requirements:
  # The data set columns/variables must match the following naming convention: 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa'.
  # Values should be: 1 = not started yet, 2 = barely started, 3 = definitely started, 4 = seems complete, and 99 = I Don't Know. For Female ('pds_5fa') question on menarche, the response can be coded as Yes = 1, No = 0.

  # remap scale values except prs_girls_5
  value_mapping <- c("0" = 1, "1" = 2, "2" = 3, "3" = 4, "4" = 99)
  columns_to_remap <- c("prs_boys_1", "prs_boys_2", "prs_boys_3", "prs_boys_4", "prs_boys_5", "prs_boys_6", "prs_girls_1", "prs_girls_2", "prs_girls_3", "prs_girls_4", "prs_girls_6")
  puberty_data[, columns_to_remap] <- lapply(puberty_data[, columns_to_remap], function(x) value_mapping[as.character(x)])

  # remap prs_girls_5
  value_mapping_g5 <- c("4" = 99)
  puberty_data[, "prs_girls_5"] <- ifelse(puberty_data[, "prs_girls_5"] == 4, value_mapping_g5[as.character(4)], puberty_data[, "prs_girls_5"])

  # subset and rename girls variables
  puberty_data_girls <- subset(puberty_data, puberty_data$sex == 0)
  puberty_data_girls <- puberty_data_girls[, -grep("boys|tanner_male_choice", names(puberty_data_girls))]
  puberty_data_girls <- puberty_data_girls %>% dplyr::rename(
    pds_1 = .data$prs_girls_1,
    pds_2 = .data$prs_girls_2,
    pds_3 = .data$prs_girls_3,
    pds_4f = .data$prs_girls_4,
    pds_5fa = .data$prs_girls_5,
    pds_6 = .data$prs_girls_6,
    tanner_choice = .data$tanner_female_choice
  )

  # subset and rename boys variables
  puberty_data_boys <- subset(puberty_data, puberty_data$sex == 1)
  puberty_data_boys <- puberty_data_boys[, -grep("girls|tanner_female_choice", names(puberty_data_boys))]
  puberty_data_boys <- puberty_data_boys %>% dplyr::rename(
    pds_1 = .data$prs_boys_1,
    pds_2 = .data$prs_boys_2,
    pds_3 = .data$prs_boys_3,
    pds_4m = .data$prs_boys_4,
    pds_5m = .data$prs_boys_5,
    pds_6 = .data$prs_boys_6,
    tanner_choice = .data$tanner_male_choice
  )
  # bind girls and boys dfs -- dplyr::bind_rows fills missing values with NA where columns don't match.
  puberty_data_for_scoring <- dplyr::bind_rows(puberty_data_girls, puberty_data_boys)

  # return data
  return(puberty_data_for_scoring)

}
