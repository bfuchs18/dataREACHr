#' util_format_cshq_data: process cshq data (called within util_redcap_parent_v2 and util_redcap_parent_v5)
#'
#' This function prepares cshq data for scoring with dataprepr::score_cshq by re-leveling scored items to have numeric values 1-3: (3 - Usually, 2 - Sometimes, 1 - Rarely or 1 - not sleepy, 2 - very sleepy, 3 - falls asleep)
#'
#'
#' @param cshq_data cshq extracted from data from REDCap events util_redcap_parent_v2 and util_redcap_parent_v5
#'
util_format_cshq_data <- function(cshq_data) {

  #### Rename columns ####

  # rename columns for items asking if a behavior is a problem
  names(cshq_data) <- gsub('_a', '_prob', names(cshq_data))

  # rename columns with _component
  names(cshq_data)[names(cshq_data) == "cshq_sleep_total_hours"] <- "cshq_night_sleep_amount_hrs_component"
  names(cshq_data)[names(cshq_data) == "cshq_sleep_total_mins"] <- "cshq_night_sleep_amount_min_component"
  names(cshq_data)[names(cshq_data) == "cshq_nap_hours"] <- "cshq_nap_amount_hrs_component"
  names(cshq_data)[names(cshq_data) == "cshq_nap_mins"] <- "cshq_nap_amount_min_component"

  #### Compute total sleep amounts from hr and min components ####

  # if min component is NA, calculate with hrs component only, else, calculate with min and hrs components
  cshq_data$cshq_night_sleep_amount_total_mins <- ifelse(is.na(cshq_data$cshq_night_sleep_amount_min_component), cshq_data$cshq_night_sleep_amount_hrs_component* 60, (cshq_data$cshq_night_sleep_amount_hrs_component* 60 + cshq_data$cshq_night_sleep_amount_min_component))
  cshq_data$cshq_nap_amount_total_mins <- ifelse(is.na(cshq_data$cshq_nap_amount_min_component), cshq_data$cshq_nap_amount_hrs_component* 60, (cshq_data$cshq_nap_amount_hrs_component* 60 + cshq_data$cshq_nap_amount_min_component))

  #### Update Usually/Sometimes/Rarely ('usr') columns ####

  # specify 'usr' columns to update
  all_nums <- seq(1, 33)
  usr_nums <- all_nums[all_nums != 7 & all_nums != 8]
  usr_cols <- paste0("cshq_", usr_nums)

  # update values to be (3 - Usually, 2 - Sometimes, 1 - Rarely) rather than (0 - Usually, 1 - Sometimes, 2 - Rarely)
  for (col_name in usr_cols) {
    # Replace values in the specified columns
    cshq_data[[col_name]] <- ifelse(cshq_data[[col_name]] == 0, 3, ifelse(cshq_data[[col_name]] == 1, 2, ifelse(cshq_data[[col_name]] == 2 ,1 , NA)))
  }

  #### Update Not Sleepy/Sleepy/Falls Asleep ('nsf') columns ####

  # specify 'nsf' columns to update
  nsf_cols <- c("cshq_7", "cshq_8")

  # update values to be (1 - Not Sleepy, 2 - Sleepy, 3 - Falls Asleep) rather than (0 - Not Sleepy, 1 - Sleepy, 2 - Falls Asleep)
  for (col_name in nsf_cols) {
    # Replace values in the specified columns
    cshq_data[[col_name]] <- ifelse(cshq_data[[col_name]] == 0, 1, ifelse(cshq_data[[col_name]] == 1, 2, ifelse(cshq_data[[col_name]] == 2,3 , NA)))
  }


  # return data
  return(cshq_data)

}
