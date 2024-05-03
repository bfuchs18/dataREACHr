#' util_format_cshq_data: process cshq data (called within util_redcap_parent_v2 and util_redcap_parent_v5)
#'
#' This function process cshq data
#'
#'
#' @param cshq_data cshq extracted from data from REDCap events util_redcap_parent_v2 and util_redcap_parent_v5
#'
util_format_cshq_data <- function(cshq_data) {

  # rename columns for items asking if a behavior is a problem
  names(cshq_data) <- gsub('_a', '_prob', names(cshq_data))

  # update values to be (3 - Usually, 2 - Sometimes, 1 - Rarely) rather than (0 - Usually, 1 - Sometimes, 2 - Rarely)

  ## make vector of columns to update
  q_numbers <- seq(1, 33)
  cshq_cols_to_update <- paste0("cshq_", q_numbers)

  ## update values
  for (col_name in cshq_cols_to_update) {
    # Replace values in the specified columns
    cshq_data[[col_name]] <- ifelse(cshq_data[[col_name]] == 0, 3, ifelse(cshq_data[[col_name]] == 1, 2, ifelse(cshq_data[[col_name]] == 2 ,1 , NA)))
  }

  # return data
  return(cshq_data)

}
