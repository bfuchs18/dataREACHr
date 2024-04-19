#' util_format_household_data: process household data (called within util_redcap_parent_v1 and util_redcap_parent_v5)
#'
#' This function process household data
#'
#'
#' @param household_data Household extracted from data from REDCap events util_redcap_parent_v1 and util_redcap_parent_v5
#'
util_format_household_data <- function(household_data) {


  # relocate household_data columns
  household_data <- household_data %>% dplyr::relocate("session_id", .after = 1) %>% dplyr::relocate("household_form_date", .after = 2) # relocate columns

  # rename columns
  names(household_data)[names(household_data) == "demo_self_report_feet"] <- "demo_parent2_reported_height_ft_component"
  names(household_data)[names(household_data) == "demo_self_report_inches"] <- "demo_parent2_reported_height_inch_component"
  names(household_data)[names(household_data) == "demo_self_report_weight"] <- "demo_parent2_reported_weight_lbs"

  # calculate parent age
  household_data[['household_form_date']] <- lubridate::as_date(household_data[['household_form_date']])
  household_data[['demo_parent_birthdate']] <- lubridate::as_date(household_data[['demo_parent_birthdate']])
  household_data[['demo_parent_age']] <- round(lubridate::interval(household_data[['demo_parent_birthdate']], household_data[['household_form_date']])/lubridate::years(1), 1)

  # remove birthdate and timestamp variables
  household_data <- household_data[, -grep("birthdate|timestamp", names(household_data))]

  # combine parent2 feet and inch components into 1 height variable in meters
  household_data$parent2_reported_height_m <- ((household_data$demo_parent2_reported_height_ft_component*12) + household_data$demo_parent2_reported_height_inch_component)*0.0254

  # calculate parent2 BMI (kg/m2)
  household_data$parent2_reported_bmi <- (household_data$demo_parent2_reported_weight_lbs*0.453592) / (household_data$parent2_reported_height_m**2)

  # return data
  return(household_data)

}
