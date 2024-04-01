#' util_calc_intake: Computes intake variables (called within util_redcap_de.R)
#'
#' This function computes intake variables
#'
#'
#' @param stacked_intake Intake data (i.e., pre and post weights) extracted double entry data
#' @return A dataframe of stacked_intake (input) variables and calculated intake variables

util_calc_intake <- function(stacked_intake) {

  #make dataframe with energy density data
  ed_data <- util_gen_ed_data() #make dataframe with energy density data

  #### calculate grilled cheese energy density ####

  # calculate energy content of individual grilled cheese components
  stacked_intake$butter_pre_kcal <- (stacked_intake$butter_pre_w_o_plate + stacked_intake$butter_2_pre_w_o_plate)*(ed_data[ed_data$food == "butter", "ed"])
  stacked_intake$bread_pre_kcal <- (stacked_intake$bread_pre_w_o_plate)*(ed_data[ed_data$food == "bread", "ed"])
  stacked_intake$cheese_pre_kcal <- (stacked_intake$cheese_pre_w_o_plate)*(ed_data[ed_data$food == "cheese", "ed"])

  # calculate grilled cheese pre_kcal
  stacked_intake$grilled_cheese_pre_kcal <- (stacked_intake$butter_pre_kcal + stacked_intake$bread_pre_kcal + stacked_intake$cheese_pre_kcal)

  # calculate grilled cheese ED
  stacked_intake$grilled_cheese_ed <- stacked_intake$grilled_cheese_pre_kcal/stacked_intake$grilled_cheese_pre_w_o_plate

  #### calculate grams consumed ####
  # loop through foods in ed_data$food

  ## grilled cheese
  stacked_intake$grilled_cheese_kcal_consumed <- NA
  #  stacked_intake$grilled_cheese_kcal_consumed <- (stacked_intake$grilled_cheese_amount_consumed)*(stacked_intake$grilled_cheese_ed)

  # calculate kcal_consumed for other items? or extract from redcap? -- yes recalculate

  # return data
  return(stacked_intake)

}
