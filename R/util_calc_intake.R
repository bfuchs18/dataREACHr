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

  #### calculate item amounts consumed ####
  foods <- ed_data$food[4:20]

  for (food in foods) {
    pre_var <- paste(food, "_pre_w_plate", sep = "")
    post_var <- paste(food, "_post_w_plate", sep = "")
    consumed_g_var <- paste(food, "_grams_consumed", sep = "")
    consumed_kcal_var <- paste(food, "_kcal_consumed", sep = "")

    # calculate grams consumed
    stacked_intake[[consumed_g_var]] <- stacked_intake[[pre_var]] - stacked_intake[[post_var]]

    # calculate kcal consumed
    if (food == "grilled_cheese") {

      # calculate kcal using EDs calculated per person
      stacked_intake[[consumed_kcal_var]] <- stacked_intake[[consumed_g_var]] * stacked_intake$grilled_cheese_ed

    } else {

      # calculate kcal using EDs in ed_data
      stacked_intake[[consumed_kcal_var]] <- stacked_intake[[consumed_g_var]] * ed_data[ed_data$food == food, "ed"]

    }
  }

  #### calculate total amounts consumed ####
  # note: by using na.rm = FALSE -- total amounts will only be calculated if there is data for all food items to be summed

  ## meal

  # make lists of meal item consumption variables
  meal_g_vars <- paste0(foods[1:7], "_grams_consumed")
  meal_kcal_vars <- paste0(foods[1:7], "_kcal_consumed")

  # sum across meal_g_vars columns
  stacked_intake$meal_grams_consumed <- stacked_intake %>%
    dplyr::select(all_of(meal_g_vars)) %>%
    rowSums(na.rm = FALSE)

  # sum across meal_kcal_vars columns
  stacked_intake$meal_kcal_consumed <- stacked_intake %>%
    dplyr::select(all_of(meal_kcal_vars)) %>%
    rowSums(na.rm = FALSE)

  ## EAH

  # make lists of eah item consumption variables
  eah_g_vars <- paste0(foods[8:17], "_grams_consumed")
  eah_kcal_vars <- paste0(foods[8:17], "_kcal_consumed")

  # sum across eah_g_vars columns
  stacked_intake$eah_grams_consumed <- stacked_intake %>%
    dplyr::select(all_of(eah_g_vars)) %>%
    rowSums(na.rm = FALSE)

  # sum across eah_kcal_vars columns
  stacked_intake$eah_kcal_consumed <- stacked_intake %>%
    dplyr::select(all_of(eah_kcal_vars)) %>%
    rowSums(na.rm = FALSE)

  ## total (meal + eah)

  # sum across meal and eah grams_consumed
  stacked_intake$total_grams_consumed <- stacked_intake %>%
    dplyr::select(c("meal_grams_consumed", "eah_grams_consumed")) %>%
    rowSums(na.rm = FALSE)

  # sum across meal and eah kcal_consumed
  stacked_intake$total_kcal_consumed <- stacked_intake %>%
    dplyr::select(c("meal_kcal_consumed", "eah_kcal_consumed")) %>%
    rowSums(na.rm = FALSE)

  # return data
  return(stacked_intake)

}
