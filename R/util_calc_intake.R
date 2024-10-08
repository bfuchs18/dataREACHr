#' util_calc_intake: Computes intake variables (called within util_redcap_de.R)
#'
#' This function computes intake variables
#'
#'
#' @param stacked_intake Intake data (i.e., pre and post weights) extracted double entry data
#' @return A dataframe of stacked_intake (input) variables and calculated intake variables

util_calc_intake <- function(stacked_intake) {

  # convert all intake columns to numeric -- anything entered as a string will be recoded as NA by coercian
  intake_cols <- names(stacked_intake[, !names(stacked_intake) %in% c("participant_id", "session_id", "visit_protocol")])

  classes <- sapply(stacked_intake[intake_cols], class)
  for (i in 1:length(classes)) {
    if (classes[i] != "numeric") {
      print(paste(names(classes[i]), "contains non-numeric values. These will recoded as NA by coercion"))
    }
  }

  stacked_intake[intake_cols] <- sapply(stacked_intake[intake_cols],as.numeric)

  # make dataframe with energy density data
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

  #### TO DO ####
  # deal with potential negative values (e.g., post amount was more than pre amount )

  #### calculate item amounts consumed ####
  foods <- ed_data$food[4:21]

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


  # define vectors of meal foods and items (foods + water)
  meal_foods <- c("grilled_cheese", "tender", "carrot", "chips", "fruit", "ranch", "ketchup")
  meal_items <- c(meal_foods, "water")

  # define names for consumption variables
  meal_foods_g_vars <- paste0(meal_foods, "_grams_consumed")
  meal_items_g_vars <- paste0(meal_items, "_grams_consumed")
  meal_kcal_vars <- paste0(meal_foods, "_kcal_consumed")

  # sum across meal_foods_g_vars columns
  stacked_intake$meal_grams_consumed <- stacked_intake %>%
    dplyr::select(dplyr::all_of(meal_foods_g_vars)) %>%
    rowSums(na.rm = FALSE)

  # sum across meal_items_g_vars columns
  stacked_intake$meal_grams_consumed_inc_water <- stacked_intake %>%
    dplyr::select(dplyr::all_of(meal_items_g_vars)) %>%
    rowSums(na.rm = FALSE)

  # sum across meal_kcal_vars columns
  stacked_intake$meal_kcal_consumed <- stacked_intake %>%
    dplyr::select(dplyr::all_of(meal_kcal_vars)) %>%
    rowSums(na.rm = FALSE)

  ## EAH

  # define vectors of eah foods and items (foods + water)
  eah_foods <- c("brownie", "corn_chip", "kiss", "ice_cream", "oreo", "popcorn", "pretzel", "skittle", "starburst")
  eah_items <- c(eah_foods, "water_eah")

  # make lists of eah item consumption variables
  eah_foods_g_vars <- paste0(eah_foods, "_grams_consumed")
  eah_items_g_vars <- paste0(eah_items, "_grams_consumed")
  eah_kcal_vars <- paste0(eah_foods, "_kcal_consumed")

  # sum across meal_foods_g_vars columns
  stacked_intake$eah_grams_consumed <- stacked_intake %>%
    dplyr::select(dplyr::all_of(eah_foods_g_vars)) %>%
    rowSums(na.rm = FALSE)

  # sum across eah_items_g_vars columns
  stacked_intake$eah_grams_consumed_inc_water <- stacked_intake %>%
    dplyr::select(dplyr::all_of(eah_items_g_vars)) %>%
    rowSums(na.rm = FALSE)

  # sum across eah_kcal_vars columns
  stacked_intake$eah_kcal_consumed <- stacked_intake %>%
    dplyr::select(dplyr::all_of(eah_kcal_vars)) %>%
    rowSums(na.rm = FALSE)

  ## total (meal + eah)

  # sum across meal and eah grams_consumed
  stacked_intake$total_grams_consumed <- stacked_intake %>%
    dplyr::select(c("meal_grams_consumed", "eah_grams_consumed")) %>%
    rowSums(na.rm = FALSE)

  # sum across meal and eah grams_consumed
  stacked_intake$total_grams_consumed_inc_water <- stacked_intake %>%
    dplyr::select(c("meal_grams_consumed_inc_water", "eah_grams_consumed_inc_water")) %>%
    rowSums(na.rm = FALSE)

  # sum across meal and eah kcal_consumed
  stacked_intake$total_kcal_consumed <- stacked_intake %>%
    dplyr::select(c("meal_kcal_consumed", "eah_kcal_consumed")) %>%
    rowSums(na.rm = FALSE)

  # return data
  return(stacked_intake)

}
