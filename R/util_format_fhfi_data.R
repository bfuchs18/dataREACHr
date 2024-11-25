#' util_format_fhfi_data: Prepare Fulkerson HFI data for scoring (called within util_redcap_parent_v4.R)
#'
#' This function prepares Fulkerson HFI data for scoring with dataprepr::score_hfi()
#'
#'
#' @param fhfi_data Fulkerson HFI extracted from data from REDCap event parent_visit_4_arm_1'
#'
util_format_fhfi_data <- function(fhfi_data) {


  # define category replacements for column names, which will be applied to all columns
  category_replacements <- c(
    'milk' = 'dairy',
    'visible' = 'accessible',
    'hfi_dessert' = 'hfi_frozen',
    'hfi_butter' = 'hfi_oils',
    'hfi_vegetable' = 'hfi_veg',
    'hfi_meat' = 'hfi_protein',
    'hfi_prepared_dessert' = 'hfi_dessert',
    'hfi_chips' = 'hfi_snacks',
    'hfi_beverage' = 'hfi_bev',
    'hfi_microwave' = 'hfi_quick',
    'hfi_condo' = 'hfi_cond'
  )

  # define columns to explicitly rename (columns will be renamed after replacing "fhfi" with "hfi")
  explicit_renames <- c(
    hfi_16 = "hfi_prepackaged",
    hfi_17 = "hfi_cereal_1",
    hfi_18 = "hfi_cereal_2",
    hfi_19 = "hfi_cereal_3",

    # these columns were added the hfi in REACH and not part of the original scale
    hfi_extra_nondairy = "hfi_dairy_11",
    hfi_extra_cond = "hfi_cond_5",
    hfi_extra_accessible_fridge = "hfi_accessible_fridge_16"
  )

  # define visible fridge columns: items hfi_visible_13 through hfi_visible_28
  visible_fridge_vars <- c(paste0("hfi_visible_", seq(13, 28)))

  # update column names, make all changes except for converting numbers to letters ----
  fhfi_data_rename <- fhfi_data %>%

    # replace fhfi with hfi in all column names
    dplyr::rename_with( ~ gsub("fhfi", "hfi", .)) %>%

    # replace 'a___' with '_type' in all column names
    dplyr::rename_with( ~ gsub("a___", "_type", .)) %>%

    # rename visible_fridge_vars
    dplyr::rename_with( ~ gsub("hfi_visible", "hfi_visible_fridge", .),
                 .cols = all_of(visible_fridge_vars)) %>%

    # rename with category_replacements
    dplyr::rename_with(~ stringr::str_replace_all(., category_replacements)) %>%

    # explicitly rename columns based on explicit_renames
    dplyr::rename(!!!explicit_renames) # !!! unquotes the vector explicit_renames so it can be evaluated inside rename



  # Convert numbers to letters ----

  non_straightforward_renames <- c(

    # fridge variables -- start at 13 and no 16
    "hfi_accessible_fridge_13" = "hfi_accessible_fridge_a",
    "hfi_accessible_fridge_14" = "hfi_accessible_fridge_b",
    "hfi_accessible_fridge_15" = "hfi_accessible_fridge_c",
    "hfi_accessible_fridge_17" = "hfi_accessible_fridge_d",
    "hfi_accessible_fridge_18" = "hfi_accessible_fridge_e",
    "hfi_accessible_fridge_19" = "hfi_accessible_fridge_f",
    "hfi_accessible_fridge_20" = "hfi_accessible_fridge_g",
    "hfi_accessible_fridge_21" = "hfi_accessible_fridge_h",
    "hfi_accessible_fridge_22" = "hfi_accessible_fridge_i",
    "hfi_accessible_fridge_23" = "hfi_accessible_fridge_j",
    "hfi_accessible_fridge_24" = "hfi_accessible_fridge_k",
    "hfi_accessible_fridge_25" = "hfi_accessible_fridge_l",
    "hfi_accessible_fridge_26" = "hfi_accessible_fridge_m",
    "hfi_accessible_fridge_27" = "hfi_accessible_fridge_n",
    "hfi_accessible_fridge_28" = "hfi_accessible_fridge_o",

    # these variables don't get letters in place of numbers
    "hfi_16" = "hfi_16",
    "hfi_17" = "hfi_17",
    "hfi_18" = "hfi_18",
    "hfi_19" = "hfi_19"

  )

  # for columns that start with "hfi"
  for (col in names(fhfi_data_rename)[grepl("^hfi", names(fhfi_data_rename))]) {

    if (col %in% names(non_straightforward_renames)) {

      # replace based on non_straightforward_renames
      names(fhfi_data_rename)[names(fhfi_data_rename) == col] <- non_straightforward_renames[col]


    # else if "type" columns
    } else if (grepl("type", col)) {

      # extract between second to last and last _
      split_string <- unlist(strsplit(col, "_"))
      extracted <- split_string[length(split_string) - 1]

      # if extracted can be converted to numeric
      if (varhandle::check.numeric(extracted) == TRUE) {
        extracted_num = as.numeric(extracted)
        letter_replacement = letters[extracted_num]

        # replace extracted with letter_replacement
        names(fhfi_data_rename)[which(names(fhfi_data_rename) == col)] <-
          sub(extracted, letter_replacement, col)

      }

    # for all other columns
    } else  {

      # extract after last _
      extracted <- sub(".*_", "", col)

      if (varhandle::check.numeric(extracted) == TRUE) {
        extracted_num = as.numeric(extracted)
        letter_replacement = letters[extracted_num]

        # replace extracted with letter_replacement
        names(fhfi_data_rename)[which(names(fhfi_data_rename) == col)] <-
          sub(extracted, letter_replacement, col)

      }
    }
  }

  # return data
  return(fhfi_data_rename)

}
