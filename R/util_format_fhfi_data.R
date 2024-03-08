#' util_format_fhfi_data: Prepare Fulkerson HFI data for scoring (called within util_redcap_parent_v4.R)
#'
#' This function prepares Fulkerson HFI data for scoring with dataprepr::score_hfi()
#'
#'
#' @param fhfi_data Fulkerson HFI extracted from data from REDCap event parent_visit_4_arm_1'
#'
util_format_fhfi_data <- function(fhfi_data) {

  # replace fhfi with hfi
  names(fhfi_data) <- gsub('fhfi', 'hfi', names(fhfi_data))

  # Fix type labels where:

  # vegetable:
    # a___0 =  _fresh
    # a___1 =  _can
    # a___2 =  _frozen
  # fruit
    # a___0 =  _fresh
    # a___1 =  _can
    # a___2 =  _frozen
    # a___3 =  _dried
  # bread
    # a___0 =  _fresh
    # a___1 =  _frozen
  # prepared dessert
    # a___0 =  _storebought
    # a___1 =  _homemade

  for (col in names(fhfi_data)) {
    if (grepl("vegetable", col) == TRUE) {
      if (grepl("a___0", col) == TRUE) {
        names(fhfi_data)[which(names(fhfi_data) == col)] <-
          sub("a___0", "_fresh", col)
      } else {
        if (grepl("a___1", col) == TRUE) {
          names(fhfi_data)[which(names(fhfi_data) == col)] <-
            sub("a___1", "_can", col)
        } else {
          if (grepl("a___2", col) == TRUE) {
            names(fhfi_data)[which(names(fhfi_data) == col)] <-
              sub("a___2", "_frozen", col)
          }
        }
      }
    } else {
      if (grepl("fruit", col) == TRUE) {
        if (grepl("a___0", col) == TRUE) {
          names(fhfi_data)[which(names(fhfi_data) == col)] <-
            sub("a___0", "_fresh", col)
        } else {
          if (grepl("a___1", col) == TRUE) {
            names(fhfi_data)[which(names(fhfi_data) == col)] <-
              sub("a___1", "_can", col)
          } else {
            if (grepl("a___2", col) == TRUE) {
              names(fhfi_data)[which(names(fhfi_data) == col)] <-
                sub("a___2", "_frozen", col)
            } else {
              if (grepl("a___3", col) == TRUE) {
                names(fhfi_data)[which(names(fhfi_data) == col)] <-
                  sub("a___3", "_dried", col)
              }
            }
          }
        }
      } else {
        if (grepl("bread", col) == TRUE) {
          if (grepl("a___0", col) == TRUE) {
            names(fhfi_data)[which(names(fhfi_data) == col)] <-
              sub("a___0", "_fresh", col)
          } else {
            if (grepl("a___1", col) == TRUE) {
              names(fhfi_data)[which(names(fhfi_data) == col)] <-
                sub("a___1", "_frozen", col)
            }
          }
        } else {
          if (grepl("prepared_dessert", col) == TRUE) {
            if (grepl("a___0", col) == TRUE) {
              names(fhfi_data)[which(names(fhfi_data) == col)] <-
                sub("a___0", "_storebought", col)
            } else {
              if (grepl("a___1", col) == TRUE) {
                names(fhfi_data)[which(names(fhfi_data) == col)] <-
                  sub("a___1", "_homemade", col)
              }
            }
          }
        }
      }
    }
  }

  # Convert numbers to letters

  ### NOTE: hfi_visible_ goes up to 28 -- what letters should 27 and 28 get??

  for (col in names(fhfi_data)) {
    # extract after last _
    extracted <- sub(".*_", "", col)

    # if extracted can be converted to numeric
    if (varhandle::check.numeric(extracted) == TRUE) {
      extracted_num = as.numeric(extracted)
      letter_replacement = letters[extracted_num]

      # replace extracted with letter_replacement
      names(fhfi_data)[which(names(fhfi_data) == col)] <-
        sub(extracted, letter_replacement, col)

    } else {
      # extract between second to last and last _
      split_string <- unlist(strsplit(col, "_"))
      extracted <- split_string[length(split_string) - 1]

      # if extracted can be converted to numeric
      if (varhandle::check.numeric(extracted) == TRUE) {
        extracted_num = as.numeric(extracted)
        letter_replacement = letters[extracted_num]

        # replace extracted with letter_replacement
        names(fhfi_data)[which(names(fhfi_data) == col)] <-
          sub(extracted, letter_replacement, col)

      }
    }
  }

  # fix category names
  names(fhfi_data) <- gsub('visible', 'accessible', names(fhfi_data))
  names(fhfi_data) <- gsub('hfi_dessert', 'hfi_frozen_dessert', names(fhfi_data))

  # return data
  return(fhfi_data)

}
