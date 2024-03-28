#' util_gen_ed_data: Generate a dataframe with energy density data
#'
#' This function generates a dataframe with energy density data for processing intake data with util_redcap_de()
#'
#'
#'
util_gen_ed_data <- function() {

  # calculate energy densities (kcal/g) according to nutrition label
  bread_ed <- 130/49
  butter_ed <- 50/14
  cheese_ed <- 70/19

  # create vectors
  food <- c('bread', 'butter', 'cheese')
  ed <- c(bread_ed, butter_ed, cheese_ed)

  # generate dataframe
  ed_data <- data.frame(food, ed)

  # return dataframe
  return(ed_data)

}
