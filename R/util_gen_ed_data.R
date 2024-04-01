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

  grilled_cheese_ed <- NA # this will remain NA -- as this will be calculated for each person

  # EDs taken from redcap -- need to confirm
  tender_ed <- 2.371
  carrot_ed <- 0.353
  chips_ed <- 5.714
  fruit_ed <- 0.427
  water_ed <- 0.0
  ranch_ed <- 4.577
  ketchup_ed <- 1.176
  brownie_ed <- 4.363
  corn_chip_ed <- 5.714
  kiss_ed <- 5.000 # check this
  ice_cream_ed <- 2.184
  oreo_ed <- 4.701
  popcorn_ed <- 5.000 # check this
  pretzel_ed <- 3.929
  skittle_ed <- 3.929
  starburst_ed <- 4.138
  water_eah_ed <- 0.0

  # create vectors
  food <- c('bread', 'butter', 'cheese', 'grilled_cheese', 'tender', 'carrot', 'chips', 'fruit', 'water', 'ranch', 'ketchup',
            'brownie', 'corn_chip', 'kiss', 'ice_cream', 'oreo', 'popcorn', 'pretzel', 'skittle', 'starburst', 'water_eah')
  ed <- c(bread_ed, butter_ed, cheese_ed, grilled_cheese_ed, tender_ed, carrot_ed, chips_ed, fruit_ed, water_ed, ranch_ed, ketchup_ed,
          brownie_ed, corn_chip_ed, kiss_ed, ice_cream_ed, oreo_ed, popcorn_ed, pretzel_ed, skittle_ed, starburst_ed, water_eah_ed)

  # generate dataframe
  ed_data <- data.frame(food, ed)

  # return dataframe
  return(ed_data)

}
