#' json_v1_liking: Generates a json file for visit 1 VAS liking data
#'
#' This function generates a json file for visit 1 VAS liking data
#'
#' @return A string with data stored in JSON format containing meta-data
#'
#'
#' @export

json_v1_liking <- function() {

  v1_liking_list <- list(
    liking_popcorn = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this popcorn?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_pretzel = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this pretzel?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_corn_chip = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this corn chip?',
                             Levels = list ('0' =	'Hate it',
                                            '1'	= 'Dislike it',
                                            '2'	= 'It\'s okay',
                                            '3'	= 'Like it',
                                            '4'	= 'Love it')),
    liking_oreo = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this cookie?',
                        Levels = list ('0' =	'Hate it',
                                       '1'	= 'Dislike it',
                                       '2'	= 'It\'s okay',
                                       '3'	= 'Like it',
                                       '4'	= 'Love it')),
    liking_brownie = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this brownie?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_starburst = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this Starburst?',
                             Levels = list ('0' =	'Hate it',
                                            '1'	= 'Dislike it',
                                            '2'	= 'It\'s okay',
                                            '3'	= 'Like it',
                                            '4'	= 'Love it')),
    liking_skittle = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this Skittle?',
                           Levels = list ('0' =	'Hate it',
                                          '1'	= 'Dislike it',
                                          '2'	= 'It\'s okay',
                                          '3'	= 'Like it',
                                          '4'	= 'Love it')),
    liking_chocolate = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this chocolate (Hershey Kiss)?',
                             Levels = list ('0' =	'Hate it',
                                            '1'	= 'Dislike it',
                                            '2'	= 'It\'s okay',
                                            '3'	= 'Like it',
                                            '4'	= 'Love it')),
    liking_icecream = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this ice cream?',
                            Levels = list ('0' =	'Hate it',
                                           '1'	= 'Dislike it',
                                           '2'	= 'It\'s okay',
                                           '3'	= 'Like it',
                                           '4'	= 'Love it')),
    liking_grilled_cheese = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this grilled cheese?',
                                  Levels = list ('0' =	'Hate it',
                                                 '1'	= 'Dislike it',
                                                 '2'	= 'It\'s okay',
                                                 '3'	= 'Like it',
                                                 '4'	= 'Love it')),
    liking_chicken_tender = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this chicken tender?',
                                  Levels = list ('0' =	'Hate it',
                                                 '1'	= 'Dislike it',
                                                 '2'	= 'It\'s okay',
                                                 '3'	= 'Like it',
                                                 '4'	= 'Love it')),
    liking_potato_chips = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this potato chip?',
                                Levels = list ('0' =	'Hate it',
                                               '1'	= 'Dislike it',
                                               '2'	= 'It\'s okay',
                                               '3'	= 'Like it',
                                               '4'	= 'Love it')),
    liking_carrot = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this baby carrot?',
                          Levels = list ('0' =	'Hate it',
                                         '1'	= 'Dislike it',
                                         '2'	= 'It\'s okay',
                                         '3'	= 'Like it',
                                         '4'	= 'Love it')),
    liking_fruit = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this fruit cocktail',
                         Levels = list ('0' =	'Hate it',
                                        '1'	= 'Dislike it',
                                        '2'	= 'It\'s okay',
                                        '3'	= 'Like it',
                                        '4'	= 'Love it')),
    liking_water = list( Description = 'Food Liking Visual Analogue Scale (VAS): How much do you like this water?',
                         Levels = list ('0' =	'Hate it',
                                        '1'	= 'Dislike it',
                                        '2'	= 'It\'s okay',
                                        '3'	= 'Like it',
                                        '4'	= 'Love it'))
  )

  # convert formatting to JSON
  v1_liking_json <- RJSONIO::toJSON(v1_liking_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(v1_liking_json, asText = TRUE))){
    print('Liking for visit 1 JSON file may be invalid')
  }

  return(v1_liking_json)
}
