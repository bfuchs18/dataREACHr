#' json_dexa: Generates a json file for the dexa
#'
#' This function generates a json file for XXXX
#' @return A string with data stored in JSON format containing meta-data for dexa data
#'
#'
#' @export

json_dexa <- function() {

  dexa_list <- list(
    'MeasurementToolMetadata' = list(
      Description = '',
      Reference = '',
      TermURL = ''),
    participant_id = list( Description = 'participant id number'),
    session_id = list( Description = 'BIDS session ID indicating when data was collected',
                       Levels = list ('ses-1' = 'session 1 / baseline',
                                      'ses-2' = 'session 2 / follow-up')),

    # add demo dexa vars

    # dexa variables
    dxa_l_arm_area = list( Description = 'L Arm Area'),
    dxa_l_arm_bmc = list( Description = 'L Arm bone mineral content (BMC)'),
    dxa_l_arm_bmd = list( Description = 'L Arm bone mineral density (BMD)'),
    dxa_r_arm_area = list( Description = 'R Arm Area'),
    dxa_r_arm_bmc = list( Description = 'R Arm bone mineral content (BMC)'),
    dxa_r_arm_bmd = list( Description = 'R Arm bone mineral density (BMD)'),

    dxa_l_ribs_area = list( Description = 'L Ribs Area'),
    dxa_l_ribs_bmc = list( Description = 'L Ribs bone mineral content (BMC)'),
    dxa_l_ribs_bmd = list( Description = 'L Ribs bone mineral density (BMD)'),
    dxa_r_ribs_area = list( Description = 'R Ribs Area'),
    dxa_r_ribs_bmc = list( Description = 'R Ribs bone mineral content (BMC)'),
    dxa_r_ribs_bmd = list( Description = 'R Ribs bone mineral density (BMD)'),

    dxa_t_spine_area = list( Description = 'T Spine Area'),
    dxa_t_spine_bmc = list( Description = 'T Spine bone mineral content (BMC)'),
    dxa_t_spine_bmd = list( Description = 'T Spine bone mineral density (BMD)'),

    dxa_l_spine_area = list( Description = 'L Spine Area'),
    dxa_l_spine_bmc = list( Description = 'L Spine bone mineral content (BMC)'),
    dxa_l_spine_bmd = list( Description = 'L Spine bone mineral density (BMD)'),

    dxa_pelvis_area = list( Description = 'Pelvis Area'),
    dxa_pelvis_bmc = list( Description = 'Pelvis bone mineral content (BMC)'),
    dxa_pelvis_bmd = list( Description = 'Pelvis bone mineral density (BMD)'),

    dxa_l_leg_area = list( Description = 'L Leg Area'),
    dxa_l_leg_bmc = list( Description = 'L Leg bone mineral content (BMC)'),
    dxa_l_leg_bmd = list( Description = 'L Leg bone mineral density (BMD)'),
    dxa_r_leg_area = list( Description = 'R Leg Area'),
    dxa_r_leg_bmc = list( Description = 'R Leg bone mineral content (BMC)'),
    dxa_r_leg_bmd = list( Description = 'R Leg bone mineral density (BMD)'),

    dxa_subtotal_area = list( Description = 'Subtotal Area'),
    dxa_subtotal_bmc = list( Description = 'Subtotal bone mineral content (BMC)'),
    dxa_subtotal_bmd = list( Description = 'Subtotal bone mineral density (BMD)'),
    dxa_subtotal_zscore = list( Description = 'Subtotal Z Score'),

    dxa_head_area = list( Description = 'Head Area'),
    dxa_head_bmc = list( Description = 'Head bone mineral content (BMC)'),
    dxa_head_bmd = list( Description = 'Head bone mineral density (BMD)'),

    dxa_total_area = list( Description = 'Total Area'),
    dxa_total_bmc = list( Description = 'Total bone mineral content (BMC)'),
    dxa_total_bmd = list( Description = 'Total bone mineral density (BMD)'),
    dxa_total_zscore = list( Description = 'Total Z Score'),

    dxa_l_arm_fat_mass = list( Description = 'L Arm Fat Mass'),
    dxa_l_arm_lean_bmc_comb = list( Description = 'L Arm Lean And bone mineral content (BMC) Combined'),
    dxa_l_arm_total_mass = list( Description = 'L Arm Total Mass'),
    dxa_l_arm_perc_fat = list( Description = 'dxa_l_arm_perc_fat'),
    dxa_l_arm_perc_fat_ptile = list( Description = 'L Arm Percent Fat aged matched percentile'),
    dxa_r_arm_fat_mass = list( Description = 'R Arm Fat Mass'),
    dxa_r_arm_lean_bmc_comb = list( Description = 'R Arm Lean and bone mineral content (BMC) Combined'),
    dxa_r_arm_total_mass = list( Description = 'R Arm Total Mass'),
    dxa_r_arm_perc_fat = list( Description = 'R Arm Percent Fat'),
    dxa_r_arm_perc_fat_ptile = list( Description = 'R Arm Percent Fat aged matched percentile'),

    dxa_trunk_fat_mass = list( Description = 'Trunk Fat Mass'),
    dxa_trunk_lean_bmc_comb = list( Description = 'Trunk Lean and bone mineral content (BMC) Combined'),
    dxa_trunk_total_mass = list( Description = 'Trunk Total Mass'),
    dxa_trunk_perc_fat = list( Description = 'Trunk Percent Fat'),
    dxa_trunk_perc_fat_ptile = list( Description = 'Trunk Percent Fat aged matched percentile'),

    dxa_l_leg_fat_mass = list( Description = 'L Leg Fat Mass'),
    dxa_l_leg_lean_bmc_comb = list( Description = 'L Leg Lean And bone mineral content (BMC) Combined'),
    dxa_l_leg_total_mass = list( Description = 'L Leg Total Mass'),
    dxa_l_leg_perc_fat = list( Description = 'L Leg Percent Fat'),
    dxa_l_leg_perc_fat_ptile = list( Description = 'L Leg Percent Fat aged matched percentile'),
    dxa_r_leg_fat_mass = list( Description = 'R Leg Fat Mass'),
    dxa_r_leg_lean_bmc_comb = list( Description = 'R Leg Lean And bone mineral content (BMC) Combined'),
    dxa_r_leg_total_mass = list( Description = 'R Leg Total Mass'),
    dxa_r_leg_perc_fat = list( Description = 'R Leg Percent Fat'),
    dxa_r_leg_perc_fat_ptile = list( Description = 'R Leg Percent Fat aged matched percentile'),

    dxa_subtotal_fat_mass = list( Description = 'Subtotal Fat Mass'),
    dxa_subtotal_lean_bmc_comb = list( Description = 'Subtotal Lean and bone mineral content (BMC) Combined'),
    dxa_subtotal_total_mass = list( Description = 'Subtotal Total Mass'),
    dxa_subtotal_perc_fat = list( Description = 'Subtotal Percent Fat'),
    dxa_subtotal_perc_fat_ptile = list( Description = 'Subtotal Percent Fat aged matched percentile'),

    dxa_head_fat_mass = list( Description = 'Head Fat Mass'),
    dxa_head_lean_bmc_comb = list( Description = 'Head Lean and bone mineral content (BMC) Combined'),
    dxa_head_total_mass = list( Description = 'Head Total Mass'),
    dxa_head_perc_fat = list( Description = 'Head Percent Fat'),

    dxa_total_fat_mass = list( Description = 'Total Fat Mass'),
    dxa_total_lean_bmc_comb = list( Description = 'Total Lean and bone mineral content (BMC) Combined'),
    dxa_total_total_mass = list( Description = 'Total Total Mass'),
    dxa_total_perc_fat = list( Description = 'Total Percent Fat'),
    dxa_total_perc_fat_ptile = list( Description = 'Total Percent Fat aged matched percentile'),

    dxa_android_fat_mass = list( Description = 'Android Fat Mass'),
    dxa_android_lean_bmc_comb = list( Description = 'Android Lean And bone mineral content (BMC) Combined'),
    dxa_android_total_mass = list( Description = 'Android Total Mass'),
    dxa_android_perc_fat = list( Description = 'Android Percent Fat'),

    dxa_gynoid_fat_mass = list( Description = 'Gynoid Fat Mass'),
    dxa_gynoid_lean_bmc_comb = list( Description = 'Gynoid Lean and bone mineral content (BMC) Combined'),
    dxa_gynoid_total_mass = list( Description = 'Gynoid Total Mass'),
    dxa_gynoid_perc_fat = list( Description = 'Gynoid Percent Fat'),


    dxa_total_body_perc_fat = list( Description = 'Total Body Percent Fat Result'),
    dxa_total_body_perc_fat_ptile = list( Description = 'Total Body Percent Fat aged matched percentile'), # name doesnt match FB - dxa_bodyfat_ptile

    dxa_fatmass_height_ratio = list( Description = 'Fat Mass Over Height Result'),
    dxa_fatmass_height_ratio_ptile = list( Description = 'Fat Mass Over Height aged matched percentile'),

    dxa_android_gynoid_ratio = list( Description = 'Android Over Gynoid Ratio Result'),

    dxa_percfat_trunk_legs_ratio = list( Description = 'Percent Fat Trunk Over Percent Fat Legs Result'),
    dxa_percfat_trunk_legs_ratio_ptile = list( Description = 'Percent Fat Trunk Over Percent Fat Legs aged matched percentile'),

    dxa_fatmass_trunk_limb_ratio = list( Description = 'Trunk Over Limb Fat Mass Ratio Result'), # name doesnt match FB - dxa_fatmass_trunk_legs_ratio
    dxa_fatmass_trunk_limb_ratio_ptile = list( Description = 'Trunk Over Limb Fat Mass Ratio aged matched percentile'), # name doesnt match FB - dxa_fatmass_trunk_legs_ratio_ptile

    dxa_est_vat_mass = list( Description = 'Est viseral adipose tissue (VAT) Mass'),
    dxa_est_vat_volume = list( Description = 'Est viseral adipose tissue (VAT) Volume'),
    dxa_est_vat_area = list( Description = 'Est viseral adipose tissue (VAT) Area'),

    dxa_lean_height_ratio = list( Description = 'Lean Over Height Result'),
    dxa_lean_height_ratio_ptile = list( Description = 'Lean Over Height aged matched percentile'),

    dxa_appen_lean_height = list( Description = 'Appen Lean Over Height Result'),
    dxa_appen_lean_height_ptile = list( Description = 'Appen Lean Over Height aged matched percentile'),

    dxa_l_arm_lean_mass = list( Description = 'L Arm Lean Mass'),
    dxa_r_arm_lean_mass = list( Description = 'R Arm Lean Mass'),

    dxa_trunk_bmc = list( Description = 'Trunk bone mineral content (BMC)'),

    dxa_trunk_lean_mass = list( Description = 'Trunk Lean Mass'),
    dxa_l_leg_lean_mass = list( Description = 'L Leg Lean Mass'),
    dxa_r_leg_lean_mass = list( Description = 'R Leg Lean Mass'),
    dxa_subtotal_lean_mass = list( Description = 'Subtotal Lean Mass'),
    dxa_head_lean_mass = list( Description = 'Head Lean Mass'),
    dxa_total_lean_mass = list( Description = 'Total Lean Mass')
    )

  # convert formatting to JSON
  dexa_json <- RJSONIO::toJSON(dexa_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(dexa_json, asText = TRUE))){
    print('dexa JSON file may be invalid')
  }

  return(dexa_json)

}
