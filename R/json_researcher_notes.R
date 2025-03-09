#' json_researcher_notes: Generates a json file for researcher_notes
#'
#'
#' @return A string with data stored in JSON format containing meta-data for researcher_notes
#'
#'
#' @export

json_researcher_notes <- function() {

  notes_list <- list(
    'MeasurementToolMetadata' = list(
      Description = 'Researcher notes about visits and tasks'),

    participant_id = list( Description = 'participant id number'),
    child_protocol_1_notes = list( Description = 'researcher notes from child visit protocol 1'),
    dexa_ses1_notes = list( Description = 'research notes regarding session-1 (baseline / V1) dexa scan'),
    rrv_task_notes = list( Description = 'researcher notes regarding RRV task'),
    child_protocol_2_notes = list( Description = 'researcher notes from child visit protocol 2'),
    child_protocol_3_notes = list( Description = 'researcher notes from child visit protocol 3'),
    space_game_notes = list( Description = 'researcher notes regarding space game'),
    pit_notes_v3 = list( Description = 'researcher notes regarding PIT task on child visit protcol 3'),
    child_protocol_4_notes = list( Description = 'researcher notes from child visit protocol 4'),
    wasi_notes = list( Description = 'researcher notes regarding WASI'),
    pit_notes_v4 = list( Description = 'researcher notes regarding PIT task on child visit protcol 4'),
    child_protocol_5_notes = list( Description = 'researcher notes from child visit protocol 5'),
    dexa_ses2_notes = list( Description = 'research notes regarding session-2 (follow-up / V5) dexa scan')

  )

  # convert formatting to JSON
  notes_json <- RJSONIO::toJSON(notes_list, pretty = TRUE)

  # double check
  if (isFALSE(RJSONIO::isValidJSON(notes_json, asText = TRUE))){
    print('notes_json file may be invalid')
  }

  return(notes_json)

}
