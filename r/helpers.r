library(StatsBombR)

### Global variables ----

# aspect_ratio pitch_length/pitch_width does not result in correct scales with theme_pitch, unclear why?
pitch_ratio = 1.46

### Helper functions ----

unlist_location <- function(match) {
  match$x <- lapply(match$location, (function (x) x[1]))
  match$y <- lapply(match$location, (function (x) x[2]))
  match$x_end <- lapply(match$pass.end_location, (function (x) x[1]))
  match$y_end <- lapply(match$pass.end_location, (function (x) x[2]))
  return(match)
}

load_match <- function(match_id) {
  comp <- FreeCompetitions()
  matches <- FreeMatches(comp)
  row_nr = which(matches[,1] == match_id)
  match <- get.matchFree(matches[row_nr,])
  match <- unlist_location(match)
  return(match)
}

load_lineup <- function(match_id) {
  comp <- FreeCompetitions()
  matches <- FreeMatches(comp)
  row_nr = which(matches[,1] == match_id)
  lineup <- get.lineupsFree(matches[row_nr,])
  lineup <- rbind(lineup$lineup[[1]], lineup$lineup[[2]])
  return(lineup)
}


pitch_measures <- function(pitch_length, pitch_width) {
  return(list(
    length = pitch_length,
    width = pitch_width,
    penalty_box_length = 16.5,
    penalty_box_width = 16.5 * 2 + 7.32,
    six_yard_box_length = 5.5,
    six_yard_box_width = 5.5 * 2 + 7.32,
    penalty_spot_distance = 11,
    goal_width = 7.32,
    origin_x = 0,
    origin_y = 0
  ))
}