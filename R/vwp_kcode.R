#' K-Code Lookup
#'
#' Determines if the attack included a middle on a route (K combination) or not
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "system" variable indicating an in-system, out-of-system, or no set
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lag
#' @importFrom rlang .data
#'
#' @export

vwp_kcode <- function(plays){
  return(plays %>% mutate(
    system = case_when(
      skill == "Set" & !is.na(set_code) ~ "In System",  # if a kcode exists then it's in-system
      skill == "Set" & is.na(set_code) ~ "Out of System",  # otherwise out of system
      skill == "Attack" & lag(skill) == "Set" & !is.na(lag(set_code)) ~ "In System",  # attacks do not have kcode, have to do this based on previous set
      skill == "Attack" & lag(skill) == "Set" & is.na(lag(set_code)) ~ "Out of System",
      skill == "Attack" & lag(skill) == "Reception" & team != lag(team) ~ "Reception Overpass",
      skill == "Attack" & (lag(skill) != "Set" | is.na(lag(skill))) ~ "Net Play",  # Overpass, bad opponent set, etc. - anything attacked directly at the net
      TRUE ~ NA_character_  # if it's not a set or an attack, kcode is irrelevant
    )  # end case_When
  )   # end mutate
  )  # end return
}
