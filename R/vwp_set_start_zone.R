vwp_set_start_zone <- function(plays){
  return(plays %>% mutate(
    set_from = case_when(
      skill == "Set" ~ start_zone,  # for a set, the start zone of the set
      skill == "Attack" & lag(skill) == "Set" & team == lag(team) ~ lag(start_zone),  # for attack, the start zone of the previous set
      TRUE ~ NA_character_  # if not a set or attack, not useful
    )
  ))
}