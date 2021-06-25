vwp_next_attack_details <- function(plays){
  
  add_attack_number <- plays %>% 
    group_by(match_id, point_id) %>%
    mutate(attack1 = cumsum(skill == "Attack"),
           attacking_team = if_else(
             skill == "Attack",
             team, # if skill is Attack then fill in the team
             NA_character_ # otherwise it's NA
           )
    )%>%
    fill(attacking_team, .direction = "updown") %>%
    ungroup()
  
  add_attack_team_system <- add_attack_number %>% 
    group_by(match_id, point_id) %>%
    mutate(
      attack2 = c(0, lag(attack1)[-1]),  # replace the NA at the start with a 0
      attacking_team_next = lead(attacking_team)
    ) %>%
    group_by(match_id, point_id, attack2) %>%
    fill(system, .direction = "up") %>%  # not sure if I need this but just making sure
    mutate(attacking_team2 = if_else(
      team == attacking_team_next,
      "Touching Team",
      "Opposing Team"
    ),
    system_next = lead(system)
    ) %>%
    ungroup()
  
  attack_details <- add_attack_team_system %>%
    mutate(attack = str_c(
      str_sub(attacking_team2, 1, 1),
      str_sub(system_next, 1, 1)
    )) %>%
    mutate(attack_next = case_when(
      is.na(attack) & team == point_won_by ~ "TW",  # team win point
      is.na(attack) & team != point_won_by ~ "OW",  # opponent win point
      TRUE ~ attack
    )) %>% 
    select(-attack1, -attack2, -attacking_team_next, -system_next, -attack, -attacking_team2)  # helpers we don't need
  
  return(attack_details)
}
