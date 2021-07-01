#' Assign next attack
#'
#' Assigns a code to all skills indicating the team and system of the next attack
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "blockers" variable indicating whether the attack faced a solo, double, triple, or seam block, or no block
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr case_when
#' @importFrom tidyr fill
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' @importFrom rlang .data
#'
#' @export

vwp_next_attack_details <- function(plays){
  
  if(!("system" %in% names(plays))){
    plays$system <- "Unknown"
  }  # if we don't know the attack system, just label everything as unknown
  
  # add information for attacks
  add_attack_number <- plays %>% 
    group_by(match_id, point_id) %>%
    mutate(attack1 = cumsum(skill == "Attack")) %>%
    mutate(attack2 = c(0, lag(attack1)[-1])) %>%  # replace the NA at the start with a 0
    group_by(match_id, point_id, attack2) %>%
    mutate(attacking_team = if_else(
      skill == "Attack",
      team, # if skill is Attack then fill in the team
      NA_character_ # otherwise it's NA
    ),
    attacking_system = if_else(
      skill == "Attack",
      system,  # if skill is Attack then fill in the system
      NA_character_
    )
    ) %>%
    tidyr::fill(attacking_team, attacking_system, .direction = "up") %>%
    ungroup()
  
  # And now for a mess of grouped mutates
  # Mutate #1: make it so that the attack is grouped with its results
  # Mutate #2: fill in the attacking team and system of the attack
  # Mutate #3: fill in the attacking team and system of the next attack
  add_attack_team_system <- add_attack_number %>% 
    group_by(match_id, point_id, attack1) %>%
    mutate(attacking_team_next = lead(attacking_team),
           system_next = lead(attacking_system),
           attacking_team2 = if_else(
             team == attacking_team_next,
             "Touching Team",
             "Opposing Team"
           )
    ) %>%
    tidyr::fill(attacking_team_next, system_next, attacking_team2, .direction = "updown") %>%
    ungroup()

  # Now convert everything to a 2-letter designation
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
    select(-attack1, -attack2, -attacking_team, -attacking_team_next, -attacking_system, -system_next, -attacking_team2, -attack)  # helpers we don't need
  
  return(attack_details)
}
