#' Assign attack type
#'
#' Assigns a code to attacks, blocks and digs to indicate the quality of the attack. Currently just recodes hard spikes vs. everything else is off-speed.
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with an "attack_type" variable indicating whether the attack was a hard spike or offspeed attack (tip, roll shot, etc.).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @export


vwp_attack_type <- function(plays){
  return(
    plays %>% mutate(attack_type_original =
                       case_when(
                         skill == "Attack" ~ skill_subtype,  # type of attack
                         lag(skill, 1) == "Attack" ~ lag(skill_subtype, 1), # attack type of previous touch
                         lag(skill, 1) == "Block" & lag(skill, 2) == "Attack" ~ lag(skill_subtype, 2),  # if off a block touch
                         TRUE ~ NA_character_  # if not immediately after an attack or block
                       ), # end 1st case_when
                     attack_type = 
                       case_when(
                         attack_type_original == "Hard spike" ~ attack_type_original,
                         !is.na(attack_type_original) & attack_type_original != "Hard spike" ~ "Offspeed", # if there is an attack type that isn't hard spike, it's off speed
                         TRUE ~ "Unknown"  
                       )  # end 2nd case_when
    ) %>%   # end mutate
      select(-attack_type_original)  # if there is a different in roll shot vs. soft spike, we can remove the second case_when and the select
  )  # end return
  
}
