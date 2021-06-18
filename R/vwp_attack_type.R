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
                         !is.na(attack_type_original) & attack_type_original != "Hard spike" ~ "Off speed", # if there is an attack type that isn't hard spike, it's off speed
                         TRUE ~ NA_character_  
                       )  # end 2nd case_when
    ) %>%   # end mutate
      select(-attack_type_original)  # if there is a different in roll shot vs. soft spike, we can remove the second case_when and the select
  )  # end return
  
}