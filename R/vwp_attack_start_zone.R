#' Assign start zone of attack
#'
#' Assigns a code to attacks and digs to indicate the start zone of the attack. Currently only supports the start_zone column (no subzones yet).
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with an "attack_start_zone" variable indicating the starting zone of the (preceding) attack
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr lag
#' @importFrom rlang .data
#'
#' @export

vwp_attack_start_zone <- function(plays){
  return(
    plays %>% 
      mutate(attack_start_zone_numeric =
               case_when(
                 skill == "Attack" ~ start_zone,
                 skill == "Dig" & lag(skill, 1) == "Attack" ~ lag(start_zone, 1),  # is this the dig start_zone?
                 skill == "Dig" & lag(skill, 1) != "Attack" & lag(skill, 2) == "Attack" ~ lag(start_zone, 2),
                 TRUE ~ NA_real_
               ) # end case_when
             ) %>% # end mutate #1
      mutate(attack_start_zone =
               case_when(
                 attack_start_zone_numeric %in% c(1, 5, 6) ~ "Back Row",  # group everything from the far back
                 attack_start_zone_numeric %in% c(2, 3, 4, 7, 8, 9) ~ paste("Zone", attack_start_zone_numeric),
                 TRUE ~ NA_character_
               )
            ) %>% # end mutate #2
      select(-attack_start_zone_numeric)
  )  # end return
}
