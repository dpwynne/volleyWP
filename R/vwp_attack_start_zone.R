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
#' @importFrom dplyr lag
#' @importFrom rlang .data
#'
#' @export

vwp_attack_start_zone <- function(plays){
  return(
    plays %>% 
      mutate(attack_start_zone =
               case_when(
                 skill == "Attack" ~ start_zone,
                 skill == "Dig" & lag(skill, 1) == "Attack" ~ lag(start_zone, 1),  # is this the dig start_zone?
                 skill == "Dig" & lag(skill, 1) != "Attack" & lag(skill, 2) == "Attack" ~ lag(start_zone, 2),
                 TRUE ~ NA_real_
               ) %>% # end case_when
               as.character() # convert to character or factor because zones should not be numeric
      ) # end mutate
  )  # end return
}
