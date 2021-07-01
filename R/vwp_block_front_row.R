#' Add opposing players in front row during an attack
#'
#' Using rotation information, record the names of the players who start in the front left/center/right positions, for the blocking team.
#' Because teams may run unusual blocking schemes (switch-blocking, middle on a pin, etc.), we can only know which players could possibly have been in position to block an attack.
#' Furthermore, a player may influence the attack without directly being involved in the block (e.g., if the setter intentionally sets away from a pin blocker)
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with three additional columns ("block_front_left", "block_front_center", "block_front_right") indicating the opposing team's front line during an attack
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lag
#' @importFrom purrr map2_chr
#' @importFrom rlang .data
#'
#' @export


vwp_block_front_row <- function(plays){
  if(all("visiting_p4" %in% names(plays), "home_p4" %in% names(plays))){
    return(
      plays %>% 
        mutate(block_front_left =
                 case_when(  # even though we only need this for attack, we might as well grab it for all skills
                   team == home_team ~ map2_chr(visiting_p4, opponent, vwp_player_number_to_player_name, plays = plays),  # This should work
                   team == visiting_team ~ map2_chr(home_p4, opponent, vwp_player_number_to_player_name, plays = plays),
                   TRUE ~ NA_character_ # we should never have this happen unless the team is NA
                 ), # end case_when for block_front_left - note that this is where they start, not where they end up in the blocking pattern
               block_front_center =
                 case_when(
                   team == home_team ~ map2_chr(visiting_p3, opponent, vwp_player_number_to_player_name, plays = plays),
                   team == visiting_team ~ map2_chr(home_p3, opponent, vwp_player_number_to_player_name, plays = plays),
                   TRUE ~ NA_character_
                 ),# end case for block_front_center
               block_front_right =
                 case_when(
                   team == home_team ~ map2_chr(visiting_p2, opponent, vwp_player_number_to_player_name, plays = plays),
                   team == visiting_team ~ map2_chr(home_p2, opponent, vwp_player_number_to_player_name, plays = plays),
                   TRUE ~ NA_character_
                 )# end case for block_front_right
        ) # end mutate
    )
    # we might be able to do this faster if we don't need to convert player numbers to player names or can do that at the very end
  } else {
    return(plays %>% mutate(
      block_front_left = "TEAM",
      block_front_center = "TEAM",
      block_front_right = "TEAM"
    )
    )
  }

}
