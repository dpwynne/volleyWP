#' Convert player number to player name
#'
#' Looks up a player's team and number in the play-by-play data and reports the corresponding name 
#'
#' @param player_number the number of the player to replace
#' @param player_team the team for which the player plays
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

vwp_player_number_to_player_name <- function(player_number, player_team, plays){
  player_name <- unique(plays$player_name[which(plays$player_number == player_number & plays$team == player_team)])
  if(length(player_name) != 1){
    return(NA_character_) 
    # two options here: either no player found in the play-by-play with that number for that team, or multiple players found; either way, we can't assign a player name
  } else {
    return(player_name)
  }
}
