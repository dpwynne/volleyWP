#' Assign receiving player
#'
#' Assigns the receiving player to the serve. If the serve is not received (e.g., error), assigns TEAM as the receiving player.
#'    The model does not compute a separate point-win probability for a reception, so this can be used to assign credit for receptions.
#'    Additionally, adds a receiving player TEAM for unforced errors to make the win-probability differences balance out.
#'    
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "receiving_player" variable indicating which player (if any) received the serve
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @export


vwp_receiving_player <- function(plays){
  return(
    plays %>% 
      mutate(receiving_player =
               case_when(
                 skill == "Serve" & lead(skill, 1) == "Reception" ~ lead(player_name, 1),  # player who receives the serve
                 skill == "Serve" & (is.na(lead(skill, 1)) | lead(skill, 1) != "Reception") ~ "TEAM",  # team in general gets credited with aces/errors that are not touched/assigned to player
                 skill %in% c("Set", "Freeball") & evaluation == "Error" ~ "TEAM", # also give generic "TEAM" credit for ball-handling errors and freeball errors - unforced errors resulting in point for team 
                 TRUE ~ NA_character_ # if not a serve, this column should be NA
               ) # end case_when
      ) # end mutate
  )  # end return
}
