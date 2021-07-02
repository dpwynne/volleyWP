#' Add cover
#'
#' Adds whether a dig was a cover. #' 
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with an "add_cover" variable indicating whether the touch was a cover.
#' Also, replaces the start zone of the touch if the touch was a cover.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr if_else
#' @importFrom rlang .data
#'
#' @export

vwp_add_cover <- function(plays){
  covered <- plays %>% mutate(
    add_cover = (skill == "Dig" & lag(skill, 1) == "Block" & lag(skill, 2) == "Attack" & team == lag(team, 2) & team != lag(team, 1))
  )
  return(covered %>% mutate(
    start_zone = if_else(
      add_cover, 6 - lag(end_zone, 1), start_zone  # if a cover, change the start zone to the "end zone" of the block  
    )
  ))
}