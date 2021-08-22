#' Add a setter flag
#'
#' Indicates whether the setter touched the ball
#' 
#' Due to the nature of the positions, attacks, digs/freeballs, and sets from setters are qualitatively different from these touches
#' when played by other positions. For teams running a 6-2 offense, this function may still be worthwhile for digs and sets.
#' 
#' @param plays a dv_plays object or data frame containing play-by-play data.
#' @param setters a character vector containing the names of all setters on the teams recorded in the `plays` dataset.
#'     For smaller datasets, the user will typically know this. For larger datasets, it must be inferred.
#'
#' @return  The same dataset, with a column with value 1 if the player touching the ball is a setter and 0 otherwise
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @export

vwp_setter_touch <- function(plays, setters){
  return(
    plays %>% mutate(
      setter = if_else(player_name %in% setters, 1, 0)
    )
  )
}
