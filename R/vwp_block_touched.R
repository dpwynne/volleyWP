#' Add whether the dig is off a block touch or not
#'
#' Classify a dig as being off a block touch or directly off an attack. Covers should always be classified as off a block touch. 
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "block_touched" variable taking the value 1 if the touch is a dig/cover off a block touch and 0 otherwise
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lag
#' @importFrom rlang .data
#'
#' @export
#' 
vwp_block_touched <- function(plays){
  return(
    plays %>% mutate(block_touched =
                       case_when(
                         skill == "Cover" ~ 1,
                         skill == "Dig" & lag(skill, 1) == "Attack" ~ 0,
                         lag(skill, 1) == "Block" ~ 1, # weird edge case where there's a different touch immediately after the block
                         TRUE ~ 0  # if not an dig or cover, or immediately after a block, then it wasn't block touched
                       )  # end case_when
    )  # end mutate
  )  # end return
}
