#' Assign quality of block
#'
#' Assigns a code to attacks and blocks to indicate the quality of the block. Currently just recodes the num_players column.
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "blockers" variable indicating whether the attack faced a solo, double, triple, or seam block, or no block
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @export

vwp_blockers <- function(plays){
  return(
    plays %>% mutate(blockers =
                       case_when(
                         skill %in% c("Attack", "Block") & num_players == "1 player block" ~ "solo",
                         skill %in% c("Attack", "Block") & num_players == "2 player block" ~ "double",
                         skill %in% c("Attack", "Block") & num_players == "3 player block" ~ "triple",
                         skill %in% c("Attack", "Block") & num_players == "Hole block" ~ "seam",
                         skill %in% c("Attack", "Block") & num_players == "No block" ~ "none",
                         skill %in% c("Attack", "Block") & num_players == "Unexpected +" ~ "solo",
                         TRUE ~ NA_character_  # if not an attack or block, no blocker code
                       )  # end case_when
    )  # end mutate
  )  # end return
}
