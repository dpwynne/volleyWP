#' Fix clearly impossible skill codes
#'
#' Recodes touches that can't possibly have been classified correctly in the original datavolley file.
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with the "skill" variable updated to change clearly-impossible touches to their more likely equivalents
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr lag
#' @importFrom rlang .data
#'
#' @export

vwp_fix_skill_codes <- function(plays){
  return(
    plays %>% 
      mutate(skill =
               case_when(
                 skill == "Block" & lag(skill, 1) != "Attack" & lead(skill, 1) != "Dig" ~ "Attack",  # can technically have a block without an attack, but this is really rare and the next skill should be a dig
                 skill == "Reception" & lag(skill, 1) != "Serve" ~ "Dig", # can't have a reception without a preceding serve; should probably be a dig instead - may be a freeball instead?
                 TRUE ~ skill # if none of these phantom issues show up, assume the skill is correctly id'd
               ) # end case_when
      ) # end mutate
  )  # end return
}
