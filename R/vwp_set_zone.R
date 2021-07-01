#' Assign quality of set location.
#'
#' Assigns a code to attacks and sets to indicate the quality of the set location. Currently just recodes the end zone of the (previous) set.
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "set_zone" variable indicating the location of the (previous) set.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @export


vwp_set_zone <- function(plays){
  return(
    plays %>% mutate(
      set_zone_numeric = case_when(
        skill == "Attack" & lag(skill, 1) == "Set" ~ lag(end_zone, 1),
        skill == "Set" ~ end_zone,
        TRUE ~ NA_real_
      )  # end case_when
    ) %>%  # end mutate #1
      mutate(set_zone = case_when(
        set_zone_numeric == 1 ~ "BR", # Back Right
        set_zone_numeric == 2 ~ "FR", # Front Right
        set_zone_numeric == 3 ~ "FC", # Front Center
        set_zone_numeric == 4 ~ "FL", # Front Left
        set_zone_numeric == 5 ~ "BL", # Back Left
        set_zone_numeric == 6 ~ "BC", # Back Center
        set_zone_numeric == 7 ~ "ML", # Middle Left
        set_zone_numeric == 8 ~ "MC", # Middle Center
        set_zone_numeric == 9 ~ "MR", # Middle Right
        is.na(set_zone_numeric) ~ "UN", # Unknown - either no set or not recorded
        TRUE ~ "UN" # if anything else is weird
      ) # end case_when
      ) %>% # end mutate #2
      select(-set_zone_numeric)
  )
}
