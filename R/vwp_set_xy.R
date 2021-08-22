#' Transform volleyball Set x-y coordinates
#'
#' Transforms set x-y coordinates based on the distance from Zone 3
#'  
#' We assume that there are negligible effects of passing slightly left/right/off the net compared to perfect pass. 
#' However, if the pass is outside Zone 3, the setting options and quality decrease quickly the further outside the zone. 
#' We transform x and y coordinates to their distance from Zone 3.
#'
#' @param plays a dv_plays object or data frame containing play-by-play data.
#' 
#' @return The same object, with new columns containing the transformed x and y coordinates of the set
#'
#' @importFrom tibble tibble
#'
#' @export

vwp_set_xy <- function(plays){
  return(
    plays %>% mutate(
      set_zone_x = case_when(
        skill == "Attack" & lag(skill, 1) == "Set" ~ lag(start_coordinate_x, 1),
        skill == "Set" ~ start_coordinate_x,
        TRUE ~ NA_real_
      ),  # end case_when
      set_zone_y = case_when(
        skill == "Attack" & lag(skill, 1) == "Set" ~ lag(start_coordinate_y, 1),
        skill == "Set" ~ start_coordinate_y,
        TRUE ~ NA_real_
      )
    ) %>%  # end mutate #1
      mutate(set_x = if_else(set_zone_x <= 2.5 & set_zone_x >= 2, 0, pmin(abs(set_zone_x - 2.5), abs(set_zone_x - 2))),
             set_y = if_else(set_zone_y <= 3.5 & set_zone_y >= 3, 0, pmin(abs(set_zone_y - 3.5), abs(set_zone_y - 3))) 
      ) %>% # end mutate #2
      select(-set_zone_x, -set_zone_y)
  )
}
