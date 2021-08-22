#' Dig distance
#'
#' Estimates the distance traveled by an attack to a dig attempt
#' 
#' We do not take into account deflections from a block when computing distance, it's just a
#' straight-line distance between the attack location and the dig location
#'
#' @param plays a dv_plays object or data frame containing play-by-play data.
#'
#' @return The same data frame, with the estimated (court) distance traveled by the ball between the attack and the dig
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#'
#' @export

vwp_dig_distance <- function(plays){
  
  dig_xy <- plays %>% mutate(
    dig_attack_x = case_when(
      !is.na(start_coordinate_x) ~ start_coordinate_x,
      start_zone %in% c(1, 2, 9) ~ 3,
      start_zone %in% c(3, 6, 8) ~ 2,
      start_zone %in% c(4, 5, 7) ~ 1,
      is.na(start_zone) ~ NA_real_
    ),
    dig_attack_y = case_when(
      !is.na(start_coordinate_y) ~ start_coordinate_y,
      start_zone %in% c(2, 3, 4) ~ 4,
      start_zone %in% c(7, 8, 9) ~ 5,
      start_zone %in% c(1, 5, 6) ~ 6,
      is.na(start_zone) ~ NA_real_
    ),
    dig_x = case_when(
      !is.na(end_coordinate_x) ~ end_coordinate_x,
      end_zone %in% c(1, 2, 9) ~ 3,
      end_zone %in% c(3, 6, 8) ~ 2,
      end_zone %in% c(4, 5, 7) ~ 1,
      is.na(end_zone) ~ NA_real_    
    ),
    dig_y = case_when(
      !is.na(end_coordinate_y) ~ end_coordinate_y,
      end_zone %in% c(2, 3, 4) ~ 3,
      end_zone %in% c(7, 8, 9) ~ 2,
      end_zone %in% c(1, 5, 6) ~ 1,
      is.na(end_zone) ~ NA_real_
    )
  ) %>% mutate(
    dig_x = case_when(
      !is.na(end_coordinate_x) ~ end_coordinate_x,
      end_subzone %in% c("A", "B") ~ dig_x + 0.25,
      end_subzone %in% c("C", "D") ~ dig_x - 0.25,
      is.na(end_subzone) ~ dig_x
    ),
    dig_y = case_when(
      !is.na(end_coordinate_y) ~ end_coordinate_y,
      end_subzone %in% c("B", "C") ~ dig_y + 0.25,
      end_subzone %in% c("A", "D") ~ dig_y - 0.25,
      is.na(end_subzone) ~ dig_y
    )    
  )
  
  return(dig_xy %>% mutate(
    dig_dist = sqrt((dig_x - dig_attack_x)^2 + (dig_y - dig_attack_y)^2)
  ) %>% select(-dig_attack_x, -dig_attack_y, -dig_x, -dig_y)
  )
}
