#' Compute zone distance
#'
#' Assigns a numerical value to the estimated distance between the starting and ending location of an attack that is dug.
#' Values are in meters. Height of the attack is not considered.
#'     Currently assumes that all attacks start at the very front center of the zone and are dug in the exact center of the zone.
#' 
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "distance" variable indicating the estimated court distance the ball traveled between the attack and the dig
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @export


vwp_zone_distance <- function(plays){
  ## Need to hard-code distances from each attack start zone to each attack end zone (primarily for digs)
  return(
    # by default NA start_zone guess = 3, NA end_zone guess = 8
    plays %>% mutate(distance = case_when(
      (start_zone == 4 & end_zone == 2) | ((start_zone == 3 | is.na(start_zone)) & end_zone == 3) | (start_zone == 2 & end_zone == 4) ~ 1.5,
      (start_zone == 4 & end_zone == 9) | ((start_zone == 3 | is.na(start_zone)) & (end_zone == 8 | is.na(end_zone))) | (start_zone == 2 & end_zone == 7) | (start_zone == 7 & end_zone == 2) | (start_zone == 8 & end_zone == 3) | (start_zone == 9 & end_zone == 4) ~ 4.5,
      (start_zone == 4 & end_zone == 1) | ((start_zone == 3 | is.na(start_zone)) & end_zone == 6) | (start_zone == 2 & end_zone == 5) | (start_zone == 1 & end_zone == 4) | (start_zone == 6 & end_zone == 3) | (start_zone == 5 & end_zone == 2) ~ 7.5,
      (start_zone %in% c(2,4) & end_zone == 3) | ((start_zone == 3 | is.na(start_zone)) & end_zone %in% c(2, 4)) ~ sqrt(1.5^2 + 3^2),
      (start_zone %in% c(2,4) & (end_zone == 8 | is.na(end_zone))) | (start_zone == 3 & end_zone %in% c(7, 9)) | (start_zone %in% c(7,9) & end_zone == 3) | (start_zone == 8 & end_zone %in% c(2,4)) ~ sqrt(4.5^2 + 3^2),
      (start_zone %in% c(2,4) & end_zone == 6) | ((start_zone == 3 | is.na(start_zone)) & end_zone %in% c(1, 5)) | (start_zone == 6 & end_zone %in% c(2,4)) | (start_zone %in% c(1,5) & end_zone == 3) ~ sqrt(7.5^2 + 3^2),
      (start_zone == 4 & end_zone == 4) | (start_zone == 2 & end_zone == 2) ~ sqrt(1.5^2 + 6^2),
      (start_zone == 4 & end_zone == 7) | (start_zone == 2 & end_zone == 9) | (start_zone == 7 & end_zone == 4) | (start_zone == 9 & end_zone == 2) ~ sqrt(4.5^2 + 6^2),
      (start_zone == 4 & end_zone == 5) | (start_zone == 2 & end_zone == 1) | (start_zone == 5 & end_zone == 4) | (start_zone == 1 & end_zone == 2) ~ sqrt(7.5^2 + 6^2),
      (start_zone == 7 & end_zone == 9) | (start_zone == 8 & (end_zone == 8 | is.na(end_zone))) | (start_zone == 9 & end_zone == 7) ~ 7.5,
      (start_zone %in% c(7,9) & (end_zone == 8 | is.na(end_zone))) | (start_zone == 8 & end_zone %in% c(7,9)) ~ sqrt(7.5^2 + 3^2),
      (start_zone == 7 & end_zone == 7) | (start_zone == 9 & end_zone == 9) ~ sqrt(7.5^2 + 6^2),
      (start_zone == 7 & end_zone == 1) | (start_zone == 8 & end_zone == 6) | (start_zone == 9 & end_zone == 5) | (start_zone == 1 & end_zone == 7) | (start_zone == 6 & (end_zone == 8 | is.na(end_zone))) | (start_zone == 5 & end_zone == 9) ~ 10.5,
      (start_zone %in% c(7,9) & end_zone == 6) | (start_zone == 8 & end_zone %in% c(1,5)) | (start_zone == 6 & end_zone %in% c(7,9)) | (start_zone %in% c(1,5) & (end_zone == 8 | is.na(end_zone))) ~ sqrt(10.5^2 + 3^2),
      (start_zone == 7 & end_zone == 5) | (start_zone == 9 & end_zone == 1) | (start_zone == 5 & end_zone == 7) | (start_zone == 1 & end_zone == 9) ~ sqrt(10.5^2 + 6^2),
      (start_zone == 5 & end_zone == 1) | (start_zone == 6 & end_zone == 6) | (start_zone == 1 & end_zone == 5) ~ 13.5,
      (start_zone %in% c(1,5) & end_zone == 6) | (start_zone == 6 & end_zone %in% c(1,5)) ~ sqrt(13.5^2 + 3^2),
      (start_zone == 5 & end_zone == 5) | (start_zone == 1 & end_zone == 1) ~ sqrt(13.5^2 + 6^2)
      
    )  # end case_when
    ) # end mutate
  )
}
