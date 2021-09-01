vwp_zone_to_xy <- function(zone){
  
  x <- case_when(
    zone %in% c(4, 5, 7) ~ 1,
    zone %in% c(3, 6, 8) | is.na(zone) ~ 2,
    zone %in% c(1, 2, 9) ~ 3
  )
  
  y <- case_when(
    zone %in% c(1, 5, 6) ~ 1,
    zone %in% c(2, 3, 4) ~ 3,
    zone %in% c(7, 8, 9) | is.na(zone) ~ 2
  )
  
  return(list(x = x, y = y))
}
