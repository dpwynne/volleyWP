#' Assign time from previous touch
#'
#' Computes the time from the previous touch until the next touch.
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same data frame, with a "time_to_touch" variable indicating how many seconds (on video time) since the previous touch
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @export


vwp_time_to_touch <- function(plays){
  return(
    plays %>% mutate(time_to_touch = c(0, diff(video_time)) # time from previous touch
                     # We can add a time_to_touch_categorical here if binning < 1 second vs 2+ seconds
    ) # end mutate
  )  # end return
}
