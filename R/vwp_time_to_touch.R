vwp_time_to_touch <- function(plays){
  return(
    plays %>% mutate(time_to_touch = c(0, diff(video_time)) # time from previous touch
                     # We can add a time_to_touch_categorical here if binning < 1 second vs 2+ seconds
    ) # end mutate
  )  # end return
}
