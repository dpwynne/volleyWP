#' Extract relevant features for a Volleyball Win Probability Model
#'
#' Subsets a data frame to include only features of interest and remove any duplicate rows (usually games that got added twice)
#' 
#'
#' @param plays a dv_plays object or data frame containing play-by-play data.
#' @param features a character vector containing the names of the variables to be included. If you don't know which ones you want to keep in, use the default.
#' This should work even if your plays object does not contain some of these columns. If you include too few features, you may accidentally delete rows corresponding to different touches.
#' Also, you may not be able to run the full model later as this function is designed for pre-processing. 
#'
#' @return The same data frame, with duplicate rows and extraneous columns removed
#'
#'
#' @export


vwp_extract_relevant_features <- function(plays, features = c("team, opponent, match_id, point_id, video_time, player_name,
                                                             skill, skill_type, evaluation_code, evaluation, attack_code, set_code,
                                                             start_zone, end_zone, end_subzone, skill_subtype, num_players,
                                                             home_p2, home_p3, home_p4, visiting_p2, visiting_p3, visiting_p4,
                                                             home_team, visiting_team, point_won_by, serving_team, phase")){
  suppressWarnings( # if something is not present in plays then we will get a warning, this suppresses the warning
    plays %>%
    select(one_of(features)) %>%
    distinct()
  )  
}