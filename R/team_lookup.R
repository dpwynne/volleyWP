#' Lookup table for NCAA teams
#'
#' Finds likely match(es) for desired volleyball team or conference.
#'     The NCAA, Volleymetrics, and Rich Kern all use a different naming scheme for teams. This function, combined with an appropriate data table,
#' allows the user to find the equivalent names used by all three organizations given a guess at the name used by any organization.
#'     In addition, the default data table used allows the same function to be used to look up all teams in a given conference. 
#'
#' @param team a string containing the name of the team to look up
#' @param team_db a data frame containing the names of teams (and conferences) used by different organizations. For columns containing team names, the organization name should be the column name. 
#' The default data frame is the D1VB_team_names dataset containing NCAA, Volleymetrics, and Rich Kern team names for 335 D1 women's volleyball teams.
#'
#' @return  A subset of `team_db` containing possible matches for the team of interest
#'
#' @importFrom stringr str_c
#' @importFrom stringr str_which
#' @importFrom tibble as_tibble

#' @export

team_lookup <- function(team, team_db = D1VB_team_names){
  
  team_start <- paste0("^", team)
  team_end <- paste0(team, "$")
  team_regex <- str_c(team_start, team_end, sep = "|")
  poss_rows <- vector("list", length = ncol(team_db))
  
  for(i in 1:length(poss_rows)){
    poss_rows[[i]] <- str_which(team_db[,i, drop = TRUE], team_regex)
  }
  
  poss_rows <- unique(unlist(poss_rows))
  
  if(length(poss_rows) == 0){
    print(paste0("No teams with name ", team, " found in database."), quote = FALSE)
    invisible(NA_character_)
  } else {
    team_output <- as_tibble(team_db[poss_rows,])
    print("The following team(s) may be the correct match:", quote = FALSE)
    print(team_output)
    invisible(team_output)
  }
}
