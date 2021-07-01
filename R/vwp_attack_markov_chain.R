#' Attack Markov Chain
#'
#' Estimates transition matrix between attack types
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param system whether to break down by in vs. out-of-system
#'
#' @return A data frame indicating the modeled point-win-probability of an in-system, out-of-system, or no-set attack
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom stats xtabs
#' @importFrom tibble tibble
#' @importFrom dplyr ungroup
#' @importFrom rlang .data
#'

vwp_attack_markov_chain <- function(plays, system = TRUE){
  
  # Step 1: Filter only to attacks
  attacks <- plays %>% filter(skill == "Attack")
  
  # Step 2: grouped mutate for team
  next_attack <- attacks %>% group_by(match_id, point_id) %>%
    mutate(next_attack_team = case_when(
      lead(team) == serving_team ~ "S", # serving team
      lead(team) != serving_team ~ "R", # receiving team
      TRUE ~ "E" # if no leading team then end of point
    ), # end case_when for next attack team
    current_attack_team = case_when(
      team == serving_team ~ "S",
      team != serving_team ~ "R"
    ) # end case_when for current attack team
    ) # end mutate #1
  
  if(system & "system" %in% names(next_attack)){  # if breaking down by system and we have a system variable added
    next_attack <- next_attack %>% mutate(  # should still be a grouped mutate I think
      next_attack_system = case_when(
        !is.na(lead(team)) ~ str_sub(lead(system), 1, 1), # I, N, or O depending on system of next attack
        is.na(lead(team)) & team == point_won_by ~ "W", # won point before next attack
        is.na(lead(team)) & team != point_won_by ~ "L" # lost point before next attack
      ), # end case_when for next attack
      current_attack_system = str_sub(system, 1, 1)
    ) # end mutate #2 if we have a system variable
  } else {
    next_attack <- next_attack %>% mutate(
      next_attack_system = case_when(
        !is.na(lead(team)) ~ "U", # unknown
        is.na(lead(team)) & team == point_won_by ~ "W", # won point before next attack
        is.na(lead(team)) & team != point_won_by ~ "L" # lost point before next attack
      ), # end case_when
      current_attack_system = "U"
    ) # end mutate #2 if we have no system variable
  }
  
  next_attack <- next_attack %>% mutate(
    current_attack_team_system = str_c(current_attack_system, current_attack_team), # combine system and team of current attack
    next_attack_team_system = case_when(
      (current_attack_team == "S" & next_attack_system == "W") | (current_attack_team == "R" & next_attack_system == "L") ~ "WS", # receiving team loses point = serving team wins point
      (current_attack_team == "R" & next_attack_system == "W") | (current_attack_team == "S" & next_attack_system == "L") ~ "WR", # serving team loses point = receiving team wins point
      TRUE ~ str_c(next_attack_system, next_attack_team) # combine team and system of next attack
    ) # end case_When
  ) %>% # end mutate #3
    ungroup()
  
  ## NOW BUILD THE TABLE
  
  attack_mc <- xtabs(~ current_attack_team_system + next_attack_team_system, data = next_attack) %>%
    prop.table(margin = 1) %>%  # every row should sum to 1
    as.matrix()
  
  ## Row names: IR, IS, NR, NS, OR, OS, RS or UR, US if no system
  ## Column names: IR, IS, NR, NS, OR, OS, WR, WS or UR, US, WR, WS if no system
  
  nr <- nrow(attack_mc)  # should be either 6 or 2
  nc <- ncol(attack_mc)  # should be either 8 or 4
  R <- attack_mc[, c(nc-1, nc)]
  if (nr > (nc - 2)){
    Q <- matrix(c(attack_mc[, -c(nc-1, nc)], rep(0, nr*(nr - nc + 2))), nrow = nr)
  } else {
    Q <- attack_mc[, -c(nc-1, nc)]
  }
  

  N <- solve(diag(nr) - Q)
  P <- N %*% R  # should be expected PWP for each state
  
  rownames(P) <- rownames(attack_mc)
  
  # Here the matrix will be slightly unbalanced, we can average the R and S versions
  if(system & "system" %in% names(next_attack)){
    PWP <- tibble(
      skill = rep("Attack", 4),
      system = c("In System", "Net Play", "Out of System", "Reception Overpass"),
      PWP = c(mean(c(P[1,1], P[2,2])), mean(c(P[3,1], P[4,2])), mean(c(P[5,1], P[6,2])), if(nrow(P)== 7) P[7,2] else P[8,2])
    )
  } else {
    PWP <- tibble(
      skill = "Attack",
      PWP = mean(P[1,1], P[2,2])
    )
  }
  
  return(PWP)
  
}
