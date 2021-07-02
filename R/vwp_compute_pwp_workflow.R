# don't export this one, it's a simple helper function
softmax <- function(x){
  exp(x)/sum(exp(x))
}

#' Compute input point-win-probability
#'
#' The input point-win probability for a touch is the modeled probability of winning the point at the exact instant before the ball is contacted.
#' Input point-win probability is estimated from model parameters that are available just prior to the touch (usually location, and information about the previous touch).
#' 
#'
#' @param design_matrix a n x p matrix where n is the number of touches to model and p is the number of features, usually computed from `vwp_model_matrix()`.
#' @param coef_matrix a p x 8 matrix containing the coefficients of a multinomial logistic regression model estimating the odds of each of 8 possible next attack outcomes
#' (OI, ON, OO, OW, TI, TN, TO, TW in that order) where the first letter represents team or opponent result and the second letter represents
#' the type of attack (I = in-system, N = net play, O = out-of-system, W = win/lose point without another attack being made).
#' @param PWP_matrix an 8 x 1 matrix containing the modeled point-win probability values for each of the 8 outcomes in the same order
#' as they are listed in the coef_matrix. The idea is to take a weighted average of the point-win probability values for each type of next attack,
#' with the weights being the probability of each outcome.
#'
#' @return A numeric vector estimating the overall point-win probability of a set of touches.
#'
#'
#' @export
vwp_input_pwp <- function(design_matrix, coef_matrix, PWP_matrix){
  
  if(ncol(design_matrix) != nrow(coef_matrix) | ncol(coef_matrix) != nrow(PWP_matrix)){
    stop("Dimensions do not match; matrix multiplication will not work.")
  }
  
  next_attack_odds <- design_matrix %*% coef_matrix
  next_attack_pwp <- t(apply(next_attack_odds, 1, softmax)) %*% PWP_matrix
  return(next_attack_pwp)
}


#' Create the model matrix for a win probabliity model
#'
#' Multinomial logistic regression models require categorical variables to be recoded as a set of dummy variables. 
#' This function creates the necessary design matrix and coefficient matrix out of the corresponding data frames, then computes the input win probabilities based on those matrices.
#'
#' @param plays a dv_plays object or data frame containing play-by-play data.
#' @param formula a formula object where the left-hand side contains the output variable (should be attack_next) and the right-hand side contains the predictor variables.
#' @param coef_df a 9-column data frame where the first column is the name of the (dummy) variable and the remaining columns contain the corresponding coefficients for the multinomial logistic regression model.
#' Currently, columns 2-9 should be labeled OI, ON, OO, OW, TI, TN, TO, TW (respectively) for best compatibility.
#' @param PWP_matrix an 8 x 1 matrix containing the modeled point-win probability values for each of the 8 outcomes in the same order
#' as they are listed in columns 2-9 of coef_df. The idea is to take a weighted average of the point-win probability values for each type of next attack,
#' with the weights being the probability of each outcome. This argument is passed directly to `vwp_input_pwp()`.
#'
#' @return A numeric vector estimating the overall point-win probability of a set of touches.
#'
#' @importFrom stats model.matrix
#'
#' @export
vwp_model_matrix <- function(plays, formula, coef_df, PWP_matrix){
  design_matrix <- model.matrix(formula, data = plays)
  colnames(design_matrix)[1] <- "Intercept"  # must rename to keep consistent, otherwise get (Intercept) and it's a problem
  coef_matrix <- as.matrix(coef_df[which(coef_df[[1]] %in% colnames(design_matrix)),-1])
  return(vwp_input_pwp(design_matrix, coef_matrix, PWP_matrix))
}


#' Compute point-win probabilities
#'
#' Computes input and output win probabilities for an entire set of plays.
#'    
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param PWP a vector of length 3 giving the estimating point-win probabilities for an average in-system, net play, and out-of-system attack (in that order).
#' @param coef_matrices a list of length 5 giving coefficient estimates for multinomial logistic regression models for attacks off a set ($system_attack),
#' attacks off net play/overpass ($net_attack), digs ($dig), sets ($set), serves ($serve), and freeballs ($freeball).
#' Currently $system_attack, $net_attack, $dig, and $set should be data frames and $serve and $freeball should be single numbers (as there is no model for serve/freeball yet).
#'
#' @return The same data frame, with three new columns (input_pwp, output_pwp, pwp_diff) representing the input and output point-win probabilities for the touch
#' and the difference in point-win probability resulting from the touch
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @export
vwp_compute_pwp <- function(plays, PWP, coef_matrices){
  
  # Step 1: set up all the features we need
  vbID <- plays %>%
    filter(skill %in% c("Serve", "Reception", "Set", "Attack", "Block", "Dig", "Freeball")) %>%  # get rid of substitutions, timeouts, and NA skills
    mutate(id = seq(1, nrow(plays))) %>%  # add an ID column - very important for splitting and recombining 
    vwp_add_cover() %>% # add cover
    vwp_kcode() %>% # add system via kcode
    vwp_next_attack_details() %>% # add details of next attack - for all models
    vwp_blockers() %>% # add number of blockers - for attack model
    vwp_attack_start_zone() %>% # add attack start zone - for attack model
    vwp_set_zone() %>%  # add set zone - for set and attack models
    vwp_block_touched() %>% # add whether block touched attack - for dig model
    vwp_attack_type() %>% # add attack type - for dig model
    vwp_time_to_touch() # add time to touch - for dig model
  
  # Step 2: split by skill
  vb_system_attacks <- vbID %>% filter(skill == "Attack", system %in% c("In System", "Out of System"))
  vb_net_attacks <- vbID %>% filter(skill == "Attack", system %in% c("Reception Overpass", "Net Play"))
  vb_sets <- vbID %>% filter(skill == "Set")
  vb_digs <- vbID %>% filter(skill == "Dig") %>% vwp_zone_distance() # add estimated distance between zones - primarily for dig model
  vb_freeballs <- vbID %>% filter(skill == "Freeball")
  vb_serves <- vbID %>% filter(skill == "Serve")  
  
  # Step 3: get input PWP
  PWP_normal <- matrix(c(1 - PWP[1], 1 - PWP[2], 1 - PWP[3], 0, PWP[1], PWP[2], PWP[3], 1), ncol = 1)
  
  vb_system_attacks$input_pwp <- as.numeric(vwp_model_matrix(vb_system_attacks, formula = attack_next ~ system + blockers + attack_start_zone + set_zone, coef_df = coef_matrices$system_attack, PWP_matrix = PWP_normal))
  vb_net_attacks$input_pwp <- as.numeric(vwp_model_matrix(vb_net_attacks, formula = attack_next ~ system + blockers + attack_start_zone, coef_df = coef_matrices$net_attack, PWP_matrix = PWP_normal))
  vb_digs$input_pwp <- as.numeric(vwp_model_matrix(vb_digs, formula = attack_next ~ block_touched + attack_type + time_to_touch + distance, coef_df = coef_matrices$dig, PWP_matrix = PWP_normal))
  vb_sets$input_pwp <- as.numeric(vwp_model_matrix(vb_sets, formula = attack_next ~ system + set_zone, coef_df = coef_matrices$set, PWP_matrix = PWP_normal))
  
  ## Right now freeball and serve are hard-coded, can change this in a future version
  vb_freeballs$input_pwp <- coef_matrices$freeball
  vb_serves$input_pwp <- coef_matrices$serve
  
  # Step 4: Combine
  vb_skills <- bind_rows(vb_net_attacks, vb_system_attacks, (vb_digs %>% select(-distance)), vb_sets, vb_serves, vb_freeballs) %>% select(id, input_pwp)

  # have to think about this later - people delete columns from their dv files   
  vb_pwp <- left_join(vbID, vb_skills, by = "id") #%>% select(id, team, opponent, match_id, point_id, video_time, player_name,
                                                  #           skill, skill_type, evaluation_code, evaluation, attack_code, set_code,
                                                  #           start_zone, end_zone, end_subzone, skill_subtype, num_players, set_number,
                                                  #           #home_p2, home_p3, home_p4, visiting_p2, visiting_p3, visiting_p4,
                                                  #           team_touch_id, home_team, visiting_team, point_won_by, add_cover, serving_team, phase,
                                                  #           blockers, system, attack_next, attack_start_zone, set_zone, block_touched, attack_type, time_to_touch, input_pwp)
  
  # Step 5: get output point-win-probability
  vb_pwp2 <- vb_pwp %>% filter(!(skill %in% c("Block", "Reception"))) %>%
    select(match_id, point_id, id, team, point_won_by, input_pwp) %>%
    group_by(match_id, point_id) %>%
    mutate(output_pwp = case_when(
      is.na(lead(input_pwp)) & team == point_won_by ~ 1,
      is.na(lead(input_pwp)) & team != point_won_by ~ 0,
      team == lead(team) ~ lead(input_pwp),
      team != lead(team) ~ 1 - lead(input_pwp)
    ))
  
  vb_pwp3 <-vb_pwp %>% left_join(vb_pwp2, by = c("match_id", "point_id", "id", "team", "point_won_by", "input_pwp")) %>%
    mutate(input_pwp = case_when(
      is.na(input_pwp) & team == lag(team) ~ lag(input_pwp),
      is.na(input_pwp) & team != lag(team) ~ 1 - lag(input_pwp),
      TRUE ~ input_pwp
    ),
    output_pwp = case_when(
      is.na(output_pwp) & team == lag(team) ~ lag(output_pwp),
      is.na(output_pwp) & team != lag(team) ~ 1 - lag(output_pwp),
      TRUE ~ output_pwp
    ),
    pwp_diff = output_pwp - input_pwp
    )
  
  
  return(vb_pwp3)
}

