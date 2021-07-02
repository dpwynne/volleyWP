#' Volleyball Win Probability Multinomial Regression Model
#'
#' Builds a model to predict the next attack from a touch and outputs the coefficient matrix
#' 
#'
#' @param plays a dv_plays object or data frame containing play-by-play data.
#' @param formula a formula whose left-hand side contains the name of the variable in `plays` containing the next attack, and 
#' whose right-hand side contains the features used in the regression
#' @param nmax the maximum number of rows to use when fitting the model. Very large datasets can cause memory issues. If you are 
#' getting error messages related to an inability to allocate memory, set nmax to a smaller number. By default we use 200000.
#' @param seed a seed for reproducibility of randomly sampling very large datasets. 
#'
#' @return A coefficient matrix for a multinomial regression model, where each column represents one possible next attack and 
#' each row represents the contribution to the odds ratio of that outcome vs. the team winning the point without any more attacks made. The matrix has dimensions
#' p x 8, where p is the number of features (including dummary variables) in the model matrix.
#'
#' @importFrom dplyr sample_n
#' @importFrom stringr str_remove_all
#' @importFrom VGAM vglm
#' @importFrom VGAM multinomial
#'
#' @export

vwp_multinomial_regression <- function(plays, formula, nmax = 200000, seed = 100){
  
  if(nrow(plays) > nmax){
    set.seed(seed)
    plays <- sample_n(plays, nmax)
  }  # if we have over nmax rows then we might not have enough memory
  # Guess that nmax = 200000 rows randomly sampled will be enough to build a reasonably accurate model, might be able to get away with less
  
  vb_model <- vglm(formula, data = plays, family = "multinomial", refLevel = "TW") # always TW is the reference level
  
  coef_names <- str_remove_all(names(coef(vb_model)), ":[0-9]") %>% unique() # get the row names
  coef_names[1] <- "Intercept"
  
  coef_matrix <- cbind(matrix(coef(vb_model), ncol = 7, byrow = T), rep(0, length(coef_names)))
  
  rownames(coef_matrix) <- coef_names
  colnames(coef_matrix) <- c("OI", "ON", "OO", "OW", "TI", "TN", "TO", "TW")
  
  return(coef_matrix)
}
