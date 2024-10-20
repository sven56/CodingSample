# ----------------------------------------------------------------------------------------------------
# Penalty Index Generation
# ----------------------------------------------------------------------------------------------------
penaltyGen_ind <- function(precision) {
  if (precision == "high") {
    vec = c(0,rbinom(15, 1, 0.5),rep(0,185)) # 10 relevant, 5 irrelevant -> 2/3precision
  } else if (precision == "medium") {
    vec = c(0,rbinom(20, 1, 0.5),rep(0,180)) # 10 relevant, 10 irrelevant -> 1/2 precision
  } else {
    vec = c(0,rbinom(30, 1, 0.5),rep(0,170)) # 10 relevant, 20 irrelevant -> 1/3 precision
  }
  return(vec)
}





# ----------------------------------------------------------------------------------------------------
# Penalty Method 1 (Non-data-driven): Binomial weights -> Input into penaltyVec
# ----------------------------------------------------------------------------------------------------
penaltyGen_binom <- function(vec) {
  output_vec = rep(1,length(vec))
  selected = which(vec == 1)
  output_vec[1] = 0
  output_vec[selected] = 0
  return(output_vec)
}





# ----------------------------------------------------------------------------------------------------
# Penalty Method 2 (Non-data-driven): Random Uniform Weights -> Input into penaltyVec
# ----------------------------------------------------------------------------------------------------
penaltyGen_unif <- function(vec) {
  output_vec = rep(1,length(vec))
  selected = which(vec == 1)
  output_vec[1] = 0
  output_vec[selected] = runif(length(selected),0,1)
  return(output_vec)
}






