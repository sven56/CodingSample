theMachine <- function(Y, X, options, amelioration, homoscedastic) {
  ### Initialization
  bias_m = numeric(6)
  se_m = numeric(6)
  coverage_m = numeric(6)
  relevant_m = numeric(6)
  total_m = numeric(6)
  R2_first = numeric(6)
  R2_second = numeric(6)
  detected = numeric(dim(X)[2]-1)
  detected[amelioration] = 1
  detected_ind = amelioration
  k = length(amelioration)
  
  
  ### METHOD 0: Control
  # Function
  reg1_control = Alg2(X[,2], X[,-2], homoscedastic = homoscedastic, options, exemptIndices = 1 )
  ind1_control = which(reg1_control$beta[-1] > 0) + 2
  reg2_control = Alg2(Y, X[,-2], homoscedastic = homoscedastic, options, exemptIndices = 1 )
  ind2_control = which(reg2_control$beta[-1] > 0) + 2
  ind_c_control = c(1,2,union(ind1_control, ind2_control))
  ols_control = lm(Y ~ -1 + X[,ind_c_control])
  
  # Metrics
  ind_hat_control = ind_c_control[which(ind_c_control != 2)]
  se_control = se_hetero(Y, X[,2], X[,ind_hat_control], coef(ols_control)[2])
  selected_control = selectedControls(k, union(ind1_control, ind2_control) - 2)
  
  # Reporting
  bias_m[1] = coef(ols_control)[2]
  se_m[1] = se_control
  coverage_m[1] = abs(bias_m[1] / se_m[1]) > qnorm(0.975)
  relevant_m[1] = selected_control$relevant
  total_m[1] = selected_control$total
# 
#   # R^2 reporting
#   if (!is_empty(ind1_control)) {
#     R2_first[1] = summary(lm(X[,2] ~ 1 + X[,ind1_control]))$r.squared
#   }
#   if (!is_empty(ind2_control)) {
#     R2_second[1] = summary(lm(Y ~ 1 + X[,ind2_control]))$r.squared
#   }

  
  
  

  ### METHOD 1: Control + Amelioration
  # Function
  ind_c_controlAmel = union(ind_c_control, detected_ind+1)
  ols_controlAmel = lm(Y ~ -1 + X[,ind_c_controlAmel])
  
  # Metrics
  ind_hat_controlAmel = ind_c_controlAmel[which(ind_c_controlAmel != 2)]
  se_controlAmel = se_hetero(Y, X[,2], X[,ind_hat_controlAmel], coef(ols_controlAmel)[2])
  selected_controlAmel = selectedControls(k, ind_c_controlAmel[-c(1:2)] - 2)
  
  
  # Reporting
  bias_m[2] = coef(ols_controlAmel)[2]
  se_m[2] = se_controlAmel
  coverage_m[2] = abs(bias_m[2] / se_m[2]) > qnorm(0.975)
  relevant_m[2] = selected_controlAmel$relevant
  total_m[2] = selected_controlAmel$total
  

  
  
  
  
  
  ### METHOD 2 : Binomial
  # Function
  penaltyBinom = penaltyGen_binom(detected)
  reg1_binom = Alg2_pen(X[,2], X[,-2], homoscedastic = homoscedastic, options, penaltyBinom)
  ind1_binom = which(reg1_binom$beta[-1] > 0) + 2
  reg2_binom = Alg2_pen(Y, X[,-2], homoscedastic = homoscedastic, options, penaltyBinom)
  ind2_binom = which(reg2_binom$beta[-1] > 0) + 2
  ind_c_binom = c(1,2,union(ind1_binom, ind2_binom))
  ols_binom = lm(Y ~ -1 + X[,ind_c_binom])
  
  # Metrics
  ind_hat_binom = ind_c_binom[which(ind_c_binom != 2)]
  se_binom = se_hetero(Y, X[,2], X[,ind_hat_binom], coef(ols_binom)[2])
  selected_binom = selectedControls(k, union(ind1_binom, ind2_binom) - 2)
  
  # Reporting
  bias_m[3] = coef(ols_binom)[2]
  se_m[3] = se_binom
  coverage_m[3] = abs(bias_m[3] / se_m[3]) > qnorm(0.975)
  relevant_m[3] = selected_binom$relevant
  total_m[3] = selected_binom$total
  
  
  
  
  ### METHOD 2 : Uniform
  # Function
  penaltyUnif = penaltyGen_binom(detected)
  penaltyUnif[penaltyUnif == 0] = 0.5
  penaltyUnif[1] = 0
  reg1_unif = Alg2_pen(X[,2], X[,-2], homoscedastic = homoscedastic, options, penaltyUnif)
  ind1_unif = which(reg1_unif$beta[-1] > 0) + 2
  reg2_unif = Alg2_pen(Y, X[,-2], homoscedastic = homoscedastic, options, penaltyUnif)
  ind2_unif = which(reg2_unif$beta[-1] > 0) + 2
  ind_c_unif = c(1,2,union(ind1_unif, ind2_unif))
  ols_unif = lm(Y ~ -1 + X[,ind_c_unif])

  # Metrics
  ind_hat_unif = ind_c_unif[which(ind_c_unif != 2)]
  se_unif = se_hetero(Y, X[,2], X[,ind_hat_unif], coef(ols_unif)[2])
  selected_unif = selectedControls(k, union(ind1_unif, ind2_unif) - 2)
  

  # Reporting
  bias_m[4] = coef(ols_unif)[2]
  se_m[4] = se_unif
  coverage_m[4] = abs(bias_m[4] / se_m[4]) > qnorm(0.975)
  relevant_m[4] = selected_unif$relevant
  total_m[4] = selected_unif$total
  
  
  
  
  
  ### METHOD 3 : two-stage OLS / adaptive
  # Function
  reg1_adap = Alg2_adaptive(X[,2], X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind1_adap = which(reg1_adap$beta[-1] > 0) + 2
  reg2_adap = Alg2_adaptive(Y, X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind2_adap = which(reg2_adap$beta[-1] > 0) + 2
  ind_c_adap = c(1,2,union(ind1_adap, ind2_adap))
  ols_adap = lm(Y ~ -1 + X[,ind_c_adap])
  
  # Metrics
  ind_hat_adap = ind_c_adap[which(ind_c_adap != 2)]
  se_adap = se_hetero(Y, X[,2], X[,ind_hat_adap], coef(ols_adap)[2])
  selected_adap = selectedControls(k, union(ind1_adap, ind2_adap) - 2)
  
  # Reporting
  bias_m[5] = coef(ols_adap)[2]
  se_m[5] = se_adap
  coverage_m[5] = abs(bias_m[5] / se_m[5]) > qnorm(0.975)
  relevant_m[5] = selected_adap$relevant
  total_m[5] = selected_adap$total
  
  
  
  ### METHOD 4: Score
  # Function
  reg1_score = Alg2_Score(X[,2], X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind1_score = which(reg1_score$beta[-1] > 0) + 2
  reg2_score = Alg2_Score(Y, X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind2_score = which(reg2_score$beta[-1] > 0) + 2
  ind_c_score = c(1,2,union(ind1_score, ind2_score))
  ols_score = lm(Y ~ -1 + X[,ind_c_score])
  
  # Metrics
  ind_hat_score = ind_c_score[which(ind_c_score != 2)]
  se_score = se_hetero(Y, X[,2], X[,ind_hat_score], coef(ols_score)[2])
  selected_score = selectedControls(k, union(ind1_score, ind2_score) - 2)
  
  
  # Reporting
  bias_m[6] = coef(ols_score)[2]
  se_m[6] = se_score
  coverage_m[6] = abs(bias_m[6] / se_m[6]) > qnorm(0.975)
  relevant_m[6] = selected_score$relevant
  total_m[6] = selected_score$total
  
  
  
  ### RETURN
  return(list(bias = bias_m, se = se_m, coverage = coverage_m, relevant = relevant_m, total = total_m, R2_first = R2_first, R2_second = R2_second))
  
}
