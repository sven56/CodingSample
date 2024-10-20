# ----------------------------------------------------------------------------------------------------
# ALGORITHM 0: Algorithm 2 vanilla style (for Control)
# ----------------------------------------------------------------------------------------------------
Alg2 <- function(Y, X, homoscedastic = TRUE, options, exemptIndices = 0 ) {
  
  # Initialization
  lambdaSims = options$lambdaSims; sigmaSims = options$sigmaSims; gamma = options$gamma; c_lambda = options$c_lambda; psi = options$psi
  n = dim(X)[1]
  k = 1; tol = TRUE
  
  # Residual initialization: Estimation of sigma^hat_0
  lambda = bigLambda(X, homoscedastic = TRUE, lambdaSims, gamma = gamma, c_lambda = c_lambda)
  hat_error = init_values(X,Y, intercept = FALSE)$residuals
  
  # Setting the penalty, check HDM documentation. 
  # Homosced has penalty term sqrt(E[x_{i,j}^2]). Heterosced has penalty term sqrt(E[x_{i,j}^2 * eps_i^2])
  if (homoscedastic) {
    penalty = sqrt(colMeans(X^2))
  } else {
    penalty = sqrt(colMeans(X^2 * hat_error^2))
  }
  
  # Setting lambda vector
  lambda_vec = psi * 2 * c_lambda * lambda * abs(penalty)
  
  # Penalization removal
  if (sum(exemptIndices != 0) > 0) { lambda_vec[exemptIndices] = 0 }
  
  # Setting up tolerance condition
  tol_val = sqrt(var(hat_error))
  
  # Estimating the sigma_hat using Post-Lasso iteration (Belloni et al., 2011)
  while (k < sigmaSims & tol == TRUE) {
    #print(k)
    tol_val_old = tol_val
    
    # Step 1: Post-Lasso estimation
    beta_hat = LassoShooting.fit(X, Y, lambda_vec)$coefficients
    post_estimator = PostEstimator(Y, X, beta_hat, 0)
    post_beta = post_estimator$betatilde; post_s = post_estimator$stilde
    
    # Step 2: Update lambda vector. i) Estimate sigma^2 / E[eps^2], ii) Correct penalty based on new iteration, iii) Update lambda
    hat_error = as.vector((Y - X%*%post_beta) * sqrt(n/(n-post_s)))
    if (homoscedastic) {
      penalty = sqrt(colMeans(X^2))
    } else {
      penalty = sqrt(colMeans(X^2 * hat_error^2))
    }
    lambda_vec =  2 * c_lambda * lambda * abs(penalty)
    
    # Penalization removal
    if (sum(exemptIndices != 0) > 0) { lambda_vec[exemptIndices] = 0 }
    
    # Tolerance
    tol_val = sqrt(var(hat_error))
    if (abs(tol_val-tol_val_old) < 1e-4) {
      tol = FALSE
    }
    k = k + 1
  }
  
  # Final LASSO iteration
  final_shooting = LassoShooting.fit(X, Y, lambda_vec)$coefficients
  beta_final = final_shooting
  beta_indices = which(final_shooting > 0)
  
  
  return(list(beta = beta_final, ind = beta_indices))
}





# ----------------------------------------------------------------------------------------------------
# ALGORITHM 1: Algorithm 2 w/ penalty vector
# ----------------------------------------------------------------------------------------------------
Alg2_pen <- function(Y, X, homoscedastic = TRUE, options, penaltyVec ) {
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### INITIALIZATION
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Parameter initialization
  lambdaSims = options$lambdaSims; sigmaSims = options$sigmaSims; gamma = options$gamma; c_lambda = options$c_lambda; psi = options$psi
  n = dim(X)[1]; #p = dim(X)[2]
  k = 1; tol = TRUE
  
  # Residual initialization: Estimation of sigma^hat_0
  lambda = bigLambda(X, homoscedastic = TRUE, lambdaSims, gamma = gamma, c_lambda = c_lambda)
  
  # Initial LS estimator for eps^hat_0
  hat_error = init_values(X,Y, intercept = FALSE)$residuals
  
  # Setting the penalty, check HDM documentation. Homosced has penalty term sqrt(E[x_{i,j}^2]). Heterosced has penalty term sqrt(E[x_{i,j}^2 * eps_i^2])
  if (homoscedastic) {
    penalty = sqrt(colMeans(X^2))
  } else {
    penalty = sqrt(colMeans(X^2 * hat_error^2))
  }
  
  # Setting lambda vector
  lambda_vec = psi * 2 * c_lambda * lambda * abs(penalty)
  
  # Penalization removal, addition of my paper!
  lambda_vec = lambda_vec * penaltyVec
  
  # Setting up tolerance condition
  tol_val = sqrt(var(hat_error))
  
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### ESTIMATION: Estimating the sigma_hat using Post-Lasso iteration (Belloni et al., 2011). Under two tolerances: i) k < val1 & ii) norm of difference penalty > val2
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  while (k < sigmaSims & tol == TRUE) {
    #print(k)
    tol_val_old = tol_val
    
    # Step 1: Post-Lasso estimation
    beta_hat = LassoShooting.fit(X, Y, lambda_vec)$coefficients
    post_estimator = PostEstimator(Y, X, beta_hat, 0)
    post_beta = post_estimator$betatilde; post_s = post_estimator$stilde
    
    # Step 2: Update lambda vector. i) Estimate sigma^2 / E[eps^2], ii) Correct penalty based on new iteration, iii) Update lambda
    hat_error = as.vector((Y - X%*%post_beta) * sqrt(n/(n-post_s)))
    if (homoscedastic) {
      penalty = sqrt(colMeans(X^2))
    } else {
      penalty = sqrt(colMeans(X^2 * hat_error^2))
    }
    lambda_vec =  2 * c_lambda * lambda * abs(penalty)
    
    # Penalization removal
    lambda_vec = lambda_vec * penaltyVec
    
    # Tolerance condition
    tol_val = sqrt(var(hat_error))
    if (abs(tol_val-tol_val_old) < 1e-4) {
      tol = FALSE
    }
    k = k + 1
  }
  
  # Final LASSO iteration
  final_shooting = LassoShooting.fit(X, Y, lambda_vec)$coefficients
  beta_final = final_shooting
  beta_indices = which(final_shooting > 0)
  
  return(list(beta = beta_final, ind = beta_indices))
}










# ----------------------------------------------------------------------------------------------------
# ALGORITHM 2: Using the Score methodology
# ----------------------------------------------------------------------------------------------------
Alg2_Score <- function(Y, X, homoscedastic = TRUE, options, exemptIndices, penaltyInd ) {
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### INITIALIZATION
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Parameter initialization
  lambdaSims = options$lambdaSims; sigmaSims = options$sigmaSims; gamma = options$gamma; c_lambda = options$c_lambda; psi = options$psi
  n = dim(X)[1]; #p = dim(X)[2]
  k = 1; tol = TRUE
  
  # Residual initialization: Estimation of sigma^hat_0
  lambda = bigLambda(X, homoscedastic = TRUE, lambdaSims, gamma = gamma, c_lambda = c_lambda)
  
  # Initial LS estimator for eps^hat_0
  hat_error = init_values(X,Y, intercept = FALSE)$residuals
  
  # Setting the penalty, check HDM documentation. Homosced has penalty term sqrt(E[x_{i,j}^2]). Heterosced has penalty term sqrt(E[x_{i,j}^2 * eps_i^2])
  if (homoscedastic) {
    penalty = sqrt(colMeans(X^2))
  } else {
    penalty = sqrt(colMeans(X^2 * hat_error^2))
  }
  
  # Setting lambda vector
  lambda_vec = psi * 2 * c_lambda * lambda * abs(penalty)
  
  # Penalization removal, addition of my paper!
  # lambda_vec = lambda_vec * penaltyVec
  if (sum(exemptIndices != 0) > 0) { lambda_vec[exemptIndices] = 0 }

  # Score
  range = lin_map(abs(colMeans(X[,penaltyInd] * hat_error)))
  lambda_vec[penaltyInd] = range * lambda_vec[penaltyInd]
  
  # Setting up tolerance condition
  tol_val = sqrt(var(hat_error))
  

  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### ESTIMATION: Estimating the sigma_hat using Post-Lasso iteration (Belloni et al., 2011). Under two tolerances: i) k < val1 & ii) norm of difference penalty > val2
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  while (k < sigmaSims & tol == TRUE) {
    tol_val_old = tol_val
    
    # Step 1: Post-Lasso estimation
    beta_hat = LassoShooting.fit(X, Y, lambda_vec)$coefficients
    post_estimator = PostEstimator(Y, X, beta_hat, 0)
    post_beta = post_estimator$betatilde; post_s = post_estimator$stilde
    
    # Step 2: Update lambda vector. i) Estimate sigma^2 / E[eps^2], ii) Correct penalty based on new iteration, iii) Update lambda
    hat_error = (Y - X%*%post_beta) * sqrt(n/(n-post_s))
    if (homoscedastic) {
      penalty = sqrt(colMeans(X^2))
    } else {
      penalty = sqrt(colMeans(X^2 * as.vector(hat_error^2)))
    }
    lambda_vec =  2 * c_lambda * lambda * abs(penalty)
    
    # Penalization removal
    if (sum(exemptIndices != 0) > 0) { lambda_vec[exemptIndices] = 0 }
    range = lin_map(abs(colMeans(X[,penaltyInd] * as.vector(hat_error))))
    lambda_vec[penaltyInd] = range * lambda_vec[penaltyInd]
    
    # Tolerance condition
    tol_val = sqrt(var(hat_error))
    if (abs(tol_val-tol_val_old) < 1e-4) {
      tol = FALSE
    }
    k = k + 1
  }
  
  # Final LASSO iteration
  final_shooting = LassoShooting.fit(X, Y, lambda_vec)$coefficients
  beta_final = final_shooting
  beta_indices = which(final_shooting > 0)
  
  return(list(beta = beta_final, ind = beta_indices))
}










# ----------------------------------------------------------------------------------------------------
# ALGORITHM 3: Adapative / Two-stage
# ----------------------------------------------------------------------------------------------------
Alg2_adaptive <- function(Y, X, homoscedastic = TRUE, options, exemptIndices, penaltyInd ) {
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### INITIALIZATION
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Parameter initialization
  lambdaSims = options$lambdaSims; sigmaSims = options$sigmaSims; gamma = options$gamma; c_lambda = options$c_lambda; psi = options$psi
  n = dim(X)[1]; #p = dim(X)[2]
  k = 1; tol = TRUE
  
  # Residual initialization: Estimation of sigma^hat_0
  lambda = bigLambda(X, homoscedastic = TRUE, lambdaSims, gamma = gamma, c_lambda = c_lambda)
  
  # Initial LS estimator for eps^hat_0
  hat_error = init_values(X,Y, intercept = FALSE)$residuals
  
  # Setting the penalty, check HDM documentation. Homosced has penalty term sqrt(E[x_{i,j}^2]). Heterosced has penalty term sqrt(E[x_{i,j}^2 * eps_i^2])
  if (homoscedastic) {
    penalty = sqrt(colMeans(X^2))
  } else {
    penalty = sqrt(colMeans(X^2 * hat_error^2))
  }
  
  # Setting lambda vector
  lambda_vec = psi * 2 * c_lambda * lambda * abs(penalty)
  
  # Penalization removal, addition of my paper!
  # lambda_vec = lambda_vec * penaltyVec
  if (sum(exemptIndices != 0) > 0) { lambda_vec[exemptIndices] = 0 }
  ols = lm(Y ~ X[,penaltyInd])
  range = lin_map((coef(ols)[-1])^(-1))
  lambda_vec[penaltyInd] = range * lambda_vec[penaltyInd]
  
  # Setting up tolerance condition
  tol_val = sqrt(var(hat_error))
  
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### ESTIMATION: Estimating the sigma_hat using Post-Lasso iteration (Belloni et al., 2011). Under two tolerances: i) k < val1 & ii) norm of difference penalty > val2
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  while (k < sigmaSims & tol == TRUE) {
    tol_val_old = tol_val
    
    # Step 1: Post-Lasso estimation
    beta_hat = LassoShooting.fit(X, Y, lambda_vec)$coefficients
    post_estimator = PostEstimator(Y, X, beta_hat, 0)
    post_beta = post_estimator$betatilde; post_s = post_estimator$stilde
    
    # Step 2: Update lambda vector. i) Estimate sigma^2 / E[eps^2], ii) Correct penalty based on new iteration, iii) Update lambda
    hat_error = (Y - X%*%post_beta) * sqrt(n/(n-post_s))
    if (homoscedastic) {
      penalty = sqrt(colMeans(X^2))
    } else {
      penalty = sqrt(colMeans(X^2 * as.vector(hat_error^2)))
    }
    lambda_vec =  2 * c_lambda * lambda * abs(penalty)
    
    # Penalization removal - Adaptive
    if (sum(exemptIndices != 0) > 0) { lambda_vec[exemptIndices] = 0 }
    ols = lm(Y ~ X[,penaltyInd])
    range = lin_map((coef(ols)[-1])^(-1))
    lambda_vec[penaltyInd] = range * lambda_vec[penaltyInd]
    
    # Tolerance condition
    tol_val = sqrt(var(hat_error))
    if (abs(tol_val-tol_val_old) < 1e-4) {
      tol = FALSE
    }
    k = k + 1
  }
  
  # Final LASSO iteration
  final_shooting = LassoShooting.fit(X, Y, lambda_vec)$coefficients
  beta_final = final_shooting
  beta_indices = which(final_shooting > 0)
  
  return(list(beta = beta_final, ind = beta_indices))
}
