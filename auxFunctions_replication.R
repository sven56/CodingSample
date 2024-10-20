# ----------------------------------------------------------------------------------------------------
# Create the beta and eta vectors to be used in DGP. Taken from Belloni et al. (2014)
# ----------------------------------------------------------------------------------------------------
modelGeneration <- function(beta0first, beta0second, RSq) {
  
  p = length(beta0first)
  S = toeplitz(rho^abs(0:(p-1)))
  
  c1 = sqrt(RSq / (  (1-RSq)*beta0first%*%S%*%beta0first ))
  beta0first_hat = c(c1) * beta0first
  
  c2 = sqrt(RSq / (  (1-RSq)*beta0second%*%S%*%beta0second ))
  beta0second_hat = c(c2) * beta0second
  
  return(list(beta0first = beta0first_hat, beta0second = beta0second_hat))
}





# ----------------------------------------------------------------------------------------------------
# Creating starting matrices to be used in MC: Creation of X & Estimation of D and Y
# Output: Y = Y [n*1], X = [1 D X] [n*(p+2)]
# ----------------------------------------------------------------------------------------------------
starting_mats <- function(n, p, rho, alpha0, beta0first, beta0second) {
  
  # Simulation X-matrix
  S = toeplitz(rho^abs(0:(p-1)))
  SC = chol(S)
  X = matrix(rnorm(n*p), nrow = n, ncol = p) %*% SC
  
  norm <- apply(X, 2, function(X) mean(X^2))
  X <- t(t(X)/sqrt(norm))
  
  # Estimate treatment D
  D = X %*% beta0first + rnorm(n)
  
  # Estimate Y
  Y = cbind(D,X) %*% c(alpha0, beta0second) + rnorm(n)
  
  return(list(Y = Y, X = cbind(1,D,X)))
}





# ----------------------------------------------------------------------------------------------------
# Creating starting matrices to be used in MC: Creation of X & Estimation of D and Y
# Output: Y = Y [n*1], X = [1 D X] [n*(p+2)]
# ----------------------------------------------------------------------------------------------------
starting_mats_2 <- function(n, p, rho, alpha0, beta0first, beta0second) {
  
  # Simulation X-matrix
  S = toeplitz(rho^abs(0:(p-1)))
  SC = chol(S)
  X = matrix(rnorm(n*p), nrow = n, ncol = p) %*% SC
  
  # St. Dev First stage
  stdfirst = abs(1 + X%*%beta0first)
  stdfirst = sqrt(stdfirst^2 / mean(stdfirst))
  
  # Estimate treatment D
  D = X %*% beta0first + rnorm(n,0,stdfirst)
  
  # Std. Dev Second stage
  stdsecond = abs(1 + cbind(D,X) %*% c(alpha0, beta0second))
  stdsecond = sqrt(stdsecond^2 / mean(stdsecond))
  
  # Estimate Y
  Y = cbind(D,X) %*% c(alpha0, beta0second) + rnorm(n,0,stdsecond)
  
  return(list(Y = Y, X = cbind(1,D,X)))
}





# ----------------------------------------------------------------------------------------------------
# Creating big Lambda, based on HDM documentation (p. 3) and Belloni et al. (2011)
# ----------------------------------------------------------------------------------------------------
bigLambda <- function(Y, X, homoscedastic, lambdaSims, gamma, c_lambda) {
  
  # Initialization
  n = dim(X)[1]
  p = dim(X)[2]
  lambda_log = numeric(lambdaSims)
  
  # Normalization to column var = 1 
  norm <- apply(X, 2, function(X) mean(X^2))
  xNorm <- t(t(X)/sqrt(norm))
  
  # Homoscedastic simulations. So far the only implementation, look at "Multiplier bootstrap procedure" for heteroscedastic.
  if (homoscedastic) { 
    
    # Simulation
    for (i in 1:lambdaSims) {
      g <- matrix(rep(rnorm(n), each = p), ncol = p, byrow = TRUE)
      lambda_log[i] <- max(colMeans(xNorm * g))
    }
    
    # Selecting (1-gamma)th quantile
    lambda = n * quantile(lambda_log, 1-gamma)
  }
  
  return(lambda)
}





# ----------------------------------------------------------------------------------------------------
# Post-LASSO algorithm: Finds the optimal lambda_vec s.t. sigma is minimized
# ----------------------------------------------------------------------------------------------------
Alg2 <- function(Y, X, homoscedastic = TRUE, options, exemptIndices = 0 ) {
  
  # Initialization
  lambdaSims = options$lambdaSims; sigmaSims = options$sigmaSims; gamma = options$gamma; c_lambda = options$c_lambda; psi = options$psi
  n = dim(X)[1]
  k = 1; tol = TRUE
  
  # Residual initialization: Estimation of sigma^hat_0
  lambda = bigLambda(Y, X, homoscedastic = TRUE, lambdaSims, gamma = 0.05, c_lambda = 1.1)
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
  
  # Estimating the sigma_hat using Post-Lasso iteration (Belloni et al., 2011)
  
  while (k < sigmaSims & tol == TRUE) {
    old_penalty = penalty
    #print(k)
    
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
    
    # Tolerance
    if (norm(as.matrix(old_penalty) - as.matrix(penalty), "F") < 1e-4) {
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
# Post-LASSO algorithm w/ customized penalty
# ----------------------------------------------------------------------------------------------------
Alg2_pen <- function(Y, X, homoscedastic = TRUE, options, penaltyVec ) {
  
  # Initialization
  lambdaSims = options$lambdaSims; sigmaSims = options$sigmaSims; gamma = options$gamma; c_lambda = options$c_lambda; psi = options$psi
  n = dim(X)[1]
  k = 1; tol = TRUE
  
  # Residual initialization: Estimation of sigma^hat_0
  lambda = bigLambda(Y, X, homoscedastic = TRUE, lambdaSims, gamma = 0.05, c_lambda = 1.1)
  hat_error = init_values(X,Y, intercept = FALSE)$residuals
  
  # Setting the penalty, check HDM documentation. 
  # Homosced has penalty term sqrt(E[x_{i,j}^2]). Heterosced has penalty term sqrt(E[x_{i,j}^2 * eps_i^2])
  if (homoscedastic) {
    penalty = sqrt(colMeans(X^2))
  } else {
    penalty = sqrt(colMeans(X^2 * as.vector(hat_error^2)))
  }
  
  # Setting lambda vector
  lambda_vec = psi * 2 * c_lambda * lambda * abs(penalty)
  
  # Penalization
  lambda_vec = lambda_vec * penaltyVec
  
  
  # Estimating the sigma_hat using Post-Lasso iteration (Belloni et al., 2011)
  while (k < sigmaSims & tol == TRUE) {
    
    old_hat_error = hat_error
    
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
    
    # Penalization
    lambda_vec = lambda_vec * penaltyVec
    
    # Tolerance
    if (sqrt(sum(old_hat_error - hat_error)^2) < 1e-4) {
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
# MacKinnon and White Heteroscedasticity Consistent Covariance Estimators (STILL TO CITE)
# ----------------------------------------------------------------------------------------------------
se_hetero <- function(Y, D, X_hat, alpha) {
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### Partialling out, afterwards the HC3 estimator (jackknife)
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  n = dim(X_hat)[1]; s = dim(X_hat)[2]
  
  M = diag(n) - X_hat %*% solve(t(X_hat) %*% X_hat ) %*% t(X_hat)
  Dtilde = M %*% D
  Ytilde = M %*% Y
  
  Etilde = Ytilde - Dtilde * alpha
  
  P = (Dtilde^2) / c(t(Dtilde) %*% Dtilde)
  
  Estar = Etilde / (1-P)
  
  var = (n / (n-s-1))  *  ((n-1)/n)  * ( (t(Dtilde^2) %*% Estar^2) / (t(Dtilde)%*%Dtilde)^2 - (1/n) * ((t(Dtilde)%*%Estar)^2)/(t(Dtilde)%*%Dtilde)^2 )
  sd = sqrt(var)
  
  return(sd)
}



