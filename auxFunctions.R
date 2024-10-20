# ----------------------------------------------------------------------------------------------------
# Creating big Lambda, based on HDM documentation (p. 3) and Belloni et al. (2011)
# ----------------------------------------------------------------------------------------------------
bigLambda <- function(X, homoscedastic, lambdaSims, gamma, c_lambda) {
  
  # Initialization
  n = dim(X)[1]
  p = dim(X)[2]
  lambda_log = numeric(lambdaSims)
  
  # Normalization to column var = 1. Unnecessary, but keep in for now..!
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
# Creating the simple X-independent big Lambda
# ----------------------------------------------------------------------------------------------------
bigLambda_simple <- function(X, gamma, c_lambda) {
  n = dim(X)[1]
  p = dim(X)[2]
  lambda = 2 * c_lambda * sqrt(n) * qnorm(1 - gamma / (2 * p))
  return(lambda)
}





# ----------------------------------------------------------------------------------------------------
# Create matrix
# ----------------------------------------------------------------------------------------------------
createDesign <- function (n, p, rho, alpha0, R21, R22, equalCoeffs, homoscedastic, eta0, beta0) {
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### Designing the matrices
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  
  # Design matrix
  S = toeplitz(rho^abs(0:(p-1)))
  SC = chol(S)
  X = matrix(rnorm(n*p), nrow = n, ncol = p) %*% SC
  
  # Normalize X-matrix
  norm <- apply(X, 2, function(X) mean(X^2))
  X <- t(t(X)/sqrt(norm))
  
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### R^2 corrections, based on Belloni et al. (2014) Supplementary material
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # If equalCoeffs = TRUE -> eta0 = beta0. If not, still have to program
  if (equalCoeffs){ #
    c1 = sqrt (  R21 / ((1-R21) * eta0%*%S%*%eta0))
    
    a = (1-R22)*beta0%*%S%*%beta0
    b = 2*(1-R22)*alpha0*c1*eta0%*%S%*%beta0
    c = (1-R22) * alpha0^2 * c1^2 * eta0 %*% S %*% eta0 - R22*(alpha0^2 * 1 + 1)
    c2 = max(Re(polyroot(c(c,b,a))))
    # Equivalent to the above: c_top = -(1-R22)*alpha0*c1*eta0%*%S%*%eta0 + sqrt((1-R22)*R22*eta0%*%S%*%eta0 *(alpha0^2 + 1)); c_bottom = (1-R22)*eta0%*%S%*%eta0
  } 
  
  # Adjusted betas
  eta0_hat = c(c1) * eta0
  beta0_hat = c(c2) * beta0
  
  
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### Error generation and creation
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Error generation for D (homo / heteroscedastic)
  if (homoscedastic) {
    sigma_d = 1
  } else {
    sigma_d_1 = (1 + X%*%eta0)^2
    sigma_d = sqrt(sigma_d_1 / mean(sigma_d_1))
  }
  
var(sigma_d)
  
  # Estimate treatment variable D
  D = X %*% eta0_hat + rnorm(n) * sigma_d
  
  # Error generation for Y (homo / heteroscedastic)
  if (homoscedastic) {
    sigma_y = 1
  } else {
    sigma_y_1 = (1 + alpha0*D + X%*%beta0)^2
    sigma_y = sqrt(sigma_y_1 / mean(sigma_y_1))
  }
  
  
  
  # Estimate outcome variable Y
  Y = cbind(D,X) %*% c(alpha0, beta0_hat) + rnorm(n) * sigma_y
  
  # Temporary R-Squared check
 # summary(lm(Y ~ X[,1:k] %*% beta0[1:k]))
  
  return(list(Y = Y, X = cbind(1,D,X)))
}





# ----------------------------------------------------------------------------------------------------
# MacKinnon and White Heteroscedasticity Consistent Covariance Estimators (STILL TO CITE)
# ----------------------------------------------------------------------------------------------------
se_hetero <- function(Y, D, X_hat, alpha) {
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  ### Partialling out, afterwards the HC3 estimator (jackknife)
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  X_hat = as.matrix(X_hat)
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





# ----------------------------------------------------------------------------------------------------
# Coverage Function, based on specific alpha
# ----------------------------------------------------------------------------------------------------
coverage <- function(bias, se) {
  sims = length(bias)
  absZ = abs( bias / se )
  z = qnorm(0.975)
  coverage_hat = sum(absZ > z) / sims
  return(coverage_hat)
}






# ----------------------------------------------------------------------------------------------------
# Total vs relevant controls metric
# ----------------------------------------------------------------------------------------------------
selectedControls <- function(k, ind) {
  relevant = sum(ind <= k)
  total = length(ind)
  return(list(relevant = relevant, total = total))
}





# ----------------------------------------------------------------------------------------------------
# Linear mapping to a uniform distribution (0,1)
# ----------------------------------------------------------------------------------------------------
lin_map <- function(x){(x-min(x))/(max(x)-min(x))}





# ----------------------------------------------------------------------------------------------------
# Summarizing data from matrices to table
# ----------------------------------------------------------------------------------------------------
matSave <- function(df) {
  # Input df
  # output, 4 averages
  colMeans(all_bias)
  
}












