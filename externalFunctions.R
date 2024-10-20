# ----------------------------------------------------------------------------------------------------
# Init values - Copied from HDM source code
# ----------------------------------------------------------------------------------------------------
init_values <- function(X, y, number = 5, intercept = TRUE) {
  suppressWarnings(corr <- abs(cor(y, X)))
  kx <- dim(X)[2]
  index <- order(corr, decreasing = T)[1:min(number, kx)]
  coefficients <- rep(0, kx)
  if (intercept == TRUE) {
    reg <- lm(y ~ X[, index, drop = FALSE])
    coefficients[index] <- coef(reg)[-1]
  } else {
    reg <- lm(y ~ -1 + X[, index, drop = FALSE])
    coefficients[index] <- coef(reg)
  }
  coefficients[is.na( coefficients)] <- 0
  res <- list(residuals = reg$residuals, coefficients = coefficients)
  return(res)
}





# ----------------------------------------------------------------------------------------------------
# Post Estimator - Re-coded from the authors MATLAB code (Belloni et al. 2014)
# ----------------------------------------------------------------------------------------------------
PostEstimator <- function(Y, X, beta_FS, eps) {
  n <- nrow(X)
  p <- ncol(X)
  
  v <- numeric(p)
  for (j in 1:p) { # Normalize, although can remove because already normalized
    v[j] <- norm(as.matrix(X[, j] / sqrt(n)), "F")
  }
  
  ind <- abs(beta_FS * v) > eps
  if (min(X[, 1]) == max(X[, 1])) { # If first column is constant, also set to TRUE
    ind[1] <- TRUE
  }
  
  XX <- matrix(nrow = n, ncol = 0)
  ind_col <- numeric()
  
  for (j in 1:p) {
    if (ind[j]) {
      XX <- cbind(XX, X[, j])
      ind_col <- c(ind_col, j)
    }
  }
  
  if (ncol(XX) > 1) { # No idea why to include this
    if (sum(abs(XX[, 1] - XX[, 2])) == 0) {
      XX <- XX[, -2]
      ind_col <- ind_col[-2]
    }
  }
  
  K <- length(ind_col)
  betatilde <- numeric(p)
  
  if (K >= n) {
    Minv <- MASS::ginv(crossprod(XX))
  } else if (K > 0) {
    if (qr(XX)$rank < ncol(XX)) {
      warning("hi there")
    }
    Minv <- solve(crossprod(XX))
  }
  
  if (K > 0) {
    betatilde[ind_col] <- Minv %*% t(XX) %*% Y
  }
  
  stilde <- K
  
  
  return(list(betatilde = betatilde, stilde = stilde))
}




# ----------------------------------------------------------------------------------------------------
# LassoShooting, from HDM package
# ----------------------------------------------------------------------------------------------------
LassoShooting.fit <- function (x, y, lambda, control = list(maxIter = 1000, optTol = 10^(-5), 
                                                            zeroThreshold = 10^(-6)), XX = NULL, Xy = NULL, beta.start = NULL) 
{
  n <- dim(x)[1]
  p <- dim(x)[2]
  if (is.null(XX)) 
    (XX <- crossprod(x))
  if (is.null(Xy)) 
    (Xy <- crossprod(x, y))
  if (is.null(beta.start)) {
    beta <- init_values(x, y, intercept = FALSE)$coef
  }
  else {
    beta <- beta.start
  }
  wp <- beta
  m <- 1
  XX2 <- XX * 2
  Xy2 <- Xy * 2
  while (m < control$maxIter) {
    beta_old <- beta
    for (j in 1:p) {
      S0 <- sum(XX2[j, ] * beta) - XX2[j, j] * beta[j] - 
        Xy2[j]
      if (sum(is.na(S0)) >= 1) {
        beta[j] <- 0
        next
      }
      if (S0 > lambda[j]) 
        beta[j] <- (lambda[j] - S0)/XX2[j, j]
      if (S0 < -1 * lambda[j]) 
        beta[j] <- (-1 * lambda[j] - S0)/XX2[j, j]
      if (abs(S0) <= lambda[j]) 
        beta[j] <- 0
    }
    wp <- cbind(wp, beta)
    if (sum(abs(beta - beta_old), na.rm = TRUE) < control$optTol) {
      break
    }
    m <- m + 1
  }
  w <- beta
  w[abs(w) < control$zeroThreshold] <- 0
  return(list(coefficients = w, coef.list = wp, num.it = m))
}

