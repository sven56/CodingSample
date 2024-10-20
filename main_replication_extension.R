# ----------------------------------------------------------------------------------------------------
# Pac±ages and Sourcing
# ----------------------------------------------------------------------------------------------------
# Initial settings
#library(hdm)
set.seed(2001)
setwd("/Users/svenvanholtencharria/Documents/Econometrics Erasmus/Year 5/Thesis/Sven Code")
source("externalFunctions.R")
#source("auxFunctions.R")
source("auxFunctions_replication.R")
source("auxFunctions.R")
source("auxPenalties.R")
source("auxAlgorithms.R")




# ----------------------------------------------------------------------------------------------------
# Definitions
# ----------------------------------------------------------------------------------------------------
sigmaSims = 50
lambdaSims = 2000
n = 100; p = 200 
alpha0 = 1
gamma = 0.05; c_lambda = 1.1 # Recommendations: conf-level & lambda param
rho = 0.5
psi = 0.75 # Recommendation
options = list(lambdaSims = lambdaSims,  
               sigmaSims = sigmaSims, 
               gamma = gamma, 
               c_lambda = c_lambda, 
               psi = psi)

beta0first = c((1:10)^-1,rep(0,190)) 
beta0second = c((1:5)^-1, rep(0,5), (1:5)^-1, rep(0,185))
homoscedastic = TRUE



# ----------------------------------------------------------------------------------------------------
# Estimations
# ----------------------------------------------------------------------------------------------------
replications = 500
all_bias = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("post","double","binom", "unif", "adaptive", "score"))))
all_relevant = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("post","double","binom", "unif", "adaptive", "score"))))
all_total = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("post","double","binom", "unif", "adaptive", "score"))))


for (i in 1:replications) {
  
  # Init
  mats = starting_mats(n, p, rho, alpha0, beta0first, beta0second)
  Y = mats$Y; X = mats$X
  
  # Control detection // Same across methods to ensure validity
  detected = c(0,rbinom(20, 1, 0.5),rep(0,180))
  detected_ind = which(detected == 1)
  
  
  ### METHOD 0.0.0: Post-Lasso
  # Function
  reg1 = Alg2(Y, X, homoscedastic = homoscedastic, options, exemptIndices = c(1,2) )
  ind1 = which(reg1$beta[-c(1,2)] > 0) + 2
  ind_c = c(1,2,ind1)
  ols = lm(Y ~ -1 + X[,ind_c])
  
  # Metrics
  selected = selectedControls(k, ind1 - 2)
  
  # Reporting
  all_bias[i,1] = coef(ols)[2] - alpha0
  all_relevant[i,1] = selected$relevant
  all_total[i,1] = selected$total
  
  
  
  ### METHOD 0: Control (post-double)
  # Function
  reg1_control = Alg2(X[,2], X[,-2], homoscedastic = homoscedastic, options, exemptIndices = 1 )
  ind1_control = which(reg1_control$beta[-1] > 0) + 2
  reg2_control = Alg2(Y, X[,-2], homoscedastic = homoscedastic, options, exemptIndices = 1 )
  ind2_control = which(reg2_control$beta[-1] > 0) + 2
  ind_c_control = c(1,2,union(ind1_control, ind2_control))
  ols_control = lm(Y ~ -1 + X[,ind_c_control])
  
  # Metrics
  selected_control = selectedControls(k, union(ind1_control, ind2_control) - 2)
  
  # Reporting
  all_bias[i,2] = coef(ols_control)[2] - alpha0

  all_relevant[i,2] = selected_control$relevant
  all_total[i,2] = selected_control$total
  
  
  ### METHOD 1 : Binomial
  # Function
  penaltyBinom = penaltyGen_binom(detected)
  reg1_binom = Alg2_pen(X[,2], X[,-2], homoscedastic = homoscedastic, options, penaltyBinom)
  ind1_binom = which(reg1_binom$beta[-1] > 0) + 2
  reg2_binom = Alg2_pen(Y, X[,-2], homoscedastic = homoscedastic, options, penaltyBinom)
  ind2_binom = which(reg2_binom$beta[-1] > 0) + 2
  ind_c_binom = c(1,2,union(ind1_binom, ind2_binom))
  ols_binom = lm(Y ~ -1 + X[,ind_c_binom])
  
  # Metrics
  selected_binom = selectedControls(k, union(ind1_binom, ind2_binom) - 2)
  
  # Reporting
  all_bias[i,3] = coef(ols_binom)[2] - alpha0
  # all_se[i,1] = se_binom
  # all_coverage[i,1] = abs(all_bias[i,1] / all_se[i,1]) > qnorm(0.975)
  all_relevant[i,3] = selected_binom$relevant
  all_total[i,3] = selected_binom$total
  
  
  
  ### METHOD 2 : Uniform
  # Function
  penaltyUnif = penaltyGen_unif(detected)
  reg1_unif = Alg2_pen(X[,2], X[,-2], homoscedastic = homoscedastic, options, penaltyUnif)
  ind1_unif = which(reg1_unif$beta[-1] > 0) + 2
  reg2_unif = Alg2_pen(Y, X[,-2], homoscedastic = homoscedastic, options, penaltyUnif)
  ind2_unif = which(reg2_unif$beta[-1] > 0) + 2
  ind_c_unif = c(1,2,union(ind1_unif, ind2_unif))
  ols_unif = lm(Y ~ -1 + X[,ind_c_unif])
  
  # Metrics
  selected_unif = selectedControls(k, union(ind1_unif, ind2_unif) - 2)
  
  # Reporting
  all_bias[i,4] = coef(ols_unif)[2] - alpha0
  all_relevant[i,4] = selected_unif$relevant
  all_total[i,4] = selected_unif$total
  
  
  
  ### METHOD 3 : two-stage OLS / adaptive
  # Function
  reg1_adap = Alg2_adaptive(X[,2], X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind1_adap = which(reg1_adap$beta[-1] > 0) + 2
  reg2_adap = Alg2_adaptive(Y, X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind2_adap = which(reg2_adap$beta[-1] > 0) + 2
  ind_c_adap = c(1,2,union(ind1_adap, ind2_adap))
  ols_adap = lm(Y ~ -1 + X[,ind_c_adap])
  
  # Metrics
  selected_adap = selectedControls(k, union(ind1_adap, ind2_adap) - 2)
  
  # Reporting
  all_bias[i,5] = coef(ols_adap)[2] - alpha0
  all_relevant[i,5] = selected_adap$relevant
  all_total[i,5] = selected_adap$total
  
  
  
  ### METHOD 4: Score
  # Function
  reg1_score = Alg2_Score(X[,2], X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind1_score = which(reg1_score$beta[-1] > 0) + 2
  reg2_score = Alg2_Score(Y, X[,-2], homoscedastic = homoscedastic, options, exemptIndices = c(1), detected_ind)
  ind2_score = which(reg2_score$beta[-1] > 0) + 2
  ind_c_score = c(1,2,union(ind1_score, ind2_score))
  ols_score = lm(Y ~ -1 + X[,ind_c_score])
  
  # Metrics
  selected_score = selectedControls(k, union(ind1_score, ind2_score) - 2)
  
  # Reporting
  all_bias[i,6] = coef(ols_score)[2] - alpha0
  all_relevant[i,6] = selected_score$relevant
  all_total[i,6] = selected_score$total
  
  
  print(i)
}





















all_bias_t <- all_bias %>%
  mutate(across(everything(), ~ (. - mean(.)) / sd(.)))

hist(out_m_transformed[,1])
hist(out_m_transformed[,2])
hist(out_m_transformed[,3])
hist(out_m_transformed[,4])

saveRDS(all_bias, "histogram.RDS")


x_values <- seq(-4, 4, length.out = 3000)
y_values <- dnorm(x_values, mean = 0, sd = 1)

df_long <- all_bias_t %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")



ggplot(df_long, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "#1b9e77", color = "black", size = 0.2) +
  geom_line(aes(x = x_values, y = y_values), color = "red", size = 1) +
  facet_wrap(~ variable, scales = "fixed", nrow = 2) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic()
