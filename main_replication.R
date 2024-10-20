# ----------------------------------------------------------------------------------------------------
# Packages and Sourcing
# ----------------------------------------------------------------------------------------------------
# Initial settings
#library(hdm)
set.seed(2001)
setwd("/Users/svenvanholtencharria/Documents/Econometrics Erasmus/Year 5/Thesis/Sven Code")
source("externalFunctions.R")
source("auxFunctions.R")
source("auxFunctions_replication.R")





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





# ----------------------------------------------------------------------------------------------------
# Estimations
# ----------------------------------------------------------------------------------------------------
replications = 1000
all_bias = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("LASSO", "Post", "Indirect", "DS","Oracle", "DoubleOracle"))))
all_se = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("LASSO", "Post", "Indirect", "DS","Oracle", "DoubleOracle"))))
all_coverage = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("LASSO", "Post", "Indirect", "DS","Oracle", "DoubleOracle"))))
all_relevant = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("LASSO", "Post", "Indirect", "DS","Oracle", "DoubleOracle"))))
all_total = data.frame(matrix(ncol = 6, nrow = replications, dimnames=list(NULL, c("LASSO", "Post", "Indirect", "DS","Oracle", "DoubleOracle"))))


for (i in 1:replications) {
  
  # Init
  mats = starting_mats(n, p, rho, alpha0, beta0first, beta0second)
  Y = mats$Y; X = mats$X
  k = 15
  
  
  
  
  
  ### METHOD 1 : LASSO
  # Function
  reg1 = Alg2(Y, X, homoscedastic = TRUE, options, exemptIndices = c(1,2) )
  
  # Standard error
  ind_hat = union(1,reg1$ind[which(reg1$ind != 2)])
  se_hat = se_hetero(Y, X[,2], X[,ind_hat], reg1$beta[2])
  selected = selectedControls(k, reg1$ind[-which(reg1$ind %in% c(1,2))] - 2)
  
  # Reporting
  all_bias[i,1] = reg1$beta[2] - alpha0
  all_se[i,1] = se_hat
  all_coverage[i,1] = abs(all_bias[i,1] / all_se[i,1]) > qnorm(0.975)
  all_relevant[i,1] = selected$relevant
  all_total[i,1] = selected$total
  
  
  
  
  
  ### METHOD 2 : POST-LASSO
  # Function
  reg1 = Alg2(Y, X, homoscedastic = TRUE, options, exemptIndices = c(1,2) )
  ind1 = which(reg1$beta[-c(1,2)] > 0) + 2
  ind_c = c(1,2,ind1)
  ols = lm(Y ~ -1 + X[,ind_c])
  
  # Standard error
  ind_hat = ind_c[which(ind_c != 2)]
  se_hat = se_hetero(Y, X[,2], X[,ind_hat], coef(ols)[2])
  selected = selectedControls(k, ind1-2)
  
  # Reporting
  all_bias[i,2] = coef(ols)[2]-alpha0
  all_se[i,2] = se_hat
  all_coverage[i,2] = abs(all_bias[i,2] / all_se[i,2]) > qnorm(0.975)
  all_relevant[i,2] = selected$relevant
  all_total[i,2] = selected$total
  
 
  
  
  
  
  ### METHOD 3 : INDIRECT-LASSO
  # Function
  reg1 = Alg2(X[,2], X[,-2], homoscedastic = TRUE, options, exemptIndices = 1 )
  ind1 = which(reg1$beta[-1] > 0) + 2
  ind_c = c(1,2,ind1)
  ols = lm(Y ~ -1 + X[,ind_c])
  
  # Standard error
  ind_hat = ind_c[which(ind_c != 2)]
  se_hat = se_hetero(Y, X[,2], X[,ind_hat], coef(ols)[2])
  selected = selectedControls(k, ind1-2)
  
  # Reporting
  all_bias[i,3] = coef(ols)[2]-alpha0
  all_se[i,3] = se_hat
  all_coverage[i,3] = abs(all_bias[i,3] / all_se[i,3]) > qnorm(0.975)
  all_relevant[i,3] = selected$relevant
  all_total[i,3] = selected$total
  
  
  
  
  
  ### METHOD 4 : POST-DOUBLE-SELECTION
  # Function
  reg1 = Alg2(X[,2], X[,-2], homoscedastic = TRUE, options, exemptIndices = 1 )
  ind1 = which(reg1$beta[-1] > 0) + 2
  reg2 = Alg2(Y, X[,-2], homoscedastic = TRUE, options, exemptIndices = 1 )
  ind2 = which(reg2$beta[-1] > 0) + 2
  ind_c = c(1,2,union(ind1, ind2))
  ols = lm(Y ~ -1 + X[,ind_c])
  
  # Standard error
  ind_hat = ind_c[which(ind_c != 2)]
  se_hat = se_hetero(Y, X[,2], X[,ind_hat], coef(ols)[2])
  selected = selectedControls(k, union(ind1,ind2)-2)
  
  # Reporting
  all_bias[i,4] = coef(ols)[2]-alpha0
  all_se[i,4] = se_hat
  all_coverage[i,4] = abs(all_bias[i,4] / all_se[i,4]) > qnorm(0.975)
  all_relevant[i,4] = selected$relevant
  all_total[i,4] = selected$total
  
  
  
  
  
  ### METHOD 5: ORACLE
  # Function
  beta_0 = c(1, 2, 3 : 7, 13 : 17)
  ols = lm(Y ~ -1 + X[,beta_0])
  
  # Standard error
  ind_hat = beta_0[which(beta_0 != 2)]
  se_hat = se_hetero(Y, X[,2], X[,ind_hat], coef(ols)[2])
  selected = selectedControls(k, beta_0[-c(1:2)]-2)
  
  # Reporting
  all_bias[i,5] = coef(ols)[2]-alpha0
  all_se[i,5] = se_hat
  all_coverage[i,5] = abs(all_bias[i,5] / all_se[i,5]) > qnorm(0.975)
  all_relevant[i,5] = selected$relevant
  all_total[i,5] = selected$total
  
  
  
  
  
  ### METHOD 6: DOUBLE-ORACLE
  # Function
  beta_1 = c(1, 2, 3 : 7, 13 : 17)
  beta_2 = c(1, 2, 3:12)
  beta_0 = union(beta_1, beta_2)
  ols = lm(Y ~ -1 + X[,beta_0])
  
  # Standard error
  ind_hat = beta_0[which(beta_0 != 2)]
  se_hat = se_hetero(Y, X[,2], X[,ind_hat], coef(ols)[2])
  selected = selectedControls(k, beta_0[-c(1:2)]-2)
  
  
  # Reporting
  all_bias[i,6] = coef(ols)[2]-alpha0
  all_se[i,6] = se_hat
  all_coverage[i,6] = abs(all_bias[i,6] / all_se[i,6]) > qnorm(0.975)
  all_relevant[i,6] = selected$relevant
  all_total[i,6] = selected$total
  
  
  
  
  
  print(paste(i, sep ="")) 
}





output_mat = data.frame(matrix(ncol = 6, nrow = 5, dimnames=list(NULL, c("LASSO", "Post", "Indirect", "DS","Oracle", "DoubleOracle"))), 
                        row.names = c("bias", "se","coverage","relevant","total"))
output_mat[1,] = colMeans(all_bias)
output_mat[2,] = colMeans(all_se)
output_mat[3,] = colMeans(all_coverage)
output_mat[4,] = colMeans(all_relevant)
output_mat[5,] = colMeans(all_total)
# 
# saveRDS(all_total, "design1_total.RDS")
# readRDS("design1_total.RDS")

all_bias = readRDS("design1_bias.RDS")
library(tidyr)
library(dplyr)
library(ggplot2)

# Convert data to long format and standardize
# all_bias_long$Method <- factor(all_bias_long$Method, levels = c("LASSO",  "Post", "Indirect", "DS", "Oracle", "DoubleOracle"))

#all_bias_long <- gather(all_bias %>% mutate(across(everything(), ~ (. - mean(.)) / sd(.))), key = "Method", value = "Value")
all_bias_long <- gather(all_bias %>% mutate(across(everything(), ~ .)), key = "Method", value = "Value")

method_labels <- c("LASSO" = "LASSO",
                   "Post" = "Post-LASSO",
                   "Indirect" = "Indirect Post-LASSO",
                   "DS" = "Double selection",
                   "Oracle" = "Oracle",
                   "DoubleOracle" = "Double selection Oracle")
all_bias_long$Method <- factor(all_bias_long$Method, levels = names(method_labels), labels = method_labels)


ggplot(all_bias_long, aes(x = Value, fill = Method, color = Method)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "#1b9e77", color = "darkgreen", size = 0.2) +
  facet_wrap(~ Method, scales = "fixed", nrow = 2) +  # Set nrow = 2 to have breaks on both rows
  labs(title = NULL,
       x = NULL, y = NULL) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +  # Set x-axis ticks every 1 unit from -4 to 4
  theme_minimal() +  # Use theme_minimal() to reduce unnecessary elements
  theme(panel.border = element_blank(),  # Remove panel borders
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
       strip.text = element_text( size = 20, hjust = -0.2),
        axis.line = element_line(size = 0.5, colour = "black"),
        ) +  # Customize strip text (facet titles)
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.5, alpha = 1) # +  # Add vertical line at x = 0
#  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
    #            color = "black", size = 1, alpha = 0.75, linetype = "dashed")






