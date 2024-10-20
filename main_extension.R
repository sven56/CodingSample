# ----------------------------------------------------------------------------------------------------
# Packages and Sourcing
# ----------------------------------------------------------------------------------------------------
# Initial settings
library(dplyr); library(purrr)
set.seed(2001)
setwd("/Users/svenvanholtencharria/Documents/Econometrics Erasmus/Year 5/Thesis/Sven Code")
source("externalFunctions.R")
source("auxFunctions.R")
source("auxPenalties.R")
source("auxAlgorithms.R")
source("auxFunctionsExtension.R")

# Importing data extension. X = [1 Constant X's]

import_v = as.matrix(read.csv("data_violent.csv", header = TRUE, check.names = FALSE)) %>%
  `colnames<-`(gsub("'", '', colnames(.)))
Y_v = import_v[,1]; X_v = import_v[,-1]

import_m = as.matrix(read.csv("data_property.csv", header = TRUE, check.names = FALSE)) %>%
  `colnames<-`(gsub("'", '', colnames(.)))
Y_p = import_m[,1]; X_p = import_m[,-1]

import_m =  as.matrix(read.csv("data_murder.csv", header = TRUE, check.names = FALSE)) %>%
  `colnames<-`(gsub("'", '', colnames(.)))
Y_m = import_m[,1]; X_m = import_m[,-1]

matrix_list = list(violent = list(Y = Y_v, X = X_v),
                   property = list(Y = Y_p, X = X_p),
                   murder = list(Y = Y_m, X = X_m))





# ----------------------------------------------------------------------------------------------------
# Definitions
# ----------------------------------------------------------------------------------------------------
sigmaSims = 20
lambdaSims = 2000
gamma = 0.05; c_lambda = 1.1 # Recommendations: conf-level & lambda param
psi = 0.75 # Recommendation
options = list(lambdaSims = lambdaSims,  
               sigmaSims = sigmaSims, 
               gamma = gamma, 
               c_lambda = c_lambda, 
               psi = psi)
amelioration = c(2:8) # Hard-coded for this data





# ----------------------------------------------------------------------------------------------------
# Violent crime 
# ----------------------------------------------------------------------------------------------------
# amelioration = c(2:8)
# 
 theMachine(Y_v, X_v, options, amelioration, homoscedastic = F)
# HETERO
# control, control + amel; discrete; discrete half; adaptive; score
# bias: -0.1580575 -0.1332509 -0.1133938 -0.1494159 -0.1421392 -0.1130425
# se: 0.1200850 0.1216170 0.1202588 0.1191140 0.1226025 0.1207528
# relevant: 0 7 6 0 2 5
# total: 11 18 18 12 13 18

# HOMO
# bias: -0.1552900 -0.1476704 -0.1470847 -0.1552900 -0.1551153 -0.1482538
# se : 0.03991453 0.04136723 0.04107985 0.03991453 0.04016469 0.04082433
# relevant: 0 7 6 0 1 2
# total: 0 7 6 0 1 2



theMachine(Y_p, X_p, options, amelioration, homoscedastic = F)
# HETERO
# bias: -0.02442548 -0.02763764 -0.01259456 -0.02682557 -0.02733067 -0.01648728
# se: 0.04292132 0.04447925 0.04414870 0.04344445 0.04351797 0.04351844
# relevant: 0 7 6 2 2 3
# total: 12 19 17 14 14 14

# HOMO
# bias: -0.1014690 -0.1093921 -0.1082287 -0.1014690 -0.1014690 -0.1020206
# se : 0.02286741 0.02360200 0.02343700 0.02286741 0.02286741 0.02303081
# relevant: 0 7 6 0 0 1
# total: 0 7 6 0 0 1

 


 theMachine(Y_m, X_m, options, amelioration, homoscedastic = F)
# HETERO
# bias: -0.11690069 -0.10295095 -0.06299893 -0.07920342 -0.11690069 -0.11723874
# se: 0.4172690 0.4336348 0.4205388 0.4166745 0.4172690 0.4233254
# relevant: 0 7 4 1 0 2
# total: 8 15 12  9  8 10
 
# HOMO
# bias: -0.2061251 -0.2119290 -0.1913883 -0.2042427 -0.2061251 -0.2078977
# se : 0.1765913 0.1822610 0.1831449 0.1765357 0.1765913 0.1783909
# relevant:  0 7 4 1 0 1
# total:   0 7 4 1 0 1

# ----------------------------------------------------------------------------------------------------
# Subset regression
# ----------------------------------------------------------------------------------------------------
reps = 100
subset_bias = data.frame(matrix(ncol = 6, nrow = reps, dimnames=list(NULL, c("control","controlAmel","binom", "unif", "adaptive", "score"))))
subset_se = data.frame(matrix(ncol = 6, nrow = reps, dimnames=list(NULL, c("control","controlAmel","binom", "unif", "adaptive", "score"))))
subset_coverage = data.frame(matrix(ncol = 6, nrow = reps, dimnames=list(NULL, c("control","controlAmel","binom", "unif", "adaptive", "score"))))
subset_relevant = data.frame(matrix(ncol = 6, nrow = reps, dimnames=list(NULL, c("control","controlAmel","binom", "unif", "adaptive", "score"))))
subset_total = data.frame(matrix(ncol = 6, nrow = reps, dimnames=list(NULL, c("control","controlAmel","binom", "unif", "adaptive", "score"))))


for (set in 1:3) {
  
  # Initialize data
  Y = matrix_list[[set]]$Y
  X = matrix_list[[set]]$X
  
  for (size in c(200, 100, 50)) {

    for (i in 1:reps) {
      
      # Setting the indices, random sampled
      ind = sort(union(1:9, sample(1:277, size - 7 ) + 9))
      X_hat = X[,ind]
      
      output = theMachine(Y, X_hat, options, amelioration, homoscedastic = FALSE)
      subset_bias[i,] = output$bias
      subset_se[i,] = output$se
      subset_coverage[i,] = output$coverage
      subset_relevant[i,] = output$relevant
      subset_total[i,] = output$total
      
      print(paste("Set: ",names(matrix_list[set]), " - Size: ", size, " - Iteration: ",i, sep ="")) 
      
      rm(output, ind, X_hat)
    }
    
    # Reporting
    output_mat = data.frame(matrix(ncol = 6, nrow = 5, dimnames=list(NULL, c("control","controlAmel","binom", "unif", "adaptive", "score"))), 
                            row.names = c("bias", "se","coverage","relevant","total"))
    output_mat[1,] = colMeans(subset_bias, na.rm = TRUE)
    output_mat[2,] = colMeans(subset_se, na.rm = TRUE)
    output_mat[3,] = colMeans(subset_coverage, na.rm = TRUE)
    output_mat[4,] = colMeans(subset_relevant, na.rm = TRUE)
    output_mat[5,] = colMeans(subset_total, na.rm = TRUE)
    
    output_name = paste("output/",names(matrix_list[set]),"_",size,".csv",sep = "")
    
    write.csv(output_mat, file = output_name)
    
  } 
}












