# ----------------------------------------------------------------------------------------------------
# Packages and Sourcing
# ----------------------------------------------------------------------------------------------------
# Initial settings
#library(hdm)
set.seed(2001)
setwd("/Users/svenvanholtencharria/Documents/Econometrics Erasmus/Year 5/Thesis/Sven Code")
source("externalFunctions.R")
source("auxFunctions.R")
source("auxPenalties.R")
source("auxAlgorithms.R")

# ----------------------------------------------------------------------------------------------------
# Definitions
# ----------------------------------------------------------------------------------------------------
sigmaSims = 20
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
homoscedastic = T





# ----------------------------------------------------------------------------------------------------
# Simulations
# ----------------------------------------------------------------------------------------------------
replications = 1000
all_bias = data.frame(matrix(ncol = 5, nrow = replications, dimnames=list(NULL, c("binom", "unif", "adaptive", "score","control"))))
all_se = data.frame(matrix(ncol = 5, nrow = replications, dimnames=list(NULL, c("binom", "unif", "adaptive", "score","control"))))
all_coverage = data.frame(matrix(ncol = 5, nrow = replications, dimnames=list(NULL, c("binom", "unif", "adaptive", "score","control"))))
all_relevant = data.frame(matrix(ncol = 5, nrow = replications, dimnames=list(NULL, c("binom", "unif", "adaptive", "score","control"))))
all_total = data.frame(matrix(ncol = 5, nrow = replications, dimnames=list(NULL, c("binom", "unif", "adaptive", "score","control"))))



for (R21 in c(0.4)) {
  for (R22 in c(0.2)) {
    for (accuracy in c("low")) {
      for (i in 1:replications) {
        
        # Model generation
        k = 10
        #R21 = R22 = R2
        eta0 = beta0 = c(rep(1,k),rep(0,p-k))
        mats = createDesign(n, p, rho, alpha0, R21, R22, equalCoeffs = TRUE, homoscedastic = homoscedastic, eta0, beta0)
        Y = mats$Y; X = mats$X
        
        
        # Control detection // Same across methods to ensure validity
        detected =  penaltyGen_ind(accuracy)
        detected_ind = which(detected == 1)
        
        
        
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
        ind_hat_binom = ind_c_binom[which(ind_c_binom != 2)]
        se_binom = se_hetero(Y, X[,2], X[,ind_hat_binom], coef(ols_binom)[2])
        selected_binom = selectedControls(k, union(ind1_binom, ind2_binom) - 2)
        
        # Reporting
        all_bias[i,1] = coef(ols_binom)[2] - alpha0
        all_se[i,1] = se_binom
        all_coverage[i,1] = abs(all_bias[i,1] / all_se[i,1]) > qnorm(0.975)
        all_relevant[i,1] = selected_binom$relevant
        all_total[i,1] = selected_binom$total
        
        
        
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
        ind_hat_unif = ind_c_unif[which(ind_c_unif != 2)]
        se_unif = se_hetero(Y, X[,2], X[,ind_hat_unif], coef(ols_unif)[2])
        selected_unif = selectedControls(k, union(ind1_unif, ind2_unif) - 2)
        
        # Reporting
        all_bias[i,2] = coef(ols_unif)[2] - alpha0
        all_se[i,2] = se_unif
        all_coverage[i,2] = abs(all_bias[i,2] / all_se[i,2]) > qnorm(0.975)
        all_relevant[i,2] = selected_unif$relevant
        all_total[i,2] = selected_unif$total
        
        
        
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
        all_bias[i,3] = coef(ols_adap)[2] - alpha0
        all_se[i,3] = se_adap
        all_coverage[i,3] = abs(all_bias[i,3] / all_se[i,3]) > qnorm(0.975)
        all_relevant[i,3] = selected_adap$relevant
        all_total[i,3] = selected_adap$total
        
        
        
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
        all_bias[i,4] = coef(ols_score)[2] - alpha0
        all_se[i,4] = se_score
        all_coverage[i,4] = abs(all_bias[i,4] / all_se[i,4]) > qnorm(0.975)
        all_relevant[i,4] = selected_score$relevant
        all_total[i,4] = selected_score$total
        
        
        
        ### METHOD 5: Control
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
        all_bias[i,5] = coef(ols_control)[2] - alpha0
        all_se[i,5] = se_control
        all_coverage[i,5] = abs(all_bias[i,5] / all_se[i,5]) > qnorm(0.975)
        all_relevant[i,5] = selected_control$relevant
        all_total[i,5] = selected_control$total
        
        
        print(paste("R21: ",R21, " - R22: ",R21, " - Accuracy: ", accuracy, " - Iteration: ",i, sep ="")) 
      }
      
      output_mat = data.frame(matrix(ncol = 5, nrow = 5, dimnames=list(NULL, c("binom", "unif", "adaptive", "score","control"))), 
                              row.names = c("bias", "se","coverage","relevant","total"))
      output_mat[1,] = colMeans(all_bias)
      output_mat[2,] = colMeans(all_se)
      output_mat[3,] = colMeans(all_coverage)
      output_mat[4,] = colMeans(all_relevant)
      output_mat[5,] = colMeans(all_total)
      
      output_name = paste("output/homo_",R21,"_",R22,"_",accuracy,".csv",sep = "")
      
      write.csv(output_mat, file = output_name)
      
    }
  }
}




# ----------------------------------------------------------------------------------------------------
# Reporting
# ----------------------------------------------------------------------------------------------------
for (i in c(1:4)) {
  print(paste("Method 1:", colnames(all_bias)[i]))
  print(paste("Average bias:", mean(all_bias[,i])))
  print(paste("Average std. error.:", mean(all_se[,i])))
  print(paste("Average coverage:", coverage(all_bias[,i], all_se[,i])))
  print(paste("Average relevant controls:", mean(all_relevant[,i])))
  print(paste("Average total controls:", mean(all_total[,i])))
  print("---------------------")
}




