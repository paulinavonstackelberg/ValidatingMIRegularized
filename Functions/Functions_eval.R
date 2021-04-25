###############################################
### Functions for evaluation of predictions ###
###############################################

# In this script, you can find the functions for evaluation of predictive measures.
# the following functions were adapted using code from https://github.com/MvanSmeden/Beyond-EPV: R2_calc, MSPE

# I am calculating the following measures: Brier score, AUC, sensitivity, specificity, Calibration slope + intercept, Cox-Snell + McFadden's R^2

library(caret)
library(pROC)



# Brier score

brier_calc <- function(pred_prob, actual){
  return(mean((pred_prob-actual)^2))
}

# calibration slope + intercept 

calibration_calc <- function(betas, b0, test, num_param){
  lp = as.vector(b0 + test[,-(num_param + 1)] %*% data.matrix(betas)) # get the linear predictor
  cal <- glm(as.factor(test[,"y"])~lp, family = "binomial")
  coefs <- cal$coef
  return(coefs)
}


# R^2

R2_calc <- function(y_test, pred_prob){ 
  # log-likelihood
  log_lik <- function(pred_prob, y_test){
    return(sum(y_test*log(pred_prob)+(1-y_test)*log(1-pred_prob)))
  }
  loglik_0 <- log_lik(pred_prob=mean(y_test), y_test = y_test)  
  loglik_1  <- log_lik(pred_prob=pred_prob, y_test = y_test) 
  cox_r2 <- 1 - exp(-(loglik_1 - loglik_0) * 2/length(y_test))
  return(list("cox_r2" = cox_r2, "mcfadden" = 1-(loglik_1/loglik_0)))
}


# AUC

AUC_calc <- function(pred_prob, actual){ 
  ROC <- pROC::roc(response = actual, predictor = as.vector(pred_prob), direction = "<")
  auc_res <- as.numeric(auc(ROC))
  return(auc_res)
  
}



# Sensitivity + specificity

sens_spec_calc <- function(thresh, pred_prob, actual){
  pred_class <- ifelse(pred_prob > thresh, 1, 0) # predict positive (Y = 1) when the predicted probability exceeds the threshold
  pred_class <- factor(pred_class)
  actual_class <- factor(actual)
  conf_matrix<-table(pred_class,actual_class)
  TP <- conf_matrix[4] # true positive
  TN <- conf_matrix[1] # true negative
  FN <- conf_matrix[3] # false negative
  FP <- conf_matrix[2] # false positive
  sens <-   TP / (TP + FN)
  spec <-   TN / (TN + FP)
  result <- list('sens' = sens, 'spec' = spec)
  return(result)
}


#length(intersect(which(pred_class == 0),which(actual_class ==0)))
#length(intersect(which(pred_class == 1),which(actual_class ==0)))


lasso_select_calc <- function(betas, true_param, num_param){
  sim_param <- data.matrix(true_param[-1,]) # only get betas, not intercept
  betas_pen <- data.matrix(betas)
  betas_select <- betas_pen != 0
  betas_import <- sim_param != 0
  
  # calculate how many relevant variables were selected and how many irrelevant variables were not selected
  
  num_imp_select <- sum(betas_pen[1:10, ] !=0) # number of important covariates that were selected
  num_imp_true <- sum(sim_param != 0) # number of important covariates in the data generating model
  sens_select <- num_imp_select / num_imp_true
  
  if(num_param > 10){
    num_junk_remove <- sum(betas_pen[11:20, ] == 0) # number of unimportant covariates that were set to 0
    num_junk_true <- sum(sim_param == 0) # number of unimportant covariates in the data generating model
    spec_select <- num_junk_remove / num_junk_true
  } else{
    spec_select <- 99
  }
  
  return(list("sens_select" = sens_select, "spec_select" = spec_select))
  
}


# MSPE

MSPE <- function(pred_prob, true_param, dataset){
  design_matrix <- as.matrix(cbind(1,dataset[,-which(colnames(dataset)=="y")]))
  true_prob <- exp(design_matrix %*% true_param)/(1+exp(design_matrix %*% true_param))
  return(mean((true_prob-pred_prob)^2))
}


