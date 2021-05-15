##################################
### Lasso and Ridge regression ###
##################################

## In this script, you can find the code needed to fit (and tune!) the Ridge and LASSO models on the training data

source("Functions_eval.R")

# LASSO regression

lasso_model <- function(train, thresh, num_param){
  
  train = data.matrix(train)
  
  # Find the optimal lambda (using 10-fold CV)
  
  model_cv <-
    cv.glmnet(x=train[, -(num_param+1)], y=train[, (num_param+1)],
              family = "binomial", alpha = 1)
  
  # Best lambda value
  best_lambda <- model_cv$lambda.min
  
  # fit the model
  
  best_model <-
   glmnet(
      x = train[, -(num_param+1)],
     y = train[, (num_param+1)],
     alpha = 1, 
      family = "binomial"
    )
  all_results <- list("model" = best_model, "lambda" = best_lambda)
  return(all_results)
  
}

# Ridge regression

ridge_model <- function(train, thresh, num_param){
  train = data.matrix(train)
  
  # Find the optimal lambda (using 10-fold CV)
  
  model_cv <-
    cv.glmnet(x=train[, -(num_param+1)], y=train[, (num_param+1)],
              family = "binomial", alpha = 0)
  
  # Best lambda value
  
  best_lambda <- model_cv$lambda.min
  
  # fit the model
  
  best_model <-
    glmnet(
      x = train[, -(num_param+1)],
      y = train[, (num_param+1)],
      alpha = 0, 
      family = "binomial"
    )
  all_results <- list("model" = best_model, "lambda" = best_lambda)
  return(all_results)
  
}

# extracting the training and test sets

extract_traintest <- function(imp_dat, ig, train = c(0,1)){
  if (train == 0){
    return(test_imp <- imp_dat[ig,]) # test set
  } else{
    return(train_imp <- imp_dat[!ig,]) # training set
  }
}
