#########################
### condition 3 and 4 ###
#########################

source("lasso_ridge_model.R")
source("lasso_ridge_predict.R")



cond_3_new <- function(data, model, thresh, num_param, true_param = true_param, k_def){
  nums <- seq(1:k_def)
#  models <- list()
  results_store <- list()
  
  # define the current training and test set
  
  for (fold in 1:k_def){
    
    test <- data[[fold]]
    notfold <- setdiff(nums, fold) # these are the training set folds
    
    if (k_def == 3){
      train <- rbind(data[[notfold[1]]], data[[notfold[2]]]) # bind training set together
    } else{
      train <- rbind(data[[notfold[1]]], data[[notfold[2]]], data[[notfold[3]]], data[[notfold[4]]])
    }
    
    stacked <- rbind(test, train) 
    rownames(stacked) <- 1 : length(rownames(stacked))
    ind_test <- 1:nrow(test)
    
    ig <- logical(length = nrow(stacked))
    
    # get the current logical vector for mice
    ig[ind_test] <- TRUE # now all the indices from test set are set to true
    
    
    # impute the training and test set (5x)
    
    imp <- mice(stacked, maxit = 15, method = "pmm", ignore = ig, print = F, m=5)
    imp_comp <- complete(imp, "all")
    
    # extract the training and test sets
    
    imp_comp_test <- lapply(FUN = extract_traintest, X = imp_comp, ig = ig, train = 0) # this function is found in lasso ridge model
    imp_comp_train <- lapply(FUN = extract_traintest, X = imp_comp, ig = ig, train = 1)
    
    # fit the models on the training sets and store them
    
    if (model == 1){
      models_train_lam <- lapply(FUN = lasso_model, X = imp_comp_train, thresh = thresh, num_param = num_param)
      models_lam <- lapply(models_train_lam, "[[", 2)
      models_train <- lapply(models_train_lam, "[[", 1)

    } else{
      models_train_lam <- lapply(FUN = ridge_model, X = imp_comp_train, thresh = thresh, num_param = num_param)
      models_lam <- lapply(models_train_lam, "[[", 2)
      models_train <- lapply(models_train_lam, "[[", 1)
    }
    
    
    # now predict every model on every test set
    
    pred_1 <- lapply(FUN = pred_test, model = model,X = imp_comp_test, model_fit = models_train[["1"]], thresh = thresh, lambda = models_lam[[1]],num_param = num_param, true_param = true_param)
    pred_2 <- lapply(FUN = pred_test, model = model,X = imp_comp_test, model_fit = models_train[["2"]], thresh = thresh, lambda = models_lam[[2]],num_param = num_param, true_param = true_param)
    pred_3 <- lapply(FUN = pred_test, model = model,X = imp_comp_test, model_fit = models_train[["3"]], thresh = thresh, lambda = models_lam[[3]],num_param = num_param, true_param = true_param)
    pred_4 <- lapply(FUN = pred_test, model = model,X = imp_comp_test, model_fit = models_train[["4"]], thresh = thresh, lambda = models_lam[[4]],num_param = num_param, true_param = true_param)
    pred_5 <- lapply(FUN = pred_test, model = model,X = imp_comp_test, model_fit = models_train[["5"]], thresh = thresh, lambda = models_lam[[5]],num_param = num_param, true_param = true_param)
    
    list_pred <- list(pred_1, pred_2, pred_3, pred_4, pred_5)
    
    # combine the results into one matrix
    
    results_test <- do.call(rbind, list_pred)
    
    # return the models (for later) and the results matrix
    
    results_store[[fold]] <- do.call(rbind, results_test) # matrix with 25 rows for 25 results (-> 5 training models predict on 5 test sets, 5*5 = 25)
   # models[[fold]] <- models_train
    
  }
  
  # return the models and the results from each fold
  
  results <- list("results_test" = results_store)#, "models_train" = models
  return(results)
  
}


cond_4_new <- function(data, model, thresh, folds, num_param, true_param = true_param, k_def){
  nums <- seq(1:k_def)
#  models <- list()
  results_store <- list()
  
  # define the current training and test set
  
  for (fold in 1:k_def){
    
    test <- data[[fold]]
    notfold <- setdiff(nums, fold) # these are the training set folds
    
    if (k_def == 3){
      train <- rbind(data[[notfold[1]]], data[[notfold[2]]]) # bind training set together
    } else{
      train <- rbind(data[[notfold[1]]], data[[notfold[2]]], data[[notfold[3]]], data[[notfold[4]]])
    }
    
    stacked <- rbind(test, train)
    rownames(stacked) <- 1 : length(rownames(stacked)) 
    ind_test <- 1:nrow(test)
    
    ig <- logical(length = nrow(stacked))
    
    # get the current logical vector for mice
    ig[ind_test] <- TRUE # now all the indices from test set are set to true
    
    
    # impute the training and test set (5x)
    
    # impute test and training separately
    
    imp_train <- mice(train, maxit = 15, method = "pmm",  print = F, m=5)
    imp_test <- mice(test, maxit = 15, method = "pmm", print = F, m=5)
    imp_comp_train <- complete(imp_train, "all")
    imp_comp_test <- complete(imp_test, "all")
    
    
    # fit the models on the training sets and store them
    
    if (model == 1){
      models_train_lam <- lapply(FUN = lasso_model, X = imp_comp_train, thresh = thresh, num_param = num_param)
      models_lam <- lapply(models_train_lam, "[[", 2)
      models_train <- lapply(models_train_lam, "[[", 1)
    } else{
      models_train_lam <- lapply(FUN = ridge_model, X = imp_comp_train, thresh = thresh, num_param = num_param)
      models_lam <- lapply(models_train_lam, "[[", 2)
      models_train <- lapply(models_train_lam, "[[", 1)
    }
    
    
    # now fit every model on every test set
    
    pred_1 <- lapply(FUN = pred_test,model = model, X = imp_comp_test, model_fit = models_train[["1"]], thresh = thresh, lambda = models_lam[[1]],num_param = num_param, true_param = true_param)
    pred_2 <- lapply(FUN = pred_test,model = model, X = imp_comp_test, model_fit = models_train[["2"]], thresh = thresh, lambda = models_lam[[2]],num_param = num_param, true_param = true_param)
    pred_3 <- lapply(FUN = pred_test, model = model,X = imp_comp_test, model_fit = models_train[["3"]], thresh = thresh, lambda = models_lam[[3]],num_param = num_param, true_param = true_param)
    pred_4 <- lapply(FUN = pred_test, model = model,X = imp_comp_test, model_fit = models_train[["4"]], thresh = thresh, lambda = models_lam[[4]],num_param = num_param, true_param = true_param)
    pred_5 <- lapply(FUN = pred_test,model = model, X = imp_comp_test, model_fit = models_train[["5"]], thresh = thresh, lambda = models_lam[[5]],num_param = num_param, true_param = true_param)
    
    list_pred <- list(pred_1, pred_2, pred_3, pred_4, pred_5)
    
    # combine the results into one matrix
    
    results_test <- do.call(rbind, list_pred)
    
    # return the models (for later) and the results matrix
    
    results_store[[fold]] <- do.call(rbind, results_test) # matrix with 25 rows for 25 results (-> 5 training models predict on 5 test sets, 5*5 = 25)
  #  models[[fold]] <- models_train
    
  }
  
  # return the models and the results from each fold
  
  results <- list("results_test" = results_store) #, "models_train" = models
  return(results)
  
}


