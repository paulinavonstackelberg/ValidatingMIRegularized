############################
### Condition 2(-y test) ###
############################

# folds are defined in folds_def, and the amputed datasets are in data_subamp

cond_2y <- function(data, model, thresh, num_param, true_param, k_def){
  nums <- seq(1:k_def)
  results_test <- matrix(data = 0, nrow = k_def, ncol = 12)
  colnames(results_test) <- c("AUC", "brier", "sens", "spec", "cal_b0", "cal_b1", "MSPE" , "CoxSnell", "McFadden", "sens_selection", "spec_selection","lambda")
 # models <- list()
  results_store <- list()
  
  for (fold in 1:k_def){
    test <- data[[fold]] # test data
    # get outcome and delete
    test_y <- test[, (num_param) + 1] # store the true y
    # delete y from test data (set to missing)
    test[, (num_param+1)] <- NA
    
    # now get the training data
    notfold <- setdiff(nums, fold) # these are the training set folds
    if (k_def == 3){
      train <- rbind(data[[notfold[1]]], data[[notfold[2]]]) # bind training set together
    } else{
      train <- rbind(data[[notfold[1]]], data[[notfold[2]]], data[[notfold[3]]], data[[notfold[4]]])
    }
    
    # merge test and train again
    
    stacked <- rbind(train, test)
    rownames(stacked) <- 1 : length(rownames(stacked))
    
    # impute this together
    
    imp_stacked <- mice(stacked, maxit = 15, method = "pmm", print = F, m=5)
    
    # get complete datasets
    
    imp_comp <- complete(imp_stacked, "all")
    
    # on each dataset: 1) split into test and train again, 2) fit models on each training set, 3) use each training set model
    # to predict on test sets (-> 5*5 =25 predictions)
    
    ind_train <- 1:nrow(train)
    ig <- logical(length = nrow(stacked))
    ig[1:length(ig)] <- TRUE
    
    # get the current logical vector for later on
    ig[ind_train] <- FALSE # now all the indices from training set are FALSE
    
    train_sets <- lapply(X = imp_comp, FUN = extract_traintest, ig = ig, train = 1) # this function is found in model file
    test_sets <- lapply(X = imp_comp, FUN = extract_traintest, ig = ig, train = 0)

    # replace the imputed y in the test sets with the real y
    test_sets <- lapply(X = test_sets, FUN = replace_y, y_real = test_y, num_param = num_param)  

    # fit the models on the training sets and store them
    
    if (model == 1){ #LASSO
      models_train_lam <- lapply(FUN = lasso_model, X = train_sets, thresh = thresh, num_param = num_param)
      models_lam <- lapply(models_train_lam, "[[", 2)
      models_train <- lapply(models_train_lam, "[[", 1)
    } else{ #ridge
      models_train_lam <- lapply(FUN = ridge_model, X = train_sets, thresh = thresh, num_param = num_param)
      models_lam <- lapply(models_train_lam, "[[", 2)
      models_train <- lapply(models_train_lam, "[[", 1)
    }
    
    # now fit every model on every test set
    
    pred_1 <- lapply(FUN = pred_test, model = model, X = test_sets, model_fit = models_train[["1"]], thresh = thresh, lambda = models_lam[[1]],num_param = num_param, true_param = true_param)
    pred_2 <- lapply(FUN = pred_test, model = model, X = test_sets, model_fit = models_train[["2"]], thresh = thresh, lambda = models_lam[[2]],num_param = num_param, true_param = true_param)
    pred_3 <- lapply(FUN = pred_test, model = model,X = test_sets, model_fit = models_train[["3"]], thresh = thresh, lambda = models_lam[[3]],num_param = num_param, true_param = true_param)
    pred_4 <- lapply(FUN = pred_test, model = model,X = test_sets, model_fit = models_train[["4"]], thresh = thresh, lambda = models_lam[[4]],num_param = num_param, true_param = true_param)
    pred_5 <- lapply(FUN = pred_test, model = model, X = test_sets, model_fit = models_train[["5"]], thresh = thresh, lambda = models_lam[[5]],num_param = num_param, true_param = true_param)
    
    list_pred <- list(pred_1, pred_2, pred_3, pred_4, pred_5)
    
    # combine the results into one matrix
    
    results_test <- do.call(rbind, list_pred)
    
    # return the models (for later) and the results matrix
    
    results_store[[fold]] <- do.call(rbind, results_test) # matrix with 25 rows for 25 results (-> 5 training models predict on 5 test sets, 5*5 = 25)
    
  }
  # return output
  results <- list("results_test" = results_store) 
  return(results)
}


replace_y <- function(test_set, y_real, num_param){
  test_set[, (num_param + 1)] <- y_real
  return(test_set)
}
