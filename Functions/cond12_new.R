##############################################################
### Condition 1, 2, and also for complete (non-missing) CV ###
##############################################################


# this function is applied to each imputed dataset

cond_12_new <- function(data, model, thresh, num_param, true_param, k_def){
  nums <- seq(1:k_def)
  results_test <- matrix(data = 0, nrow = k_def, ncol = 12)
  colnames(results_test) <- c("AUC", "brier", "sens", "spec", "cal_b0", "cal_b1", "MSPE" , "CoxSnell", "McFadden", "sens_selection", "spec_selection","lambda")
 # models <- list()
  
  for (fold in 1:k_def){
    test_imp <- data[[fold]]
    notfold <- setdiff(nums, fold) # these are the training set folds
    
    if (k_def == 3){
      train_imp <- rbind(data[[notfold[1]]], data[[notfold[2]]]) # bind training set together
    } else if(k_def == 5){
      train_imp <- rbind(data[[notfold[1]]], data[[notfold[2]]], data[[notfold[3]]], data[[notfold[4]]])
    }
    
    # fit the model on the training set 
    
    if (model == 1){ # LASSO
      # set.seed(1) # seed was only used here for testing
      model_train <- lasso_model(train = train_imp, thresh = thresh, num_param = num_param)
      results_test[fold,] <- pred_test(model = model, model_fit = model_train[["model"]], lambda = model_train[["lambda"]], test = test_imp, num_param = num_param, thresh = thresh, true_param = true_param)
      
    } else{ # ridge
      model_train <- ridge_model(train = train_imp, thresh = thresh, num_param = num_param)
      results_test[fold,] <- pred_test(model = model, model_fit = model_train[["model"]], lambda = model_train[["lambda"]], test = test_imp,  num_param = num_param, thresh = thresh, true_param = true_param)
      
    }
   #  models[[fold]] <- model_train
    
  }
  
  # return 
  
  results <- list("results_test" = results_test) # , "models" = models
  return(results)
}


cond12_impdatloop <- function(dataset, thresh, num_param, model, true_param = true_param, k_def){
  result <- lapply(X = dataset, k_def = k_def , FUN = cond_12_new, model = model , thresh = thresh, num_param = num_param, true_param = true_param)
  # so if we are at dataset = extract_folds_1[[1]], then within that dataset, there's 5 sub-datasets. To each of those, cond_12_new
  # is applied, giving results from the 3 folds.
  
  # pick the results apart
  results_test <- lapply(result, "[[", 1)
  results_test <- do.call(rbind, results_test)
 # models_train <- lapply(result, "[[", 2)
  
  return(list("results_test" = results_test)) #, "models_train" = models_train
}




cond_12_gather <- function(imp_dat, model, thresh, num_param){
  results_gather <- lapply(FUN = cond_12_new, X = imp_dat, model, thresh, num_param)
  return(results_gather)
}


cond_12_gather <- function(data, model, thresh, num_param){
  result <- list()
  for(i in 1:5){
    result[[i]] <- cond_12_new(data = data[[i]], model=model, thresh=thresh, num_param=num_param)
  }
  return(result)
}





