library(glmnet)

pred_test <- function(model_fit, test, lambda, num_param, thresh, true_param, model){
  
  # storage
  
  results_test <- matrix(data = 0, nrow = 1, ncol = 12)
  colnames(results_test) <- c("AUC", "brier", "sens", "spec", "cal_b0", "cal_b1", "MSPE" , "CoxSnell" , "McFadden", "sens_selection", "spec_selection", "lambda")
  test = data.matrix(test)
  test[,(num_param + 1)] <- test[,(num_param + 1)]-1 
  
  
  # predict on the test set
  
  preds_test <- predict(model_fit, s = lambda, newx = test[,-(num_param+1)], type = "response")
  
  # get coefficients from best model for prediction evaluation (e.g., calibration slope)
  
  best_model_coeff <-  predict(model_fit, type="coefficients", s=lambda)
  
 
  # evaluate the prediction: on test set
  
   results_test[, "AUC"] <- AUC_calc(pred_prob = preds_test, actual = test[, "y"]) 
   results_test[, "brier"] <- brier_calc(pred_prob = preds_test, actual = (test[, "y"]))
   results_test[, "sens"] <- sens_spec_calc(thresh = thresh, pred_prob = preds_test, actual = test[, "y"])[["sens"]] 
   results_test[, "spec"] <- sens_spec_calc(thresh = thresh, pred_prob = preds_test, actual = test[, "y"])[["spec"]] 
   results_test[, "cal_b0"] <- calibration_calc(betas =best_model_coeff[2:(num_param+1),], b0 = best_model_coeff[1,], test = test, num_param = num_param )[1]
   results_test[, "cal_b1"] <- calibration_calc(betas = best_model_coeff[2:(num_param+1),], b0 = best_model_coeff[1,], test = test, num_param = num_param)[2]
   results_test[, "MSPE"] <- MSPE(pred_prob = preds_test, true_param = true_param, dataset = test)
  results_test[, "CoxSnell"] <- R2_calc(y_test = test[,(num_param + 1)], pred_prob = preds_test)[["cox_r2"]]
  results_test[, "McFadden"] <- R2_calc(y_test = test[,(num_param + 1)], pred_prob = preds_test)[["mcfadden"]]
  results_test[, "lambda"] <- lambda
  if(model == 1){ # don't calculate this when ridge is used
    results_test[ , "sens_selection"] <- lasso_select_calc(betas = best_model_coeff[2:(num_param+1),], true_param = true_param, num_param = num_param)[["sens_select"]]
    results_test[ , "spec_selection"] <- lasso_select_calc(betas = best_model_coeff[2:(num_param+1),], true_param = true_param, num_param = num_param)[["spec_select"]]
  }
  
  return(results_test)
  
}


sub_ext_pred <- function(model_test, test, num_param, thresh, true_param, model){
  results <- list()
  for (i in 1:length(model_test)){
  
    results[[i]] <- pred_test(model = model, model_fit = model_test[[i]][["model"]], test = test[[i]], lambda = model_test[[i]][["lambda"]], num_param = num_param, thresh = thresh, true_param = true_param)
  }
  return(results)
}
