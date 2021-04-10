##################################
### Function to run simulation ###
##################################


sim <- function(k_def, nsub, nsim, n_param, mis_prop, mis_mech = c("MCAR", "MAR"), mis_pattern, model, data_spec){
  
  library(parallel)
  library(tidyverse)
  
  source("data_sim.R")
  source("miss_patt.R")
  source("sample_ampute_sub.R")
  source("CV_folds.R")
  source("cond12_new.R")
  source("cond34_new.R")
  source("cond2_minusy.R")
  source("lasso_ridge_model.R")
  source("lasso_ridge_predict.R")
  
  
  
  options(scipen=999)
  
  ###################
  #### DATA PREP ####
  ###################
  
  # Step 1: make the huge sample
  
  data_comp <- data_spec[["data"]]
  true_param <- as.matrix(c(data_spec[["intercept"]], data_spec[["betas"]]))
  
  # Step 2: now sample from the huge sample and store this in a list
  
  data_sub_ext <- sample_sub_list(nsim = nsim, data_comp = data_comp, n = nsub)
  data_sub <- lapply(data_sub_ext, `[[`, 1) # data sub (n = X)
  data_ext <- lapply(data_sub_ext, `[[`, 2) # data external (n = 50000 - X)
  
  # Step 3: ampute every element in data_sub list
  
  data_subamp <- lapply(X = data_sub, FUN = ampute_sub, mis_pattern = mis_pattern, mis_prop = mis_prop, mis_mech = mis_mech, n_param = n_param)
  
  # note: the warnings you get here are "Data is made numeric because the calculation of weights requires numeric data", which 
  # is normal for ampute(). Just ignore.
  
  
  # Step 4: create random splits and store this in a list. NOTE: this is done using the complete data (outcome is always observed) to 
  # ensure that this is always the same.
  
  folds_def <- lapply(X = data_sub, FUN = CV_fold, n_param = n_param, k_def = k_def)
  
  # Step 5: fit the model on data_sub and predict on external
  
  if(model == 1){
    model_sub <- lapply(X = data_sub, FUN = lasso_model, thresh = 0.5, num_param = n_param)
  } else{
    model_sub <- lapply(X = data_sub, FUN = ridge_model, thresh = 0.5, num_param = n_param)
  }
  
  evaluate_sub_ext <- sub_ext_pred(model = model, model_test = model_sub, test = data_ext, num_param = n_param, thresh = 0.5, true_param = true_param) # to be found in lasso ridge predict
  evaluate_sub_ext <- do.call(rbind, evaluate_sub_ext)
  
  # Step 6: do CV + evaluation on complete data (the data from step 2)

  folds_comp <- CV_fold_extract(folds = folds_def, data = data_sub, nsim = nsim, k_def = k_def) # these are the folds used
  results_comp <- parallel::parLapply(cl =clust ,X = folds_comp, fun = cond_12_new, model = model, thresh = 0.5, num_param = n_param, true_param = true_param, k_def = k_def)

  # Step 7: extract the folds from the amputed data (for conditions 3 + 4)
  
  folds_incomp <- CV_fold_extract(folds = folds_def, data = data_subamp, nsim = nsim, k_def = k_def) # these are the folds used
  
  
  ### DATA PREP END ###
  
  
  ########################
  #### RUN CONDITIONS ####
  ########################
  
  # NOTE: the results from complete data are in step 5 above (results_comp)
  
  # Step 1: Condition 1. Impute whole dataset, then perform CV.
  
  
  con1_imp <- parallel::parLapply(cl = clust, X = data_subamp, fun = mice, maxit = 15, method = "pmm", print = F) # apply mice to every element from data_subamp
  con1_data <- lapply(X = con1_imp, FUN = complete, "all")# get the 5 complete datasets per element
  extract_folds_1 <- CV_fold_extract_imp(k_def = k_def, folds_defi = folds_def, con1_dat = con1_data, nsim = nsim) 
  results_cond1 <- lapply(X = extract_folds_1, FUN = cond12_impdatloop, thresh = 0.5, model = model, num_param = n_param, true_param = true_param, k_def = k_def)
  
  
  # Step 2: Condition 2. Same as condition 1, just with y missing in validation set. 

  
  results_cond2 <- parallel::parLapply(cl = clust, X = folds_incomp, fun = cond_2y, model = model, thresh = 0.5, num_param = n_param, true_param = true_param, k_def = k_def)
  
 
  # Step 3: Condition 3. Use ignore.
  
  results_cond3 <- parallel::parLapply(cl = clust,X = folds_incomp, fun = cond_3_new,  thresh = 0.5, num_param = n_param, true_param = true_param, model = model, k_def = k_def)
 
  
  # Step 4: Condition 4. Impute training and test set separately. For this I will use folds_incomp.
  
  results_cond4 <- parallel::parLapply(cl = clust, X = folds_incomp, fun= cond_4_new,  thresh = 0.5, num_param = n_param, true_param = true_param, model = model, k_def = k_def)
 
  
  all_results <- list("Results_cond1" = results_cond1, "Results_cond2" = results_cond2 ,"Results_cond3" = results_cond3, "Results_cond4" = results_cond4, "data_spec" = data_spec,
                      "data_sub" = data_sub, "data_subamp" = data_subamp, "folds_def" = folds_def, "evaluate_sub_ext" = evaluate_sub_ext, "results_comp" = results_comp)
  #"model_sub" = model_sub,"data_ext" = data_ext,
  
  return(all_results)
  
}

