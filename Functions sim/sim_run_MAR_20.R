#########################################
### running the simulation conditions ###
#########################################

# Note: maybe it's better not to run everything at once, just 4 at a time. :) 
# On my laptop, every condition takes roughly 2-3 hours - so 4 can be started to run over night for instance.
# Also, I have provided you with the datasets already (the big 50.000 ones; data_spec_1, and data_spec_2) that are then called in this script.
# Thanks hon <3

source("sim.R")
source("miss_patt.R")
source("data_sim.R")
source("miss_patt.R")
source("sample_ampute_sub.R")
source("CV_folds.R")
source("cond12_new.R")
source("cond34_new.R")
source("cond2_minusy.R")
source("lasso_ridge_model.R")
source("lasso_ridge_predict.R")
source("Functions_eval.R")

cores_choose <- detectCores() - 1 # this leaves one core free
clust <- parallel::makeCluster(cores_choose)


#clust <- makeCluster(n.cores)
clusterExport(cl=clust, varlist=c("lasso_select_calc","extract_traintest","lasso_model" ,"ridge_model", "pred_test", "AUC_calc", "brier_calc", "calibration_calc", 
                                  "R2_calc", "sens_spec_calc", "MSPE","replace_y"  ,"cond_12_new"), envir=environment())


clusterEvalQ(clust, c(library(mice), library(pROC), library(glmnet)))

### Condition: 20 covariates ###

mis_pattern <- miss_patt(n_param= 20)

### MAR ###




## run all conditions for lasso
#set.seed(395)
#data_spec_1 <- call_datasim(scenario = 1, n = 50000)

data_spec_1 <- readRDS("data_spec_1") # read in data
# n = 200, mis_prop = .25, cor = ind, lasso

set.seed(45687)
MAR_1_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = ind, lasso

set.seed(17651)
MAR_2_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = ind, lasso
set.seed(8888)
MAR_3_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = ind, lasso
set.seed(9876)
MAR_4_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

## run all conditions for ridge


set.seed(56745)
# n = 200, mis_prop = .25, cor = ind, lasso

MAR_1_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = ind, lasso
set.seed(56778)
MAR_2_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = ind, lasso
set.seed(56779)
MAR_3_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = ind, lasso
set.seed(567710)
MAR_4_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MAR_1_lasso, "MAR_1_lasso")
saveRDS(MAR_2_lasso, "MAR_2_lasso")
saveRDS(MAR_3_lasso, "MAR_3_lasso")
saveRDS(MAR_4_lasso, "MAR_4_lasso")
saveRDS(MAR_1_ridge, "MAR_1_ridge")
saveRDS(MAR_2_ridge, "MAR_2_ridge")
saveRDS(MAR_3_ridge, "MAR_3_ridge")
saveRDS(MAR_4_ridge, "MAR_4_ridge")




#set.seed(45571)


## run all conditions for lasso
#data_spec_2 <- call_datasim(scenario = 3, n = 50000)

data_spec_2 <- readRDS("data_spec_2") # read in data

# n = 200, mis_prop = .25, cor = 0.3, lasso

set.seed(45572)
MAR_5_lasso <- sim(k_def = 3, data_spec = data_spec_2 , nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = 0.3, lasso
set.seed(45573)
MAR_6_lasso <- sim(k_def = 3, data_spec = data_spec_2 , nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = 0.3, lasso
set.seed(45574)
MAR_7_lasso <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim =200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = 0.3, lasso
set.seed(45575)
MAR_8_lasso <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim =200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)



# n = 200, mis_prop = .25, cor = 0.3, lasso
set.seed(45576)
MAR_5_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = 0.3, lasso
set.seed(45577)
MAR_6_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = 0.3, lasso
set.seed(45578)
MAR_7_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = 0.3, lassow
set.seed(45579)
MAR_8_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MAR_5_lasso, "MAR_5_lasso")
saveRDS(MAR_6_lasso, "MAR_6_lasso")
saveRDS(MAR_7_lasso, "MAR_7_lasso")
saveRDS(MAR_8_lasso, "MAR_8_lasso")
saveRDS(MAR_5_ridge, "MAR_5_ridge")
saveRDS(MAR_6_ridge, "MAR_6_ridge")
saveRDS(MAR_7_ridge, "MAR_7_ridge")
saveRDS(MAR_8_ridge, "MAR_8_ridge")
