#########################################
### running the simulation conditions ###
#########################################

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
library(parallel)

### Condition: 20 covariates ###

#mis_pattern <- miss_patt(n_param= 20)

cores_choose <- parallel::detectCores() -1 # this leaves one core free
clust <- parallel::makeCluster(cores_choose)
parallel::clusterExport(cl=clust, varlist=c("lasso_select_calc","extract_traintest","lasso_model" ,"ridge_model", "pred_test", "AUC_calc", "brier_calc", "calibration_calc", 
                                            "R2_calc", "sens_spec_calc", "MSPE","replace_y"  ,"cond_12_new"), envir=environment())


clusterEvalQ(clust, c(library(mice), library(pROC), library(glmnet)))

### MCAR ###

### datasets ###

#set.seed(395)
#data_spec_1 <- call_datasim(scenario = 1, n = 50000)

#set.seed(45571)
#data_spec_2 <- call_datasim(scenario = 3, n = 50000)

data_spec_1 <- readRDS("data_spec_1save")
data_spec_2 <- readRDS("data_spec_2save")
mis_pattern <- readRDS("mis_patt20")

## run all conditions for lasso

# n = 200, mis_prop = .25, cor = ind, lasso
set.seed(135)
clusterSetRNGStream(cl =clust, iseed = 135)
MCAR_1_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)


# n = 1000, mis_prop = .25, cor = ind, lasso
set.seed(1351)
clusterSetRNGStream(cl =clust, iseed = 1351)
MCAR_2_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = ind, lasso
set.seed(1352)
clusterSetRNGStream(cl =clust, iseed = 1352)
MCAR_3_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = ind, lasso
set.seed(1353)
clusterSetRNGStream(cl =clust, iseed = 1353)
MCAR_4_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

## run all conditions for ridge


# n = 200, mis_prop = .25, cor = ind, ridge
set.seed(1354)
clusterSetRNGStream(cl =clust, iseed = 1354)
MCAR_1_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = ind, ridge
set.seed(1355)
clusterSetRNGStream(cl =clust, iseed = 1355)
MCAR_2_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = ind, ridge
set.seed(1356)
clusterSetRNGStream(cl =clust, iseed = 1356)
MCAR_3_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = ind, ridge
set.seed(1357)
clusterSetRNGStream(cl =clust, iseed =1357)
MCAR_4_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MCAR_1_lasso, "MCAR_1_lasso")
saveRDS(MCAR_2_lasso, "MCAR_2_lasso")
saveRDS(MCAR_3_lasso, "MCAR_3_lasso")
saveRDS(MCAR_4_lasso, "MCAR_4_lasso")
saveRDS(MCAR_1_ridge, "MCAR_1_ridge")
saveRDS(MCAR_2_ridge, "MCAR_2_ridge")
saveRDS(MCAR_3_ridge, "MCAR_3_ridge")
saveRDS(MCAR_4_ridge, "MCAR_4_ridge")


## run all conditions for lasso


# n = 200, mis_prop = .25, cor = 0.3, lasso
set.seed(1358)
clusterSetRNGStream(cl =clust, iseed =1358)
MCAR_5_lasso <- sim(k_def = 3, data_spec = data_spec_2 , nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = 0.3, lasso
set.seed(1359)
clusterSetRNGStream(cl =clust, iseed = 1359)
MCAR_6_lasso <- sim(k_def = 3, data_spec = data_spec_2 , nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = 0.3, lasso
set.seed(239)
clusterSetRNGStream(cl =clust, iseed = 239)
MCAR_7_lasso <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = 0.3, lasso
set.seed(2391)
clusterSetRNGStream(cl =clust, iseed =2391)
MCAR_8_lasso <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)



# n = 200, mis_prop = .25, cor = 0.3, ridge
set.seed(2392)
clusterSetRNGStream(cl =clust, iseed =2392)
MCAR_5_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = 0.3, ridge
set.seed(2393)
clusterSetRNGStream(cl =clust, iseed = 2393)
MCAR_6_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = 0.3, ridge
set.seed(2394)
clusterSetRNGStream(cl =clust, iseed = 2394)
MCAR_7_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = 0.3, ridge
set.seed(2395)
clusterSetRNGStream(cl =clust, iseed = 2395)
MCAR_8_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MCAR_5_lasso, "MCAR_5_lasso")
saveRDS(MCAR_6_lasso, "MCAR_6_lasso")
saveRDS(MCAR_7_lasso, "MCAR_7_lasso")
saveRDS(MCAR_8_lasso, "MCAR_8_lasso")
saveRDS(MCAR_5_ridge, "MCAR_5_ridge")
saveRDS(MCAR_6_ridge, "MCAR_6_ridge")
saveRDS(MCAR_7_ridge, "MCAR_7_ridge")
saveRDS(MCAR_8_ridge, "MCAR_8_ridge")

