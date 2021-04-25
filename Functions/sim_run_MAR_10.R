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
source("Functions_eval.R")
library(parallel)

#mis_pattern <- miss_patt(n_param= 10)
cores_choose <- parallel::detectCores() -1 # this leaves one core free
clust <- parallel::makeCluster(cores_choose)
parallel::clusterExport(cl=clust, varlist=c("lasso_select_calc","extract_traintest","lasso_model" ,"ridge_model", "pred_test", "AUC_calc", "brier_calc", "calibration_calc", 
                                            "R2_calc", "sens_spec_calc", "MSPE","replace_y"  ,"cond_12_new"), envir=environment())


clusterEvalQ(clust, c(library(mice), library(pROC), library(glmnet)))

### Condition: 10 covariates ###


### datasets ###

#set.seed(3)
#data_spec_3 <- call_datasim(scenario = 2, n = 50000)

#set.seed(4987)
#data_spec_4 <- call_datasim(scenario = 4, n = 50000)


data_spec_3 <- readRDS("data_spec_3save")
data_spec_4 <- readRDS("data_spec_4save")
mis_pattern <- readRDS("mis_patt10")

### MAR ###


## run all conditions for lasso

# n = 200, mis_prop = .25, cor = ind, lasso

set.seed(690)
clusterSetRNGStream(cl =clust, iseed = 690)
MAR_1_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = ind, lasso

set.seed(6901)
clusterSetRNGStream(cl =clust, iseed = 6901)
MAR_2_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = ind, lasso


set.seed(6902)
clusterSetRNGStream(cl =clust, iseed = 6902)
MAR_3_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = ind, lasso

set.seed(6904)
clusterSetRNGStream(cl =clust, iseed = 6904)
MAR_4_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

## run all conditions for ridge


# n = 200, mis_prop = .25, cor = ind, ridge

set.seed(6905)
clusterSetRNGStream(cl =clust, iseed = 6905)
MAR_1_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)



# n = 1000, mis_prop = .25, cor = ind, ridge

set.seed(6906)
clusterSetRNGStream(cl =clust, iseed = 6906)
MAR_2_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = ind, ridge
set.seed(6907)
clusterSetRNGStream(cl =clust, iseed = 6907)
MAR_3_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = ind, ridge
set.seed(6908)
clusterSetRNGStream(cl =clust, iseed = 6908)
MAR_4_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)  


saveRDS(MAR_1_lasso10, "MAR_1_lasso10")
saveRDS(MAR_2_lasso10, "MAR_2_lasso10")
saveRDS(MAR_3_lasso10, "MAR_3_lasso10")
saveRDS(MAR_4_lasso10, "MAR_4_lasso10")
saveRDS(MAR_1_ridge10, "MAR_1_ridge10")
saveRDS(MAR_2_ridge10, "MAR_2_ridge10")
saveRDS(MAR_3_ridge10, "MAR_3_ridge10")
saveRDS(MAR_4_ridge10, "MAR_4_ridge10")



## run all conditions for lasso

# n = 200, mis_prop = .25, cor = 0.3, lasso


set.seed(6909)
clusterSetRNGStream(cl =clust, iseed = 6909)
MAR_5_lasso10 <- sim(k_def = 3, data_spec = data_spec_4 , nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = 0.3, lasso
set.seed(442)
clusterSetRNGStream(cl =clust, iseed =442)
MAR_6_lasso10 <- sim(k_def = 3, data_spec = data_spec_4 , nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = 0.3, lasso
set.seed(4421)
clusterSetRNGStream(cl =clust, iseed = 4421)
MAR_7_lasso10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 200, nsim =200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = 0.3, lasso
set.seed(4422)
clusterSetRNGStream(cl =clust, iseed = 4422)
MAR_8_lasso10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)



# n = 200, mis_prop = .25, cor = 0.3, ridge
set.seed(4423)
clusterSetRNGStream(cl =clust, iseed = 4423)
MAR_5_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = 0.3, ridge
set.seed(4424)
clusterSetRNGStream(cl =clust, iseed = 4424)
MAR_6_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = 0.3, ridge
set.seed(4425)
clusterSetRNGStream(cl =clust, iseed = 4425)
MAR_7_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = 0.3, ridge
set.seed(4426)
clusterSetRNGStream(cl =clust, iseed = 4426)
MAR_8_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MAR_5_lasso10, "MAR_5_lasso10")
saveRDS(MAR_6_lasso10, "MAR_6_lasso10")
saveRDS(MAR_7_lasso10, "MAR_7_lasso10")
saveRDS(MAR_8_lasso10, "MAR_8_lasso10")
saveRDS(MAR_5_ridge10, "MAR_5_ridge10")
saveRDS(MAR_6_ridge10, "MAR_6_ridge10")
saveRDS(MAR_7_ridge10, "MAR_7_ridge10")
saveRDS(MAR_8_ridge10, "MAR_8_ridge10")
