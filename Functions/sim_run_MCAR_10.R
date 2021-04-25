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


cores_choose <- parallel::detectCores() -1 # this leaves one core free
clust <- parallel::makeCluster(cores_choose)
parallel::clusterExport(cl=clust, varlist=c("lasso_select_calc","extract_traintest","lasso_model" ,"ridge_model", "pred_test", "AUC_calc", "brier_calc", "calibration_calc", 
                                            "R2_calc", "sens_spec_calc", "MSPE","replace_y"  ,"cond_12_new"), envir=environment())


clusterEvalQ(clust, c(library(mice), library(pROC), library(glmnet)))


### Condition: 10 covariates ###

#mis_pattern <- miss_patt(n_param= 10)


### datasets ###

#set.seed(3)
#data_spec_3 <- call_datasim(scenario = 2, n = 50000)

#set.seed(4987)
#data_spec_4 <- call_datasim(scenario = 4, n = 50000)


data_spec_3 <- readRDS("data_spec_3save")
data_spec_4 <- readRDS("data_spec_4save")
mis_pattern <- readRDS("mis_patt10")


### MCAR ###



## run all conditions for lasso

# n = 200, mis_prop = .25, cor = ind, lasso

set.seed(222)
clusterSetRNGStream(cl =clust, iseed = 222)
MCAR_1_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = ind, lasso

set.seed(2221)
clusterSetRNGStream(cl =clust, iseed = 2221)
MCAR_2_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = ind, lasso

set.seed(2222)
clusterSetRNGStream(cl =clust, iseed =2222)
MCAR_3_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = ind, lasso

set.seed(2223)
clusterSetRNGStream(cl =clust, iseed = 2223)
MCAR_4_lasso10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

## run all conditions for ridge


# n = 200, mis_prop = .25, cor = ind, ridge

set.seed(2224)
clusterSetRNGStream(cl =clust, iseed = 2224)
MCAR_1_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = ind, ridge

set.seed(2225)
clusterSetRNGStream(cl =clust, iseed = 2225)
MCAR_2_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = ind, ridge

set.seed(2226)
clusterSetRNGStream(cl =clust, iseed = 2226)
MCAR_3_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = ind, ridge

set.seed(2227)
clusterSetRNGStream(cl =clust, iseed = 2227)
MCAR_4_ridge10 <- sim(k_def = 3, data_spec = data_spec_3, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MCAR_1_lasso10, "MCAR_1_lasso10")
saveRDS(MCAR_2_lasso10, "MCAR_2_lasso10")
saveRDS(MCAR_3_lasso10, "MCAR_3_lasso10")
saveRDS(MCAR_4_lasso10, "MCAR_4_lasso10")
saveRDS(MCAR_1_ridge10, "MCAR_1_ridge10")
saveRDS(MCAR_2_ridge10, "MCAR_2_ridge10")
saveRDS(MCAR_3_ridge10, "MCAR_3_ridge10")
saveRDS(MCAR_4_ridge10, "MCAR_4_ridge10")



# n = 200, mis_prop = .25, cor = 0.3, lasso


set.seed(2228)
clusterSetRNGStream(cl =clust, iseed = 2228)
MCAR_5_lasso10 <- sim(k_def = 3, data_spec = data_spec_4 , nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = 0.3, lasso

set.seed(2229)
clusterSetRNGStream(cl =clust, iseed = 2229)
MCAR_6_lasso10 <- sim(k_def = 3, data_spec = data_spec_4 , nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = 0.3, lasso

set.seed(102)
clusterSetRNGStream(cl =clust, iseed =102)
MCAR_7_lasso10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 200, nsim =200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = 0.3, lasso

set.seed(1021)
clusterSetRNGStream(cl =clust, iseed = 1021)
MCAR_8_lasso10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 1)



# n = 200, mis_prop = .25, cor = 0.3, ridge

set.seed(1022)
clusterSetRNGStream(cl =clust, iseed = 1022)
MCAR_5_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = 0.3, ridge

set.seed(1023)
clusterSetRNGStream(cl =clust, iseed = 1023)
MCAR_6_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.25, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = 0.3, ridge

set.seed(1024)
clusterSetRNGStream(cl =clust, iseed = 1024)
MCAR_7_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 200, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = 0.3, ridge

set.seed(1025)
clusterSetRNGStream(cl =clust, iseed =1025)
MCAR_8_ridge10 <- sim(k_def = 3, data_spec = data_spec_4, nsub = 1000, nsim = 200, n_param = 10, mis_prop = 0.75, mis_mech = "MCAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MCAR_5_lasso10, "MCAR_5_lasso10")
saveRDS(MCAR_6_lasso10, "MCAR_6_lasso10")
saveRDS(MCAR_7_lasso10, "MCAR_7_lasso10")
saveRDS(MCAR_8_lasso10, "MCAR_8_lasso10")
saveRDS(MCAR_5_ridge10, "MCAR_5_ridge10")
saveRDS(MCAR_6_ridge10, "MCAR_6_ridge10")
saveRDS(MCAR_7_ridge10, "MCAR_7_ridge10")
saveRDS(MCAR_8_ridge10, "MCAR_8_ridge10")
