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

cores_choose <- parallel::detectCores() -1 # this leaves one core free
clust <- parallel::makeCluster(cores_choose)


#clust <- makeCluster(n.cores)
parallel::clusterExport(cl=clust, varlist=c("lasso_select_calc","extract_traintest","lasso_model" ,"ridge_model", "pred_test", "AUC_calc", "brier_calc", "calibration_calc", 
                                            "R2_calc", "sens_spec_calc", "MSPE","replace_y"  ,"cond_12_new"), envir=environment())


clusterEvalQ(clust, c(library(mice), library(pROC), library(glmnet)))

### Condition: 20 covariates ###


### datasets ###

#set.seed(395)
#data_spec_1 <- call_datasim(scenario = 1, n = 50000)

#set.seed(45571)
#data_spec_2 <- call_datasim(scenario = 3, n = 50000)

data_spec_1 <- readRDS("data_spec_1save")
data_spec_2 <- readRDS("data_spec_2save")
mis_pattern <- readRDS("mis_patt20")
### MAR ###




## run all conditions for lasso

# n = 200, mis_prop = .25, cor = ind, lasso

set.seed(885)
clusterSetRNGStream(cl =clust, iseed = 885)
MAR_1_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = ind, lasso

set.seed(8851)
clusterSetRNGStream(cl =clust, iseed = 8851)
MAR_2_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = ind, lasso
set.seed(8852)
clusterSetRNGStream(cl =clust, iseed = 8852)
MAR_3_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = ind, lasso
set.seed(8853)
clusterSetRNGStream(cl =clust, iseed = 8853)
MAR_4_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

## run all conditions for ridge

# n = 200, mis_prop = .25, cor = ind, ridge
set.seed(8854)
clusterSetRNGStream(cl =clust, iseed = 8854)
MAR_1_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = ind, ridge
set.seed(8855)
clusterSetRNGStream(cl =clust, iseed = 8855)
MAR_2_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = ind, ridge
set.seed(8856)
clusterSetRNGStream(cl =clust, iseed = 8856)
MAR_3_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = ind, ridge
set.seed(8857)
clusterSetRNGStream(cl =clust, iseed = 8857)
MAR_4_ridge <- sim(k_def = 3, data_spec = data_spec_1, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MAR_1_lasso, "MAR_1_lasso")
saveRDS(MAR_2_lasso, "MAR_2_lasso")
saveRDS(MAR_3_lasso, "MAR_3_lasso")
saveRDS(MAR_4_lasso, "MAR_4_lasso")
saveRDS(MAR_1_ridge, "MAR_1_ridge")
saveRDS(MAR_2_ridge, "MAR_2_ridge")
saveRDS(MAR_3_ridge, "MAR_3_ridge")
saveRDS(MAR_4_ridge, "MAR_4_ridge")





## run all conditions for lasso


# n = 200, mis_prop = .25, cor = 0.3, lasso

set.seed(8858)
clusterSetRNGStream(cl =clust, iseed = 8858)
MAR_5_lasso <- sim(k_def = 3, data_spec = data_spec_2 , nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .25, cor = 0.3, lasso
set.seed(8859)
clusterSetRNGStream(cl =clust, iseed = 8859)
MAR_6_lasso <- sim(k_def = 3, data_spec = data_spec_2 , nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 200, mis_prop = .75, cor = 0.3, lasso
set.seed(714)
clusterSetRNGStream(cl =clust, iseed = 714)
MAR_7_lasso <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim =200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# n = 1000, mis_prop = .75, cor = 0.3, lasso
set.seed(7141)
clusterSetRNGStream(cl =clust, iseed = 7141)
MAR_8_lasso <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim =200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)



# n = 200, mis_prop = .25, cor = 0.3, ridge
clusterSetRNGStream(cl =clust, iseed = 7142)
MAR_5_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .25, cor = 0.3, ridge
set.seed(7143)
clusterSetRNGStream(cl =clust, iseed =7143)
MAR_6_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 200, mis_prop = .75, cor = 0.3, ridge
set.seed(7144)
clusterSetRNGStream(cl =clust, iseed = 7144)
MAR_7_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)

# n = 1000, mis_prop = .75, cor = 0.3, ridge
set.seed(7145)
clusterSetRNGStream(cl =clust, iseed = 7145)
MAR_8_ridge <- sim(k_def = 3, data_spec = data_spec_2, nsub = 1000, nsim = 200, n_param = 20, mis_prop = 0.75, mis_mech = "MAR", mis_pattern = mis_pattern, model = 2)


saveRDS(MAR_5_lasso, "MAR_5_lasso")
saveRDS(MAR_6_lasso, "MAR_6_lasso")
saveRDS(MAR_7_lasso, "MAR_7_lasso")
saveRDS(MAR_8_lasso, "MAR_8_lasso")
saveRDS(MAR_5_ridge, "MAR_5_ridge")
saveRDS(MAR_6_ridge, "MAR_6_ridge")
saveRDS(MAR_7_ridge, "MAR_7_ridge")
saveRDS(MAR_8_ridge, "MAR_8_ridge")
