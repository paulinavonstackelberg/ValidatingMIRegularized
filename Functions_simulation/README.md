## Functions

This folder contains all functions used in the simulation study. The functions are grouped into different .R files based on their purpose (e.g., a file with evaluation functions, or a file containing functions for CV splitting); a description of these files, and the relevant functions they contain, can be found in the table below. <br/>

## How to run this simulation study

Use the project `Val_MIsim.Rproj`. The most important files are `sim_run_MAR_20.R`, `sim_run_MAR_10.R`, `sim_run_MCAR_20.R` and `sim_run_MCAR_10.R`. In these files, `sim.R` is called for each of the conditions. Therefore, you can run these scripts to generate and save the output for the study. *Note*: As imputation can become computationally intensive when nested in cross-validation, I parallelized parts of this simulation study using the package `parallel` (version 4.0.3) in R, using 7 cores on a personal computer (Linux-based, but this parallelization package also works on Windows-based machines). Below the table describing the functions, you can find a brief example showing you how to run this study. For reproducibility, a seed is always set before starting the simulation for a selected condition; the ones I used can be found in the main scripts (e.g., in `sim_run_MAR_20.R`).




| File name             | Functions contained in the file                                                                                                        | Description of functions                                                                                                                                                                                                                                                                                                                                      |
|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| CV_folds.R            | 1) `CV_fold` <br/>  2) `CV_fold_extract`<br/>  3) `CV_fold_extract_imp`                                                                        | - creating the CV folds <br/>  - extracting the rows in the data corresponding to the respective CV folds <br/>  - extracting the rows in the data corresponding to the respective CV fold per imputed dataset                                                                                                                                                |
| Functions_eval.R      | 1) `brier_calc` <br/>  2) `calibration_calc` <br/>  3) `R2_calc` <br/>  4) `AUC_calc` <br/>  5) `sens_spec_calc` <br/>  6) `lasso_select_calc` <br/> | - file containing the functions to compute the Brier score, calibration, pseudo R2, AUC, sensitivity, specificity, and sensitivity/specificity of selection.                                                                                                                                                                                                  |
| cond12_new.R          | 1) `cond_12_new` <br/>  2) `cond12_impdatloop`                                                                                               | - file for condition 1 (MI-bef(y)) and for the complete data reference scenario <br/>  - CV is completed on each of the five imputed datasets <br/>                                                                                                                                                                                                                                                        |
| cond2_minusy.R        | 1) `cond_2y` <br/>  2) `replace_y`                                                                                                           | - file for condition 2 (MI-bef(-y)) <br/>  - the outcome is deleted from the validation fold, then imputation is performed five times <br/>  - the imputed outcome from validation fold is then replaced with original (real) outcome values <br/>                                                                                                            |
| cond34_new.R          | 1) `cond_3_new` <br/>  2) `cond_4_new`                                                                                                       | - file for conditions 3 (MI-CVreuse) and 4 (MI-CVsep); NOTE: in the manuscript, MI-CVreuse is condition 4, and MI-CVsep is condition 3. This is not a typo. <br/>  - MI-CVreuse: the validation fold is imputed using the imputation model developed on the training set <br/>  - MI-CVsep: the validation fold and training set are imputed separately <br/> |
| data_sim.R            | 1) `data_sim` <br/>  2) `call_datasim` <br/>  3) `imbalance_create`                                                                            | - file used to generate the N=50000 datasets <br/>  - contains functions to control outcome (im)balance, data simulation, preparation of conditions <br/>                                                                                                                                                                                                     |
| lasso_ridge_model.R   | 1) `lasso_model` <br/>  2) `ridge_model` <br/>  3) `extract_traintest`                                                                         | - file containing the code used to fit a Ridge/LASSO regression model on the training set <br/>  - also contains a function to extract the training and test sets per imputed dataset <br/>                                                                                                                                                                   |
| lasso_ridge_predict.R | 1) `pred_test` <br/>  2) `sub_ext_pred`                                                                                                      | - file containing the code in which the model fitted on the training set is used to predict the outcome in the validation fold <br/>                                                                                                                                                                                                                          |
| miss_patt.R           | 1) `miss_patt`                                                                                                                             | - file containing the code used to generate the missingness pattern                                                                                                                                                                                                                                                                                           |
| sample_ampute_sub.R   | 1) `sample_sub` <br/>  2) `sample_sub_list` <br/>  3) `ampute_sub`                                                                             | - file containing the code used to sample from the N=50000 datasets, and to subsequently ampute (generate missingness in) those smaller datasets                                                                                                                                                                                                              |
| sim.R                 | 1) `sim`                                                                                                                                   | - file in which the procedure is initiated: the data is drawn from the large datasets, the missingness is created, and the four conditions are implemented <br/>  - returns results for all four conditions and the reference condition without missingness                                                                                                   |
| sim_run_MAR_10.R      | calling sim.R                                                                                                                            | - file for all conditions with MAR missingness and 10 covariates                                                                                                                                                                                                                                                                                              |
| sim_run_MAR_20.R      | calling sim.R                                                                                                                            | - file for all conditions with MAR missingness and 20 covariates                                                                                                                                                                                                                                                                                              |
| sim_run_MCAR_10.R     | calling sim.R                                                                                                                            | - file for all conditions with MCAR missingness and 10 covariates                                                                                                                                                                                                                                                                                             |
| sim_run_MCAR_20.R     | calling sim.R                                                                                                                            | - file for all conditions with MCAR missingness and 20 covariates                                                                                                                                         



### Example

Here you can find a brief example how to run the following condition: sample size *n*=200, 200 iterations, missingness proportion of 0.25 under a MAR (missing at random) mechanism, LASSO regression. Generally speaking, the following steps are always taken inside of this simulation (as also seen in `sim.R`):

1) Draw a sample (*n*= 200, or 1000) from the *N*= 50000 dataset.
2) Do the cross-validation on the complete (i.e., non-amputed) data. Save results.
3) Ampute the sample (i.e., generate missingness).
4) Do the cross-validation according to the procedure as described in MI-bef(y), MI-bef(-y), MI-CVsep, or MI-CVreuse (see manuscript). Save results.
5) Return all results.

```R

# load all your functions

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

# preparing: initiate the parallelization

cores_choose <- parallel::detectCores() -1 # this leaves one core free; in my simulation, this is set to 7.
clust <- parallel::makeCluster(cores_choose)
parallel::clusterExport(cl=clust, varlist=c("lasso_select_calc","extract_traintest","lasso_model" ,"ridge_model", "pred_test", "AUC_calc", "brier_calc", "calibration_calc", 
                                            "R2_calc", "sens_spec_calc", "MSPE","replace_y"  ,"cond_12_new"), envir=environment())
clusterEvalQ(clust, c(library(mice), library(pROC), library(glmnet)))

# open the missingness pattern and the N=50000 dataset you want

data_spec_1 <- readRDS("data_spec_1save")
mis_pattern <- readRDS("mis_patt20")

# set your seed and run the condition!

set.seed(885)
clusterSetRNGStream(cl =clust, iseed = 885)
MAR_1_lasso <- sim(k_def = 3, data_spec = data_spec_1, nsub = 200, nsim = 200, n_param = 20, mis_prop = 0.25, mis_mech = "MAR", mis_pattern = mis_pattern, model = 1)

# save the output

saveRDS(MAR_1_lasso, "MAR_1_lasso")

```
