# How to validate regularized regression models on incomplete data

This repository contains all files needed to replicate the simulation study results presented in *"How to validate regularized regression models on incomplete data"*. This project served as my MSc thesis, and was completed at the Methodology & Statistics department at Utrecht University.

## Abstract

When building prediction models using real-world data, two procedures often need to be combined: i) multiple imputation of missing data, and ii) performing cross-validation to estimate out-of-sample performance of the model. While this topic has previously been investigated in the context of unpenalized logistic regression, prediction models built using LASSO or Ridge regression have only gained sparse attention. It is currently not clear how multiple imputation and cross-validation should be combined in these situations, and what effect different procedures have on properties such as shrinkage. Commonly used methods to deal with missingness in internal validation settings include imputing the data before validation, or imputing training and validation folds separately. Past research has however shown that these strategies lead to optimistic or pessimistic bias. In this study, I introduced and evaluated a novel approach (MI-CVreuse) in which the imputation model is built on the training set and re-used to impute the test set. To compare this method to pre-existing strategies, Monte Carlo simulations were conducted to highlight how different imputation approaches affect discrimination and calibration. The results showed that using MI-CVreuse lead to low bias on most considered performance measures, even under difficult conditions with high missingness and a smaller sample size. Therefore, I recommend to treat imputation the same way as other supervised pre-processing steps in cross-validation: the imputation model should be based on the training set only.


## Contents

This repository is split into several folders. 

- `Documents`: contains the ethical review document and the manuscript (& TeX files).
- `Functions_simulation`: contains all functions needed to replicate this study.
- `Datasets`: contains the N=50000 datasets generated for the current study as well as the two missingness patterns.
- `Analysis`: contains the script needed to generate the tables.
- `Results`: contains the results obtained in this simulation study. 
- `Supplementary_figures`: contains the supplementary figures for all conditions. Also, this folder contains the PDF `Supplementary_file` which is referred to in-text.

## Software requirements

This simulation study was conducted in R (version 4.0.3) using RStudio (version 1.2.5042). Below you can find packages used and the specific versions; I cannot guarantee that exactly similar results are obtained with other package versions.

| Package     | Version  | Usage                                                |
|-------------|----------|------------------------------------------------------|
| `MASS`      | 7.3-53   | Generating *N*=50000 datasets                        |
| `DescTools` | 0.99.40  | Generating *N*=50000 datasets                        |
| `mice`      | 3.13.0   | Performing multiple imputation                       |
| `glmnet`    | 4.1-1    | Tuning and fitting LASSO and Ridge regression models |
| `caret`     | 6.0-86   | Splitting the data into folds for cross-validation   |
| `pROC`      | 1.17.0.1 | Calculating the AUC                                  |

## Ethical approval

This simulation study was approved by the Ethics Committee at Utrecht University (registration number 21-0473). The corresponding PDF can be found in the folder `Documents`. All files, including the output, are stored in the archives belonging to Utrecht University. They are stored for a minimum duration of 10 years. 


