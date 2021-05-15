# How to validate regularized regression models on incomplete data

This repository contains all files needed to replicate the simulation study presented in *"How to validate regularized regression models on incomplete data"*. This project served as my MSc thesis, and was completed at the [Methodology & Statistics department](https://www.uu.nl/en/organisation/methodology-and-statistics) at Utrecht University, Netherlands.

## Simulation period and manuscript submission

**Author**: Paulina von Stackelberg

**Preparation & simulation period**: February 2021 - April 2021

**Date manuscript submission**: May 10, 2021

**Date archive submission**: May 15, 2021

## Abstract

When building prediction models using real-world data, two procedures often need to be combined: i) multiple imputation of missing data, and ii) performing cross-validation to estimate out-of-sample performance of the model. While this topic has previously been investigated in the context of unpenalized logistic regression, prediction models built using LASSO or Ridge regression have only gained sparse attention. It is currently not clear how multiple imputation and cross-validation should be combined in these situations, and what effect different procedures have on properties such as shrinkage. Commonly used methods to deal with missingness in internal validation settings include imputing the data before validation, or imputing training and validation folds separately. Past research has however shown that these strategies lead to optimistic or pessimistic bias. In this study, I introduced and evaluated a novel approach (MI-CVreuse) in which the imputation model is built on the training set and re-used to impute the test set. To compare this method to pre-existing strategies, Monte Carlo simulations were conducted to highlight how different imputation approaches affect discrimination and calibration. The results showed that using MI-CVreuse lead to low bias on most considered performance measures, even under difficult conditions with high missingness and a smaller sample size. Therefore, I recommend to treat imputation the same way as other supervised pre-processing steps in cross-validation: the imputation model should be based on the training set only.


## Contents

This repository is split into several folders, organized following the sequence of steps taken when conducting this study. 

1) `Functions_simulation`: contains all functions (& corresponding descriptions) needed to replicate this study. Here, you can also find brief instructions how to run this simulation study.
2) `Datasets`: contains the N=50000 datasets generated for the current study as well as the two missingness patterns.
3) `Analysis`: contains the script needed to generate the tables, as well as the script used to generate plots.
4) `Results`: contains the results obtained in this simulation study.
5) `Documents`: contains the ethical review document and the thesis manuscript (& corresponding TeX files).
6) `Supplementary_figures`: contains the supplementary figures for all conditions. Here, you can find the PDF [`Supplementary_file`](https://github.com/paulinavonstackelberg/ValidatingMIRegularized/blob/main/Supplementary_figures/Supplementary_file.pdf) which is referred to in the manuscript (S1, S2).

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
| `tidyverse` | 1.3.0    | Data wrangling                                       |
| `ggplot2`   | 3.3.2    | Data visualization                                   |

## Privacy and ethical approval

As this is a simulation study in which not only missingness, but also the data were simulated, no privacy issues had to be dealt with. This study was approved by the Ethics Committee at Utrecht University (registration number 21-0473). The corresponding PDF can be found in the folder `Documents`.

## Permission and access

The data from this study are stored in the archives belonging to Utrecht University for a minimum duration of 10 years. All code needed to reproduce the simulation study is available via [Github](https://github.com/paulinavonstackelberg/ValidatingMIRegularized). *Note*: the output files are too big to be stored on Github (but will be stored on the university servers) - however, all R scripts are provided and therefore replication is possible. Explanations regarding the functions and procedure can be found in `Functions_simulation`.

## Contact

For questions or suggestions, please contact the person responsible for this archive.

**Name**: Paulina von Stackelberg 

**Email**: paulina-von-stackelberg@web.de



