# How to validate regularized regression models on incomplete data

This repository contains all files needed to replicate the results obtained in the simulation study conducted for the paper "How to validate regularized regression models on incomplete data". 

Abstract

When building prediction models using real-world data, two procedures often need to be combined: 1) multiple imputation of missing data, and 2) performing cross-validation to estimate future performance of the model. Currently, it is not fully explored how these two steps need to be combined to get as close as possible to an internal validation setting without missingness. In the present study, I conduct Monte Carlo simulations to investigate at which point the imputation should be performed, as well as the methodological consequences resulting from the different possible procedures. Three existing methods are compared to a novel approach in which the imputation model is built on the training set and subsequently applied to the test set. This is done with a focus on prediction models built using LASSO and Ridge regression. The results show that using the novel method leads to low bias on many measures even under high missingness, but that attention should be paid to the calibration of the prediction model.



