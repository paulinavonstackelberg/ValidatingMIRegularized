# How to validate regularized regression models on incomplete data

This repository contains all files needed to replicate the results obtained in the simulation study conducted for the paper "How to validate regularized regression models on incomplete data". 

## Abstract

When building prediction models using real-world data, two procedures often needto be combined: i) multiple imputation of missing data, and ii) performing cross-validation to estimate future performance of the model. While this issue has beeninvestigated in the context of unpenalized logistic regression, prediction modelsbuilt using LASSO or Ridge regression have only gained sparse attention. It is cur-rently not clear how multiple imputation and cross-validation should be combinedwhen validating prediction models built using regularized regression, and what effectdifferent procedures have on properties such as shrinkage. In the current study, I con-ducted Monte Carlo simulations to investigate at which point imputation should beperformed, as well as to highlight how different methodologies affect discriminationand calibration. I introduced and evaluated a novel approach in which the imputa-tion model is built on the training set, and subsequently used to impute the test set.This new method was compared to three pre-existing strategies that have previouslybeen shown to lead to optimistic or pessimistic estimates. In the first two proce-dures, imputation was performed before cross-validation, while in- or excluding theoutcome from the validation fold during imputation. In the third strategy, separate imputation models were fit in training and validation sets. The results show that usingthe novel method lead to low bias on most measures, even under high missingnessand in smaller samples: when CV is used for internal validation in the given settings,I therefore recommend to base the imputation model for MI on the training set 


## Contents

This repository is split into several folders. 

- `Documents`: contains the manuscript file, as well as the ethical review document.
- `Functions`: contains all functions needed to replicate this study.
- `Supplementary_mat`: contains the supplementary plots mentioned in the manuscript.
