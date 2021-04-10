#############################
### Creating the CV folds ###
#############################

library(caret)

CV_fold <- function(n_param, data, k_def){
  
  # define folds
  folds <- createFolds(data[,(n_param+1)], k = k_def, list = TRUE, returnTrain = FALSE)
  
  # if there is one fold in which no minority case enters the fold, resample
  
  # data[5,] == v[2,] # checked if folds works -> it does
  
  if (k_def == 3){ 
    
    if((sum(as.numeric(data[folds[[1]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[2]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[3]], (n_param+1)])-1) == 0)){
      while((sum(as.numeric(data[folds[[1]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[2]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[3]], (n_param+1)])-1) == 0)){
        folds <- createFolds(data[,(n_param+1)], k= k_def, list = TRUE, returnTrain = FALSE)
      }
    }
  } else{
    if((sum(as.numeric(data[folds[[1]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[2]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[3]], (n_param+1)])-1) == 0)
       || (sum(as.numeric(data[folds[[4]], (n_param+1)])-1) == 0) || (sum(as.numeric(data[folds[[5]], (n_param+1)])-1) == 0)){
      while((sum(as.numeric(data[folds[[1]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[2]], (n_param+1)])-1) == 0) ||(sum(as.numeric(data[folds[[3]], (n_param+1)])-1) == 0)
            || (sum(as.numeric(data[folds[[4]], (n_param+1)])-1) == 0) || (sum(as.numeric(data[folds[[5]], (n_param+1)])-1) == 0)){
        folds <- createFolds(data[,(n_param+1)], k= k_def, list = TRUE, returnTrain = FALSE)
      }
    }
  }
  return(folds)
}


CV_fold_extract <- function(folds, data, nsim, k_def){
  
  dat_split <- list()
  
  if (k_def == 3){
    
    for(i in 1:nsim){
      fold1 <- data[[i]][folds[[i]][["Fold1"]],]
      fold2 <- data[[i]][folds[[i]][["Fold2"]],]
      fold3 <- data[[i]][folds[[i]][["Fold3"]],]
      data_folds <- list("fold1" = fold1, "fold2" = fold2, "fold3" = fold3)
      dat_split[[i]] <- data_folds
    }
  } else{
    for(i in 1:nsim){
      fold1 <- data[[i]][folds[[i]][["Fold1"]],]
      fold2 <- data[[i]][folds[[i]][["Fold2"]],]
      fold3 <- data[[i]][folds[[i]][["Fold3"]],]
      fold4 <- data[[i]][folds[[i]][["Fold4"]],]
      fold5 <- data[[i]][folds[[i]][["Fold5"]],]
      
      data_folds <- list("fold1" = fold1, "fold2" = fold2, "fold3" = fold3, "fold4" = fold4, "fold5" = fold5)
      dat_split[[i]] <- data_folds
    }
  }
  
  return(dat_split)
}


CV_fold_extract_imp <- function(folds_defi, con1_dat, nsim, k_def){
  dat_split_it <- list()
  for(i in 1:nsim){
    fold_defi <- folds_defi[[i]]
    data_imp_def <- con1_dat[[i]]
    dat_split <- list()
    for(j in 1:5){
      if (k_def == 3){
        fold1 <- data_imp_def[[j]][fold_defi[["Fold1"]],]
        fold2 <- data_imp_def[[j]][fold_defi[["Fold2"]],]
        fold3 <- data_imp_def[[j]][fold_defi[["Fold3"]],]
        #data_imp_def[[1]][2,] == fold1[1,] # check!
        #data_imp_def[[1]][5,] == fold1[2,]
        # for j = 2
        #data_imp_def[[2]][9,] == fold2[1,]
        
        data_folds <- list("fold1" = fold1, "fold2" = fold2, "fold3" = fold3)
      } else{
        fold1 <- data_imp_def[[j]][fold_defi[["Fold1"]],]
        fold2 <- data_imp_def[[j]][fold_defi[["Fold2"]],]
        fold3 <- data_imp_def[[j]][fold_defi[["Fold3"]],]
        fold4 <- data_imp_def[[j]][fold_defi[["Fold4"]],]
        fold5 <- data_imp_def[[j]][fold_defi[["Fold5"]],]
        data_folds <- list("fold1" = fold1, "fold2" = fold2, "fold3" = fold3, "fold4" = fold4, "fold5" = fold5)
      }
      dat_split[[j]] <- data_folds
    }
    dat_split_it[[i]] <- dat_split
  }
  return(dat_split_it)
}
