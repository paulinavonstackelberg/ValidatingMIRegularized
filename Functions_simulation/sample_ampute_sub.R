#################################################
### sampling from the huge dataset & amputing ###
#################################################

# In this function, you can find the code that 1) samples nsim times from the huge dataset 
# and stores these datasets of size n in a list, 2) amputes these size n datasets

library(mice)

sample_sub <- function(n, data_comp){
  data_sub_row <- sample(nrow(data_comp), n, replace = FALSE) # sampling from huge dataset without replacement
  data_sub <- data_comp[data_sub_row,]
  data_ext <- data_comp[-data_sub_row, ] # this is the 'external' dataset
  return(list("data_sub" = data_sub, "data_ext" = data_ext))
}

sample_sub_list <- function(nsim, data_comp, n){
  samp_list <- list()
  for (i in 1:nsim){
    samp_list[[i]] <- sample_sub(n=n, data_comp=data_comp) # do this (subsample) for each of the simulation iterations
  }
  return(samp_list)
}


ampute_sub <- function(data_sub, mis_pattern, mis_prop, mis_mech = c("MCAR", "MAR"), n_param){ 
  if(mis_mech == "MAR"){ # MAR missingness
    amputing <- ampute(data_sub, prop = mis_prop, patterns = mis_pattern, mech = "MAR")
    amputed_dat <-amputing$amp # amputed data
    # re-factor and return amputed data
    amputed_dat[,(n_param+1)] <- amputed_dat[,(n_param+1)] - 1
    amputed_dat[,(n_param+1)] <- as.factor(amputed_dat[,(n_param+1)])
  } else{ # MCAR missingness
    amputing <- ampute(data_sub, prop = mis_prop, patterns = mis_pattern, mech = "MCAR")
    amputed_dat <-amputing$amp # amputed data
  }
  
  return(amputed_dat)
}
