###################################################
### Functions for the generation of the dataset ###
###################################################

# This script contains two functions: 1) imbalance create: optimizes the intercept so that we get a certain outcome proportion 
# (i.e., because I want to ensure that I have roughly 50% cases (Y=1) in my population)

# 2) data sim: calls imbalance_create and creates data for the logistic models

# 3) data sim call: calls data sim and returns the data frame

library(DescTools)



imbalance_create <- function(bs, min, design, betas){
  out <- matrix(0, nrow = length(bs), ncol = 1)
  for (i in 1: length(bs)){
    outcome_new <- bs[i]+ design %*% betas 
    out[i,] = mean(1/(1+exp(-outcome_new)))- min
  }
  return(out)
  
}





# Data simulation function

data_sim <- function(means, vars, cor_cond = c('ind', 'corr'),  n, betas, imb_level, n_param, correl){
  
  # simulate multivariate normal predictors
  
  R <- matrix(numeric(n_param*n_param), nrow = n_param) #correlation matrix
  diag(R) <- 1 #set diagonal to 1
  
  
  # if the cor = 0.3 correlation structure is desired, implement this (if not, then the correlations between 
  # variables will be 0):
  
  if (cor_cond == 'corr'){ # cor=0.3 structure
    R[R < 1] <- correl
  }
  
  
  sigma <- diag(sqrt(vars)) %*% R %*% diag(sqrt(vars)) # variance-covariance matrix
  
  simdata <-
    as.data.frame(mvtnorm::rmvnorm(n = n, mean = means, sigma = sigma)) #create data
  
  
  # simulate the outcome (bernoulli distributed)
  
  design <- as.matrix(simdata)
  outcome <-  design %*% betas 
  
  
  ### Optimize b0 to achieve a certain class (im)balance
  
  bs <- seq(-15, 15, by=0.001)
  imb <- imbalance_create(bs=bs, min=imb_level, design=design, betas=betas) 
  min <- Closest(imb, 0, which = T, na.rm = FALSE)
  b0 <-bs[min]
  
  # make the outcome
  
  outcome_new <- b0+ design %*% betas 
  pr = 1/(1+exp(-outcome_new))  # pass through an inv-logit function
  y <- rbinom(n,1,pr) 
  y <- (factor(y))
  data <- cbind.data.frame(design, y)
  data_b0 <- list('intercept' = b0, 'data' = data)
  
  # return dataset and intercept value
  return(data_b0)
  
}




call_datasim <- function(scenario = c(1, 2, 3, 4), n){
  
  if(scenario == 1){
    
    # scenario 1: 10 good variables, 10 junk variables, no correlation
    
    non_junk <- c(-0.2, 0.2, -0.1, 0.1, -0.4, 0.4, 0.4, -0.4, 0.6, 0.6)
    junk <- rep(0,10) # junk variables
    betas <- c(non_junk, junk)
    means <- rep(0, 20)
    vars <- rep(1, 20)
    data_comp <- data_sim(means, vars, cor_cond = "ind", n=n, betas, imb_level = 0.5, n_param = 20, correl = 0.3)
    
  }else if(scenario == 2){
    # scenario 2: 10 good variables, 0 junk variables, no correlation
    
    non_junk <- c(-0.2, 0.2, -0.1, 0.1, -0.4, 0.4, 0.4, -0.4, 0.6, 0.6)
    betas <- non_junk
    means <- rep(0, 10)
    vars <- rep(1, 10)
    data_comp <- data_sim(means, vars, cor_cond = "ind", n=n, betas, imb_level = 0.5, n_param = 10, correl = 0.3)
    
  }else if(scenario == 3){
    
    # scenario 3: 10 good variables, 10 junk variables, 0.3 correlation
    non_junk <- c(-0.2, 0.2, -0.1, 0.1, -0.4, 0.4, 0.4, -0.4, 0.6, 0.6)
    junk <- rep(0,10) # junk variables
    betas <- c(non_junk, junk)
    means <- rep(0, 20)
    vars <- rep(1, 20)
    data_comp <- data_sim(means, vars, cor_cond = "corr", n=n, betas, imb_level = 0.5, n_param = 20, correl = 0.3)
    
  }else if(scenario == 4){
    
    # scenario 4: 10 good variables, 0 junk variables, 0.3 correlation
    non_junk <- c(-0.2, 0.2, -0.1, 0.1, -0.4, 0.4, 0.4, -0.4, 0.6, 0.6)
    betas <- non_junk
    means <- rep(0, 10)
    vars <- rep(1, 10)
    data_comp <- data_sim(means, vars, cor_cond = "corr", n=n, betas, imb_level = 0.5, n_param = 10, correl = 0.3)
  }
  
  output <- list("betas" = betas, "intercept" = data_comp[["intercept"]], "data" = data_comp[["data"]])
  return(output)
  
}

