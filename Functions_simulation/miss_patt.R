######################################
### Generating missingness pattern ###
######################################

# 5 (joint) missingness patterns are generated. NOTE: only the covariates are missing; the outcome is observed.
# I will randomly choose which variables have the missingness

miss_patt <- function(n_param){
  
  set.seed(999) # set a seed to make sure this is fixed for everyone; of course, you can also use your own seed if the goal is not to reproduce my results
  
  pattern_1 <- matrix(data = rep(x = 1, times = 10)) 
  pattern_1[c(sample(seq(1,10), size = 7, replace = F))] <- 0
  pattern_1 <- t(pattern_1)
  
  pattern_2 <- matrix(data = rep(x = 1, times = 10)) 
  pattern_2[c(sample(seq(1,10), size = 6, replace = F))] <- 0
  pattern_2 <- t(pattern_2)
  
  pattern_3 <- matrix(data = rep(x = 1, times = 10))
  pattern_3[c(sample(seq(1,10), size = 5, replace = F))] <- 0
  pattern_3 <- t(pattern_3)
  
  pattern_4 <- matrix(data = rep(x = 1, times = 10)) 
  pattern_4[c(sample(seq(1,10), size = 4, replace = F))] <- 0
  pattern_4 <- t(pattern_4)
  
  pattern_5 <- matrix(data = rep(x = 1, times = 10))
  pattern_5[c(sample(seq(1,10), size = 3, replace = F))] <- 0
  pattern_5 <- t(pattern_5)
  
  pattern_comb_x <- rbind(pattern_1, pattern_2, pattern_3, pattern_4, pattern_5)
  
  if (n_param > 10){ # when the number of covariates is 20: add this to missingness pattern, but the junk variables are never missing
    pattern_comb_x20 <- matrix(data = 1, nrow = 5, ncol = 10)
    pattern_comb_x <- cbind(pattern_comb_x, pattern_comb_x20)
  }
  
  outcome_var <- matrix(data = 1, nrow= nrow(pattern_comb_x), ncol = 1)
  pattern_MAR <- cbind(pattern_comb_x, outcome_var)
  
  return(pattern_MAR)
  
}
