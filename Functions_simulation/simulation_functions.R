

# Create the correlation matrix for all variables
get_cormat <- function(cor, vars) {
  cormat <- diag(length(vars))
  cormat[cormat == 0] <- cor
  as.matrix(cormat)
}

# Make the regression coefficients given a prespecified effect size (specified in
# data simulation function)
get_coefs <- function(var_yhat, rel_weight, cormat) {
  sqrt(var_yhat / sum(rel_weight %*% t(rel_weight) * cormat)) * rel_weight
}

# Calculate an intercept based on a known prevalence (to vary class imbalance)
get_intercept <- function(betas, cormat, prevalence) {
  
  sigma <- c(sqrt(t(betas) %*% cormat %*% betas))
  
  integral <- function(x, b0) (1/sqrt(2 * pi)) * exp(-x^2 / 2) / (1 + exp(-b0 - sigma * x))
  
  f_integrate <- function(g, prevalence, g_lower, g_upper, ...) {
    prevalence - integrate(f = g, lower = g_lower, upper = g_upper, ...)$value
  }
  
  uniroot(f = f_integrate,
          interval = c(-99999999, 99999999),
          prevalence = prevalence,
          g = integral,
          g_lower = -Inf,
          g_upper = Inf)$root
}

# Simulate the complete samples
sim_dat <- function(n, r2, prevalence = 0.5, rel_weight, cormat) {
  var_yhat <- (r2 * pi^2/3) / (1-r2)
  coefs <- get_coefs(var_yhat, rel_weight, cormat)
  b0 <- get_intercept(coefs, cormat, prevalence = prevalence)
  
  X <- DataCpp::mvrnormArma(n, mu = rep(0, length(coefs)), sigma = cormat)
  Y <- rbinom(n, 1, 1 / (1 + exp(-(b0 + X %*% coefs))))
  
  bind_cols(X = data.frame(X), Y = Y)
}

# Create the missingness pattern
miss_patt <- function(params, mis_vars = 7:3) {
  sapply(mis_vars, function(x) {
    pat <- matrix(rep(1, 10))
    pat[sample(1:10, x, replace=F)] <- 0
    pat
  }) %>%
    t() %>%
    cbind(1, matrix(1, nrow = length(mis_vars), ncol = params - 10))
}

miss_pat2 <- function(n_pat) {
  
  pat <- list(0:1) %>%
    rep(10) %>%
    expand.grid()
  
  cbind(pat[sample(2:(nrow(pat) - 1), n_pat), ], 1)
}

## Function below is not finished yet, I am still thinking about how to do this
## most efficiently (and how to structure the output in a general way, also 
## dependent on what we decide with respect to the cross-validation)
impute <- function(amputes, complete, cv_method, folds, m = 5, maxit = 15, imp_method) {
  if (!cv_method %in% c("MIbefy", "MIbefminusy", "MICVsep", "MICVreuse")) {
    stop("The specified method is incorrect, specify one of the following methods: 'MIbefy', 'MIbefminusy', 'MICVsep', 'MICVreuse'")
  }
  if (sum(is.na(amputes)) < 1) {
    stop("The data does not contain missings, make sure that the data is incomplete.")
  }
  
  fold_ind <- 1:max(folds)
  names(fold_ind) <- paste0("fold", fold_ind)
  
  if (cv_method == "MIbefy") {
    imp <- mice::mice(amputes, m = m, maxit = maxit, print = F) %>% 
      complete("all")
    
    names(imp) <- paste0("imp", names(imp))
    
    out <- map(imp, function(i) {
      map(fold_ind, function(fold) {
        list(train = i[folds != fold, ],
             test = i[folds == fold, ])
      })
    })
  }
  
  if (cv_method == "MIbefminusy") {
    out <- map(fold_ind, function(fold) {
      amp <- amputes
      amp$Y[folds == fold] <- NA
      out <- mice::mice(amp, m = m, maxit = maxit, print = F) %>%
        complete("all") %>%
        map(function(imp) {
          imp$Y[folds == fold] <- amputes$Y[folds == fold]
          imp
        })
    })
  }
  
  if (cv_method == "MICVsep") {
    out <- map(1:nfolds, function(indices) {
      train <- mice::mice(amp[folds != indices, ], m = m, maxit = maxit, print = F) #%>%
      #complete("all")
      test  <- mice::mice(amp[folds == indices, ], m = m, maxit = maxit, print = F) #%>%
      #complete("all")
      
      list(train, test)
      # imp <- mice::mice(amp, maxit = 0, m = m, print = F) %>% complete("all")
      # 
      # map(1:m, function(x) {
      #   imp[[x]][folds != indices, ] <- train[[x]]
      #   imp[[x]][folds == indices, ] <- test[[x]]
      # })
    })
  }
  
  if (cv_method == "MICVreuse") {
    out <- map(1:nfolds, function(indices) {
      mice::mice(amp, m = m, maxit = maxit, print = F, ignore = folds == indices) %>%
        complete("all")
    })
  }
  
  out
}
