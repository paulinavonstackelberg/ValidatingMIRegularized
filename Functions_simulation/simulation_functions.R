

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
  
  bind_cols(X = as.data.frame(X), Y = Y)
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
