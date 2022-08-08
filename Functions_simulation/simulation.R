

################################################################################
## Load required packages and functions
################################################################################
# Install DataCpp first, this package contains only a single self-written function
# to generate the data partly in C++

# devtools::build("DataCpp")
# devtools::install("DataCpp")
library(dplyr)
library(purrr)
library(magrittr)
library(tidyr)
library(furrr)
library(mice)

source("simulation_functions.R")

################################################################################
## Test model specifications
################################################################################

set.seed(123)

n <- 1000000
r2 <- 0.25

rel_weight <- c(rep(1:5, each=2) * rep(c(-1,1), times=5), rep(0,10))
cormat <- get_cormat(0.3, rel_weight)


dat <- sim_dat(n = n, 
               r2 = r2, 
               prevalence = 0.1,
               rel_weight = rel_weight, 
               cormat = cormat)

mean(dat$Y) # prevalence of 0.1

fit <- glm(Y ~ ., data = dat, family = binomial)

performance::r2_mckelvey(fit) # r2 of 0.25
summary(fit)
get_coefs((r2 * pi^2/3) / (1 - r2), rel_weight, cormat)

## All specifications seem correct; lets check a different set of specifications

rel_weight <- c(rep(1:5, each = 2) * rep(c(-1,1), times = 5))
cormat <- get_cormat(0, rel_weight)

dat <- sim_dat(n = n, 
               r2 = r2, 
               prevalence = 0.5,
               rel_weight = rel_weight, 
               cormat = cormat)

mean(dat$Y) # prevalence of 0.1

fit <- glm(Y ~ ., data = dat, family = binomial)

performance::r2_mckelvey(fit) # r2 of 0.25
summary(fit)
get_coefs((r2 * pi^2/3) / (1 - r2), rel_weight, cormat)

## All specifications seem correct again.

################################################################################
## Create matrix with all unique outcome combinations
################################################################################

set.seed(999)

# Create a data.frame with all conditions, ultimately, we will loop over all
# conditions, but that is something for a later moment.

output <- expand_grid(n = c(200, 1000),
                      correlation = c(0, 0.3),
                      rel_weight = list(c(rep(1:5, each=2) * rep(c(-1,1), times=5)),
                                        c(rep(1:5, each=2) * rep(c(-1,1), times=5), rep(0,10))),
                      prevalence = c(0.1, 0.5),
                      mechanism = c("MCAR", "MAR"),
                      proportion = c(0.25, 0.75))

################################################################################
## Run simulations
################################################################################

## Run in loop or futuremap or so {

plan(multisession)




pattern <- miss_pat2(10)

output %>%
    mutate(
      cormat = map2(.x = correlation, .y = rel_weight, ~get_cormat(.x, .y)),
      #pattern = map(rel_weight, ~miss_patt(length(.x), 7:3)),
      pattern = map(rel_weight, function(x) {
        cbind(pattern, matrix(1, nrow = nrow(pattern), ncol = length(x) - 10))
      }),
      complete = pmap(list(n, r2 = 0.25, prevalence, rel_weight, cormat),
                      function(n, r2, prevalence, rel_weight, cormat) {
                        sim_dat(n, r2, prevalence, rel_weight, cormat)
                      }),
      amputes = pmap(list(complete, mechanism, proportion, pattern),
                     function(complete, mechanism, proportion, pattern) {
                       mice::ampute(data = complete, prop = proportion,
                                    patterns = pattern, mech = mechanism) %$%
                         amp
                     }),
      models = list(c(mod_logistic = "logistic",
                      mod_lasso = "lasso",
                      mod_ridge = "ridge",
                      mod_rf = "rf")),
      method = list(c("MIbefy", "MIbefminusy", "MICVsep", "MICVreuse"))
      ) %>%
  unnest(method) %>%
  mutate(imp_and_fit = map2(method, data, ~imp_and_fit(method = .x, data = .y)))
  unnest_wider(models)
  

impute <- function(amputes, cv_method, nfolds = 5, m = 5, maxit = 15, imp_method = "pmm") {
  if (!cv_method %in% c("MIbefy", "MIbefminusy", "MICVsep", "MICVreuse")) {
    stop("The specified method is incorrect, specify one of the following methods: MIbefy, MIbefminusy, MICVsep, MICVreuse")
  }
  if (sum(is.na(amputes)) < 1) {
    stop("The data does not contain missings, make sure that the data is incomplete.")
  }
  
  folds <- map(1:m, ~caret::createFolds(amputes$Y, k = nfolds, list = F))
  
  if (cv_method == "MIbefy") {
    mice::mice(amputes, m = m, method = imp_method, maxit = maxit, print = F) %>% 
      complete("all")
  }
  
  if (method == "MIbefminusy") {
    
  }
  
}

out[1, ] %>%
  mutate(imp = map2(amputes, method, ~impute(.x, .y))) %$%
  imp
  

set.seed(123)

x <- rbinom(1000, 1, 0.1)

caret::createFolds(x) %>%
  map(~x[.x] %>% table)


data.frame(y = rbinom(1000, 1, 0.1),
           x1 = rnorm(1000),
           x2 = rnorm(1000),
           x3 = rnorm(1000))

      # amputes = pmap(list(complete, mechanism, proportion, pattern),
      #                function(complete, mechanism, proportion, pattern) {
      #                  mice::ampute(data = complete, prop = proportion,
      #                               patterns = pattern, mech = mechanism)$amp
      #                }),
      # mids = map(amputes, ~mice::mice(.x, maxit = 15)),
      # imps = pmap(list(n, r2 = 0.25, prevalence, rel_weight, cormat, mechanism, proportion, pattern),
      #                 function(n, r2, prevalence, rel_weight, cormat, mechanism, proportion, pattern) {
      #                   sim_dat(n, r2, prevalence, rel_weight, cormat) %>%
      #                     mice::ampute(., prop=proportion, patterns=pattern, mech=mechanism) %$%
      #                     amp %>%
      #                     mice::mice(maxit = 15, print=F) %>%
      #                     complete("all")
      #                 }),




# } end loop / futuremap here

out$pattern[[1]]
out$amputes[[1]]

patt <- expand.grid(rep(list(0:1), 10))[2:1023,]
mice:::check.patterns(patt, 1/nrow(patt), prop=0.5)

DataCpp::mvrnormArma(1000, rep(0,10), diag(10)) %>%
  data.frame() %>%
  mice::ampute(patterns = expand.grid(rep(list(0:1), 10))[2:1023,]) %$%
  amp %>%
  ggmice::plot_pattern()
  
mice::ampute.default.patterns(100)

head(expand.grid(rep(list(0:1), 10)))

mice:::check.patterns(expand.grid(rep(list(0:1), 10))[2:1023,], prop=0.5)

out %>%
  select(-imps) %>%
  mutate(model = rep(list(c("logistic", "lasso", "ridge", "rf")), nrow(.))) %>%
  unnest(model)


