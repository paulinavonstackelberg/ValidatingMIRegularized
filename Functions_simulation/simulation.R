

################################################################################
## Load required packages and functions
################################################################################
# Install DataCpp first, this package contains only a single self-written function
# to generate the data partly in C++

# devtools::build("DataCpp")
# devtools::install("DataCpp")
library(DataCpp)
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
      folds  = map(complete, ~caret::createFolds(factor(.x$Y), k = 5, list = FALSE)),
      method = list(c("MIbefy", "MIbefminusy", "MICVsep", "MICVreuse")),
      models = list(c(mod_logistic = "logistic",
                      mod_lasso = "lasso",
                      mod_ridge = "ridge",
                      mod_rf = "rf")),
      )




# } end loop / futuremap here


