
set.seed(1)
# Relative strength of predictors
rel_weight <- c(rep(1:5, each=2) * rep(c(-1,1), times=5), rep(0,10))
# Correlation matrix
cormat <- get_cormat(0.3, rel_weight)
# Complete data
df <- sim_dat(n = 200, r2 = 0.5, prevalence = 0.5, rel_weight, cormat)

glm(Y ~ ., data = df, family = "binomial") %>%
  summary()

allp <- list(0:1) %>%
  rep(10) %>%
  expand.grid()

rows <- sample(2:(nrow(allp)-1), 10)
comp_vars <- matrix(1, 10, 10, dimnames = list(NULL, paste0("Var", 11:20)))
pat <- cbind(allp[rows, ], comp_vars, matrix(1, 10, 1, dimnames = list(NULL, "Y")))

amp <- mice::ampute(df, patterns = pat, prop = 0.25)$amp

folds <- caret::createFolds(factor(amp$Y), k = 5, list = F)



table(folds, amp$Y)

MIbefy <- mice(amp, maxit = 15, m = 5)
MIbefy_comp <- complete(MIbefy, "all")

X <- df %>% select(-Y) %>% as.matrix()
Y <- df %>% select(Y) %>% as.matrix()

mod <- glmnet::cv.glmnet(x = X,
                         y = Y,
                         alpha = 1,
                         family = "binomial")

plot(mod)

plot(mod)

map(MIbefy_comp, ~ glmnet::cv.glmnet(.x %>% select(-Y) %>% as.matrix(),
                                     .x %>% select(Y) %>% as.matrix(),
                                     alpha = 1,
                                     family = "binomial") %>%
      plot())

predict(mod, newx = X, type = "class", s = mod$lambda.min)
plot(mod)
out <- impute(amp, df, "MIbefy", folds, imp_method = "pmm")

mod
###############################################################################

set.seed(123)
# Relative strength of predictors
rel_weight <- c(rep(1:5, each=2) * rep(c(-1,1), times=5))
# Correlation matrix
cormat <- get_cormat(0.5, rel_weight)
# Complete data
df <- sim_dat(n = 100, r2 = 0.25, prevalence = 0.5, rel_weight, cormat)


allp <- list(0:1) %>%
  rep(10) %>%
  expand.grid()

rows <- sample(2:(nrow(allp)-1), 10)
pat <- cbind(allp[rows,], Y = 1)

amp <- mice::ampute(df, patterns = pat)$amp

folds <- caret::createFolds(factor(amp$Y), k = 2, list = F)

out <- impute(amp, df, "MIbefminusy", folds, maxit = 5, imp_method = "pmm")


map(out, function(imp) {
  map(imp, function(fold) {
    train_x <- fold$train %>% dplyr::select(-Y) %>% as.matrix()
    train_y <- fold$train %>% dplyr::select(Y) %>% as.matrix()
    test_x <- fold$test %>% dplyr::select(-Y) %>% as.matrix()
    test_y <- fold$test %>% dplyr::select(Y) %>% as.matrix()
    
    lasso <- lasso(train_x, train_y)
    ridge <- ridge(train_x, train_y)
    
    list(lasso = lasso, ridge = ridge) %>%
      map(~predict(.x, newx = test_x))
  })
})


lasso <- function(train_x, train_y) {

  mod <- glmnet::cv.glmnet(x = train_x, 
                           y = train_y, 
                           alpha = 1, 
                           family = "binomial")
  
  glmnet::glmnet(x = train_x, 
                 y = train_y, 
                 alpha = 1, 
                 family = "binomial", 
                 lambda = mod$lambda.min)
}

ridge <- function(train_x, train_y) {
  
  mod <- glmnet::cv.glmnet(x = train_x, 
                           y = train_y, 
                           alpha = 0, 
                           family = "binomial")
  
  glmnet::glmnet(x = train_x, 
                 y = train_y, 
                 alpha = 0, 
                 family = "binomial", 
                 lambda = mod$lambda.min)
}

