#' Build cross-validated XGB model using train data
#'
#' Build cross-validated XBG model using train data. Uses mlrMBO to do CV.
#'
#' @param data_train train set X (imputed)
#' @param y_train train set - outcome
#' @param return_var_imp return variable importance for the prediction model?
#'
#' @return complete XBG model

fit_XGB_CV <- function(data_train, y_train, return_var_imp = FALSE){

  if (is.numeric(y_train)){
    objective <- "reg:squarederror"
    cv_metric <- "rmse"
    data_train_xgb <- xgb.DMatrix(data = as.matrix(data_train),
                                  label = y_train)
  } else {
    objective <- "binary:logistic"
    cv_metric <- "logloss"
    data_train_xgb <- xgb.DMatrix(data = as.matrix(data_train),
                                  label = if_else(y_train == positive_class,
                                                  TRUE, FALSE))
  }

  n_rounds <- 7000

  # optimize using mlrMBO (see helper functions)
  start_time <- Sys.time()

  runs <- do_bayes(n_design = iters_warmup, of = obj_fun, opt_steps = iters_optim)

  best_fit <- runs$run$x

  message(sprintf("XGB tuning DONE in %s minutes.",
                  difftime(Sys.time(), start_time, units = "mins") %>% as.numeric()))

  # fit final model
  optimal_cv <- xgb.cv(params = best_fit,
                       objective = objective,
                       booster = "gbtree",
                       data = data_train_xgb,
                       nrounds = n_rounds,
                       nthread = n_cpu,
                       nfold = n_fold,
                       prediction = FALSE,
                       showsd = FALSE,
                       early_stopping_rounds = 25,
                       metrics = cv_metric, # use same objective as CV metric
                       verbose = 0,
                       print_every_n = 1000)

  XGB_model <- xgboost(params = best_fit,
                       objective = objective,
                       booster = "gbtree",
                       data = data_train_xgb,
                       nrounds = optimal_cv$best_ntreelimit,
                       verbose = 0)

  return(XGB_model)
}
