#' Build cross-validated XGB model using train data
#'
#' Build cross-validated XBG model using train data. Uses caret to do CV.
#'
#' @param data_train train set X (imputed)
#' @param y_train train set - outcome
#' @param return_var_imp return variable importance for the prediction model?
#'
#' @return complete XBG model

fit_XGB_CV_caret <- function(data_train, y_train, return_var_imp = FALSE){

  # hyperparameter grid
  # TODO: bigger grid? check timings
  XGB_grid <- expand.grid(nrounds =  c(100, 500),
                          eta = c(0.01, 0.1),
                          max_depth = c(2, 4),
                          gamma = 0.01,
                          colsample_bytree = c(0.3, 0.7),
                          min_child_weight = 1,
                          subsample = c(0.3, 0.5))

  if (is.numeric(y_train)){
    ctrl <- caret::trainControl(method = "cv",
                                number = n_fold)
  } else {

    # brier_summary <- function(data, lev = NULL, model = NULL) {
    #
    #   y <- data$obs
    #   unique_vals <- levels(y)
    #   y_binary <- matrix(ncol = length(unique_vals), nrow = length(y))
    #   colnames(y_binary) <- unique_vals
    #
    #   for (i in 1:length(unique_vals)) {
    #     y_binary[,i] <- ifelse(y == unique_vals[[i]], 1, 0)
    #   }
    #
    #   probabilities <- data[,lev]
    #
    #   brier_score <- mean(rowSums((probabilities - y_binary)^2))
    #
    #   c(brier = brier_score, twoClassSummary(data, lev = lev))
    # }

    ctrl <- caret::trainControl(method = "cv",
                                number = n_fold,
                                classProbs = TRUE,
                                #summaryFunction = brier_summary,
                                summaryFunction = mnLogLoss,
                                selectionFunction = "best")
  }

  # train the model
  if (is.numeric(y_train)){
    start_time_total <- Sys.time()
    XGB_model <- caret::train(x = as.matrix(data_train),
                              y = y_train,
                              method = "xgbTree",
                              metric = "RMSE",
                              trControl = ctrl,
                              tuneGrid = XGB_grid,
                              verbosity = 0)
    time_total <- as.numeric(difftime(Sys.time(), start_time_total, units = "mins"))
    print(sprintf("XGB CV time: %s minutes", round(time_total, 2)))

  } else {
    start_time_total <- Sys.time()
    XGB_model <- caret::train(x = as.matrix(data_train),
                              y = y_train,
                              method = "xgbTree",
                              #metric = "brier",
                              metric = "logLoss",
                              maximize = FALSE, # needed for Brier score / logloss
                              trControl = ctrl,
                              tuneGrid = XGB_grid,
                              verbosity = 0)
    time_total <- as.numeric(difftime(Sys.time(), start_time_total, units = "mins"))
    print(sprintf("XGB CV time: %s minutes", round(time_total, 2)))
  }

  return(XGB_model)
}
