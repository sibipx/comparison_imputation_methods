#' Imputation of training and test sets using knn
#'
#' @param train_X train set
#' @param text_X test set
#' @param results_timing dataframe with timing results
#' @return train set, test set and timing results.
#' Timing results are returned only for the caret function timing, not for any additional steps

impute_knn <- function(train_X, test_X){

  # in case of failure return NA
  time_knn_train_fit <- NA
  time_knn_train_predict <- NA
  time_knn_test_predict <- NA

  # train model
  start_time_knn_train_fit <- Sys.time()
  knn_imp_model <- caret::preProcess(train_X,
                                         method =  c("center", "scale", 'knnImpute'),
                                         k = 5,
                                         verbose = FALSE)
  time_knn_train_fit <- as.numeric(difftime(Sys.time(), start_time_knn_train_fit, units = "secs"))

  # impute train set
  data_train_imp_knn <- tryCatch(
    {
      start_time_knn_train_predict <- Sys.time()
      data_train_imp_knn <- predict(knn_imp_model,
                                    newdata = train_X,
                                    method =  c("center", "scale", 'knnImpute'))
      time_knn_train_predict <- as.numeric(difftime(Sys.time(),
                                                    start_time_knn_train_predict, units = "secs"))

      # unscale
      data_train_imp_knn <- unscale(data_train_imp_knn,
                                    scale = knn_imp_model$std,
                                    center = knn_imp_model$mean)

      data_train_imp_knn
    },
    error=function(cond) {
      return(NULL)
    }
  )

  # impute test set
  data_test_imp_knn <- tryCatch(
    {
      start_time_knn_test_predict <- Sys.time()
      data_test_imp_knn <- predict(knn_imp_model,
                                   newdata = test_X,
                                   method =  c("center", "scale", 'knnImpute'))
      time_knn_test_predict <- as.numeric(difftime(Sys.time(), start_time_knn_test_predict, units = "secs"))

      # bring back on scale
      data_test_imp_knn <- unscale(data_test_imp_knn,
                                   scale = knn_imp_model$std,
                                   center = knn_imp_model$mean)

      data_test_imp_knn
    },
    error=function(cond) {
      message(cond)
      return(NULL)
    }
  )

  timings <- c(train_fit = time_knn_train_fit,
               train_predict = time_knn_train_predict,
               test_predict = time_knn_test_predict)

  return(list(data_train_imp_knn = data_train_imp_knn,
              data_test_imp_knn = data_test_imp_knn,
              timings = timings))

}
