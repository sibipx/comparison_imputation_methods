#' Imputation of training and test sets using bagging
#'
#' @param train_X train set
#' @param text_X test set
#' @param results_timing dataframe with timing results
#' @return train set, test set and timing results.
#' Timing results are returned only for the caret function timing, not for any additional steps

impute_bagging <- function(train_X, test_X){

  # in case of failure return NA
  time_bag_train_fit <- NA
  time_bag_train_predict <- NA
  time_bag_test_predict <- NA

  # train model
  start_time_bag_train_fit <- Sys.time()
  bagging_imp_model <- caret::preProcess(train_X,
                                         method = "bagImpute",
                                         verbose = FALSE)
  time_bag_train_fit <- as.numeric(difftime(Sys.time(), start_time_bag_train_fit, units = "secs"))

  # impute train set
  data_train_imp_bag <- tryCatch(
    {
      start_time_bag_train_predict <- Sys.time()
      data_train_imp_bag <- predict(bagging_imp_model,
                                    newdata = train_X)
      time_bag_train_predict <- as.numeric(difftime(Sys.time(),
                                                    start_time_bag_train_predict, units = "secs"))

      data_train_imp_bag
    },
    error=function(cond) {
      return(NULL)
    }
  )

  # impute test set
  data_test_imp_bag <- tryCatch(
    {
      start_time_bag_test_predict <- Sys.time()
      data_test_imp_bag <- predict(bagging_imp_model,
                                   newdata = test_X)
      time_bag_test_predict <- as.numeric(difftime(Sys.time(), start_time_bag_test_predict, units = "secs"))

      data_test_imp_bag
    },
    error=function(cond) {
      message(cond)
      return(NULL)
    }
  )

  timings <- c(train_fit = time_bag_train_fit,
               train_predict = time_bag_train_predict,
               test_predict = time_bag_test_predict)

  return(list(data_train_imp_bag = data_train_imp_bag,
              data_test_imp_bag = data_test_imp_bag,
              timings = timings))

}
