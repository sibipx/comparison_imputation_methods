#' Run mice RF imputation and return results
#'
#' Run mice FR imputation and return results
#'
#' @param data_train_X_miss train set with missing values (expected already transformed)
#' @param data_test_X_miss test set with missing values (expected already transformed)
#' @param data_train_X_miss_orig original train set with missing values (not transformed)
#' @param data_test_X_miss_orig original test set with missing values (not transformed)
#' @param i iteration i
#' @param j iteration j
#' @param data_train_X original train set with no simulated missing values (NULL if the original df already has MVs)
#' @param data_test_X original test set with no simulated missing values (NULL if the original df already has MVs)
#' @param m number of imputations for multiple imputation (m = 1 is single imputation)
#' @param dataset_name dataset name
#' @param vars_to_impute variables to impute, as specified by miceRanger
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#' @param dataset_name dataset name
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

run_mice_rf <- function(data_train_X_miss, data_test_X_miss, y_train, y_test,
                        data_train_X_miss_orig = NULL, data_test_X_miss_orig = NULL,
                        i, j, dataset_name,
                        data_train_X = NULL, data_test_X = NULL,
                        return_var_imp = FALSE,
                        m = 5,
                        vars_to_impute = NULL,
                        positive_class = NULL,
                        save_dataset = FALSE,
                        run_pred_model = TRUE){

  if(is.null(vars_to_impute)) stop("The variables to be imputed need to be specficied using vars_to_impute")

  results <- init_results()

  # build method string
  method <- "mice_rf"

  # mice - train model- use futuremice for parallelization
  start_time_train_fit <- Sys.time()
  mice_imp_model <- mice::futuremice(data_train_X_miss,
                                     n.core = 5,
                                     method = "rf",
                                     ntree = 100,
                                     m = 5,
                                     maxit = maxiter_mice)
  time_train_fit <- as.numeric(difftime(Sys.time(), start_time_train_fit, units = "secs"))

  # keep imputation model object size
  imp_model_size <- as.numeric(object.size(mice_imp_model))

  # add object size
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            obj_size_bytes = imp_model_size,
            method = method)

  # add timing train
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = time_train_fit,
            method = method,
            type = "train_fit")

  # mice - impute train
  start_time_train_predict <- Sys.time()
  data_train_imp <- mice::complete(mice_imp_model, "all")
  time_train_predict <- as.numeric(difftime(Sys.time(),
                                            start_time_train_predict, units = "secs"))

  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = time_train_predict,
            method = method,
            type = "train_predict")

  # get one single imputed dataframe from 5 imputed dfs
  data_train_imp <- aggregate_mice_results(data_train_imp)

  # mice - impute test
  start_time_test_predict <- Sys.time()
  data_test_imp <- mice::mice.mids(mice_imp_model,
                                   newdata = data_test_X_miss,
                                   printFlag = FALSE)
  data_test_imp <- mice::complete(data_test_imp, "all")
  time_test_predict <- as.numeric(difftime(Sys.time(), start_time_test_predict, units = "secs"))

  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = time_test_predict,
            method = method,
            type = "test_predict")

  # get one single imputed dataframe from 5 imputed dfs
  data_test_imp <- aggregate_mice_results(data_test_imp)

  # variable-wise train imputation error
  if (!is.null(data_train_X)){

    # train imputation error
    var_wise_results_train <- evaluate_var_wise(data_train_imp, data_train_X_miss_orig, data_train_X,
                                                method, type = "train", dataset_name, i, j)

    results <- results %>%
      add_row(var_wise_results_train)

    # test imputation error
    var_wise_results_test <- evaluate_var_wise(data_test_imp, data_test_X_miss_orig, data_test_X,
                                               method, type = "test", dataset_name, i, j)

    results <- results %>%
      add_row(var_wise_results_test)

  }

  if (save_dataset){
    save(data_train_imp, file = sprintf("data_imp/train_imp_%s_%s_%s.RData", dataset_name, method, i))
    save(data_test_imp, file = sprintf("data_imp/test_imp_%s_%s_%s.RData", dataset_name, method, i))
  }

  # build and evaluate all prediction models
  if (run_pred_model){
    results_pred_models <- run_pred_models(data_train_imp, data_test_imp,
                                           y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                           positive_class = positive_class,
                                           data_train_X_miss)

    results <- results %>% add_row(results_pred_models)
  }

  return(results)

}
