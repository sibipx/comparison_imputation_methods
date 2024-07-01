#' Run knn imputation and return results
#'
#' Run knn imputation and return results
#'
#' @param data_train_X_miss train set with missing values (expected already transformed)
#' @param data_test_X_miss test set with missing values (expected already transformed)
#' @param data_train_X_miss_orig original train set with missing values (not transformed)
#' @param data_test_X_miss_orig original test set with missing values (not transformed)
#' @param i iteration i
#' @param j iteration j
#' @param data_train_X original train set with no simulated missing values (NULL if the original df already has MVs)
#' @param data_test_X original test set with no simulated missing values (NULL if the original df already has MVs)
#' @param return_var_imp return variable importance for the prediction model?
#' @param dataset_name dataset name
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

run_mean_mode <- function(data_train_X_miss, data_test_X_miss, y_train, y_test,
                    data_train_X_miss_orig = NULL, data_test_X_miss_orig = NULL,
                    i, j,
                    dataset_name,
                    data_train_X = NULL, data_test_X = NULL,
                    return_var_imp = FALSE,
                    positive_class = NULL){

  results <- init_results()

  # build method string
  method <- "mean/mode"

  # impute train / test
  mean_mode_fit <- impute_mean_mode_train(data_train_X_miss)
  data_train_imp <- mean_mode_fit$data
  data_test_imp <- impute_mean_mode_test(data_test_X_miss, mean_mode_fit$imputation_values)

  # keep imputation model object size
  imp_model_size <- as.numeric(object.size(mean_mode_fit$imputation_values))

  # add object size
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            obj_size_bytes = imp_model_size,
            method = method)

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

  # build and evaluate all prediction models
  results_pred_models <- run_pred_models(data_train_imp, data_test_imp,
                                         y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                         positive_class = positive_class,
                                         data_train_X_miss)

  results <- results %>% add_row(results_pred_models)

  return(results)
}
