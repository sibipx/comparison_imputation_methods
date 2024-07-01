#' Build all models and evaluate test performance.
#'
#' Build all models and evaluate test performance.
#'
#' @param data_train_imp train set X (imputed)
#' @param data_test_imp test set X (imputed)
#' @param y_train train set - outcome
#' @param y_test test set - outcome
#' @param i iteration i
#' @param j iteration j
#' @param dataset_name dataset name
#' @param method imputation method name
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

run_pred_models <- function(data_train_imp, data_test_imp,
                            y_train, y_test, return_var_imp,
                            dataset_name, i, j, method,
                            positive_class,
                            data_train_X_miss){

  results <- init_results()

  # build and evaluate cubic spline model
  if (!skip_cubic_splines){
    results_cubic <- evaluate_linear_model(data_train_imp, data_test_imp, y_train, y_test,
                                           dataset_name, i, j, method,
                                           knots = 3,
                                           positive_class = positive_class,
                                           data_train_X_miss = data_train_X_miss)
    results <- results %>% add_row(results_cubic)
  }

  # build and evaluate RF model
  if (!skip_RF){
    results_pred <- evaluate_RF_model(data_train_imp, data_test_imp,
                                      y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                      positive_class = positive_class)
    results <- results %>% add_row(results_pred)
  }

  # build and evaluate LASSO model
  if (!skip_LASSO){
    results_LASSO <- evaluate_LR_model(data_train_imp, data_test_imp, y_train, y_test,
                                       dataset_name, i, j, method,
                                       alpha = 1,
                                       positive_class = positive_class)
    results <- results %>% add_row(results_LASSO)
  }

  # build and evaluate Ridge model
  if (!skip_Ridge){
    results_Ridge <- evaluate_LR_model(data_train_imp, data_test_imp, y_train, y_test,
                                       dataset_name, i, j, method,
                                       alpha = 0,
                                       positive_class = positive_class)
    results <- results %>% add_row(results_Ridge)
  }

  # build and evaluate XGB model
  if (!skip_XGB){
    results_XGB <- evaluate_XGB_model(data_train_imp, data_test_imp,
                                      y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                      positive_class = positive_class)
    results <- results %>% add_row(results_XGB)
  }

  return(results)
}
