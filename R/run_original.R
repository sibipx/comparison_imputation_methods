#' Run knn imputation and return results
#'
#' Run knn imputation and return results
#'
#' @param data_train_X original train set with no simulated missing values
#' @param data_test_X original test set with no simulated missing values
#' @param i iteration i
#' @param j iteration j
#' @param return_var_imp return variable importance for the prediction model?
#' @param dataset_name dataset name
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#' @param dataset_name dataset name
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

run_original <- function(data_train_X, data_test_X, y_train, y_test,
                         i, j, dataset_name,
                         return_var_imp = TRUE,
                         positive_class = NULL,
                         save_dataset = FALSE,
                         run_pred_model = TRUE){

  results <- init_results()

  # build method string
  method <- "original"

  if (save_dataset){
    save(data_train_X, file = sprintf("data_imp/train_%s_orig_%s.RData", dataset_name, i))
    save(data_test_X, file = sprintf("data_imp/test_%s_orig_%s.RData", dataset_name, i))
  }

  # build and evaluate all prediction models
  if (run_pred_model){
    results_pred_models <- run_pred_models(data_train_X, data_test_X,
                                           y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                           positive_class = positive_class,
                                           data_train_X_miss = data_train_X)

    results <- results %>% add_row(results_pred_models)
  }

  return(results)
}
