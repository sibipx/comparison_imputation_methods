#' Run IterativeImputer imputation and return results
#'
#' Run IterativeImputer imputation and return results
#'
#' @param data_train_X_miss train set with missing values (expected already transformed)
#' @param data_test_X_miss test set with missing values (expected already transformed)
#' @param data_train_X_miss_orig original train set with missing values (not transformed)
#' @param data_test_X_miss_orig original test set with missing values (not transformed)
#' @param transf_type normal (no transformation of categorical variables), bin or cont transformation
#' @param i iteration i
#' @param j iteration j
#' @param categorical_vars vector of categorical variables names
#' @param data_train_X_miss_model model to back-transform binary transformation
#' @param mapping_tables_train_miss mapping table to back-transform continuous transformation
#' @param data_train_X original train set with no simulated missing values (NULL if the original df already has MVs)
#' @param data_test_X original test set with no simulated missing values (NULL if the original df already has MVs)
#' @param return_var_imp return variable importance for the prediction model?
#' @param dataset_name dataset name
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#' @param save_dataset save the train and test imputed datasets
#' @param run_pred_model run prediction model (or only run imputation)
#' @param dataset_name dataset name
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

run_IterativeImputer <- function(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                 data_train_X_miss_orig = NULL, data_test_X_miss_orig = NULL,
                                 transf_type = "normal", i, j,
                                 categorical_vars = NULL,
                                 data_train_X_miss_model = NULL,
                                 mapping_tables_train_miss = NULL,
                                 data_train_X = NULL, data_test_X = NULL,
                                 return_var_imp = FALSE,
                                 dataset_name,
                                 positive_class = NULL,
                                 save_dataset = FALSE,
                                 run_pred_model = TRUE){

  results <- init_results()

  # build method string
  method <- "IterativeImputer"
  if (transf_type %in% c("bin", "cont"))
    method <- sprintf("%s - %s", method, transf_type)

  # prepare train and test (expected already transformed)
  data_train_X_miss[is.na(data_train_X_miss)] <- NaN
  data_test_X_miss[is.na(data_test_X_miss)] <- NaN

  data_train_X_miss_py <- r_to_py(data_train_X_miss)
  data_test_X_miss_py <- r_to_py(data_test_X_miss)

  # impute in python using IterativeImputer
  dfs <- impute_IterativeImputer(data_train_X_miss_py, data_test_X_miss_py)

  data_train_imp <- dfs[[1]]
  data_test_imp <- dfs[[2]]
  timings <- dfs[[3]]
  imp_model_size = dfs[[4]]

  # add timings
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = as.numeric(timings[1,, drop = TRUE]),
            method = method,
            type = colnames(timings))

  # add object size
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            obj_size_bytes = imp_model_size,
            method = method)

  # back-transform dummy variables to categorical variables
  if (transf_type == "bin"){
    data_train_imp <- make_col_categ(data_train_imp, categorical_vars,
                                     data_train_X_miss_model$dropped_levels)
    data_test_imp <- make_col_categ(data_test_imp, categorical_vars,
                                    data_train_X_miss_model$dropped_levels)
  }

  # back-transform continuous variables to categorical variables
  if (transf_type == "cont"){

    data_train_imp <- make_col_categ_from_cont(data_train_imp, categorical_vars,
                                               mapping_tables = mapping_tables_train_miss)
    data_test_imp <- make_col_categ_from_cont(data_test_imp, categorical_vars,
                                              mapping_tables = mapping_tables_train_miss)

  }

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
