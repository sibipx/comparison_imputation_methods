#' Run missForest imputation and return results
#'
#' Run missForest imputation and return results
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
#' @param predictor_matrix predictor matrix to be passed to missForest
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

run_missForest <- function(data_train_X_miss, data_test_X_miss, y_train, y_test,
                           data_train_X_miss_orig = NULL, data_test_X_miss_orig = NULL,
                           transf_type = "normal", i, j, dataset_name,
                           categorical_vars = NULL,
                           data_train_X_miss_model = NULL,
                           mapping_tables_train_miss = NULL,
                           data_train_X = NULL, data_test_X = NULL,
                           return_var_imp = FALSE,
                           predictor_matrix = predictor_matrix,
                           positive_class = NULL,
                           ...){

  results <- init_results()

  # build method string
  method <- "missForestPredict"
  if (transf_type %in% c("bin", "cont"))
    method <- sprintf("%s - %s", method, transf_type)

  # missForest - train model
  start_time_train_fit <- Sys.time()
  missForest_imp_model <- missForestPredict::missForest(data_train_X_miss,
                                                        save_models = TRUE,
                                                        initialization = "mean/mode",
                                                        verbose = FALSE,
                                                        num.trees = 100,
                                                        predictor_matrix = predictor_matrix,
                                                        maxiter = maxiter_missForest,
                                                        ...)
  time_train_fit <- as.numeric(difftime(Sys.time(), start_time_train_fit, units = "secs"))

  # keep imputation model object size
  imp_model_size <- as.numeric(object.size(missForest_imp_model))

  # add object size
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            obj_size_bytes = imp_model_size,
            method = method)

  # add timings
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = time_train_fit,
            method = method,
            type = "train_fit")

  # missForest - impute train
  start_time_train_predict <- Sys.time()
  data_train_imp <- missForest_imp_model$ximp
  time_train_predict <- as.numeric(difftime(Sys.time(), start_time_train_predict, units = "secs"))

  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = time_train_predict,
            method = method,
            type = "train_predict")

  # impute test
  start_time_test_predict <- Sys.time()
  data_test_imp <- missForestPredict(missForest_imp_model,
                                     newdata = data_test_X_miss)
  time_test_predict <- as.numeric(difftime(Sys.time(),
                                           start_time_test_predict, units = "secs"))
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = time_test_predict,
            method = method,
            type = "test_predict")

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

  # OOB error
  last_OOB_err <- get_last_OOB_error(missForest_imp_model)
  last_OOB_err <- last_OOB_err %>%
    mutate(iteration_i = i, iteration_j = j, type = "OOB", dataset = dataset_name,
           method = method)

  results <- results %>% add_row(last_OOB_err)

  n_iter_missFP <- missForest_imp_model$OOB_err %>% filter(!is.na(NMSE)) %>%
    pull(iteration) %>% max()

  results <- results %>% add_row(iteration_i = i, iteration_j = j,
                                 metric = "n_iter_converge", dataset = dataset_name,
                                 method = method,
                                 value = n_iter_missFP)

  # build and evaluate all prediction models
  results_pred_models <- run_pred_models(data_train_imp, data_test_imp,
                                         y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                         positive_class = positive_class,
                                         data_train_X_miss)

  results <- results %>% add_row(results_pred_models)

  return(results)
}
