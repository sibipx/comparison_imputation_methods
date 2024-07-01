#' Run missForest imputation and return results
#'
#' Run missForest imputation and return results
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
#'
#' @return results

run_miceRanger <- function(data_train_X_miss, data_test_X_miss, y_train, y_test,
                           data_train_X_miss_orig = NULL, data_test_X_miss_orig = NULL,
                           i, j, dataset_name,
                           data_train_X = NULL, data_test_X = NULL,
                           return_var_imp = FALSE,
                           m = 5,
                           vars_to_impute = NULL,
                           positive_class = NULL){

  if(is.null(vars_to_impute)) stop("The variables to be imputed need to be specficied using vars_to_impute")

  results <- init_results()

  # build method string
  method <- "miceRanger"

  cl <- makeCluster(n_cpu)
  registerDoParallel(cl)

  # miceRanger - MI - train model
  start_time_train_fit <- Sys.time()
  miceRanger_imp_model <- miceRanger::miceRanger(data_train_X_miss,
                                                 parallel = TRUE,
                                                 m = m,
                                                 returnModels = TRUE,
                                                 num.trees = 100,
                                                 verbose = FALSE,
                                                 vars = vars_to_impute,
                                                 maxiter = maxiter_miceRanger)
  time_train_fit <- as.numeric(difftime(Sys.time(), start_time_train_fit, units = "secs"))
  stopCluster(cl)
  registerDoSEQ()

  # keep imputation model object size
  imp_model_size <- as.numeric(object.size(miceRanger_imp_model))

  # add object size
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            obj_size_bytes = imp_model_size,
            method = method)

  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = time_train_fit,
            method = method,
            type = "train_fit")

  # miceRanger - impute train
  start_time_train_predict <- Sys.time()
  data_train_imp <- lapply(completeData(miceRanger_imp_model), as.data.frame)
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

  # miceRanger - impute test
  start_time_test_predict <- Sys.time()
  data_test_imp <- miceRanger::impute(data.table(data_test_X_miss),
                                      miceRanger_imp_model, verbose = FALSE)
  data_test_imp <- lapply(data_test_imp$imputedData, as.data.frame)
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

  # OOB error
  last_OOB_err <- miceRanger_imp_model$finalError %>% colMeans()
  last_OOB_err <- tibble(variable = names(last_OOB_err), value = last_OOB_err)
  last_OOB_err <- last_OOB_err %>% filter(variable != "dataset")
  var_types <- sapply(data_train_imp, class)
  last_OOB_err$var_type <- NA

  for (col in colnames(data_train_imp)){
    if ("factor" %in% var_types[col] | "character" %in% var_types[col]){
      last_OOB_err[last_OOB_err$variable == col,"var_type"] <- "factor"
    } else {
      last_OOB_err[last_OOB_err$variable == col,"var_type"] <- "numeric"
    }
  }

  last_OOB_err <- last_OOB_err %>%
    mutate(NMSE = if_else(var_type == "numeric", 1 - value, NA_real_),
           MER = if_else(var_type == "factor", 1 - value, NA_real_)) %>%
    select(-c(value, var_type)) %>%
    pivot_longer(cols = c("NMSE", "MER"), names_to = "metric") %>%
    filter(!is.na(value))

  last_OOB_err <- last_OOB_err %>%
    mutate(iteration_i = i, iteration_j = j, type = "OOB", dataset = dataset_name,
           method = "miceRanger")

  results <- results %>% add_row(last_OOB_err)

  # build and evaluate all prediction models
  results_pred_models <- run_pred_models(data_train_imp, data_test_imp,
                                         y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                         positive_class = positive_class,
                                         data_train_X_miss)

  results <- results %>% add_row(results_pred_models)

  return(results)

}
