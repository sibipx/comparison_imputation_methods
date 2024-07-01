#' Run linear model imputation and return results
#'
#' Run linear model imputation and return results
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
#'
#' @return results

run_tidy_linear <- function(data_train_X_miss, data_test_X_miss, y_train, y_test,
                            data_train_X_miss_orig = NULL, data_test_X_miss_orig = NULL,
                            transf_type = "normal", i, j,
                            dataset_name,
                            categorical_vars = NULL,
                            data_train_X_miss_model = NULL,
                            mapping_tables_train_miss = NULL,
                            data_train_X = NULL, data_test_X = NULL,
                            return_var_imp = FALSE,
                            positive_class = NULL,
                            vars_to_impute = NULL){

  if(is.null(vars_to_impute)) stop("The variables to be imputed need to be specficied using vars_to_impute")

  results <- init_results()

  # build method string
  method <- "linear"
  if (transf_type %in% c("bin", "cont"))
    method <- sprintf("%s - %s", method, transf_type)

  # impute train / test using mean/mode
  mean_mode_fit <- impute_mean_mode_train(data_train_X_miss)
  data_train_imp_mean <- mean_mode_fit$data
  data_test_imp_mean <- impute_mean_mode_test(data_test_X_miss, mean_mode_fit$imputation_values)

  # prepare recipe for each column using the other columns initialized with mean/mode
  impute_one_by_one <- function(v, data, data_init) {
    x_other_cols <- colnames(data)[v != colnames(data)]
    X_train_to_impute <- cbind(data[,v, drop = FALSE],
                               data_init[,x_other_cols])
    imp_model <- recipes::recipe(x = X_train_to_impute) %>%
      recipes::step_impute_linear(all_of(v),
                                  impute_with = colnames(data)) %>%
      recipes::prep(training = X_train_to_impute)  # prep the recipe

    return(imp_model)
  }

  start_time_train_fit <- Sys.time()
  imp_models <- lapply(vars_to_impute, function(x) impute_one_by_one(x, data_train_X_miss, data_train_imp_mean))
  time_train_fit <- as.numeric(difftime(Sys.time(), start_time_train_fit, units = "secs"))

  # keep imputation model object size
  imp_model_size <- as.numeric(object.size(imp_models))

  # bake recipe on train or test
  bake_model <- function(imp_model, data, data_init, v){
    x_other_cols <- colnames(data)[v != colnames(data)]
    data_to_impute <- cbind(data[,v, drop = FALSE],
                            data_init[,x_other_cols])

    imp_model %>%
      recipes::bake(new_data = data_to_impute) %>%
      select(all_of(v))
  }

  # impute train and test
  start_time_train_predict <- Sys.time()
  data_train_imp <- do.call(cbind, lapply(1:length(vars_to_impute), function(x)
    bake_model(imp_models[[x]], data_train_X_miss,
               data_train_imp_mean,
               vars_to_impute[x])))
  time_train_predict <- as.numeric(difftime(Sys.time(),
                                            start_time_train_predict, units = "secs"))
  # add complete variables
  data_train_imp <- cbind(data_train_imp, data_train_X_miss[!colnames(data_train_X_miss) %in% vars_to_impute])
  data_train_imp <- data_train_imp[,colnames(data_train_X_miss)] # set in original order

  start_time_test_predict <- Sys.time()
  data_test_imp <- do.call(cbind, lapply(1:length(vars_to_impute), function(x)
    bake_model(imp_models[[x]], data_test_X_miss,
               data_test_imp_mean,
               vars_to_impute[x])))
  time_test_predict <- as.numeric(difftime(Sys.time(), start_time_test_predict, units = "secs"))
  # add complete variables
  data_test_imp <- cbind(data_test_imp, data_test_X_miss[!colnames(data_test_X_miss) %in% vars_to_impute])
  data_test_imp <- data_test_imp[,colnames(data_test_X_miss)] # set in original order

  # add timings
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            time_sec = c(time_train_fit, time_train_predict, time_test_predict),
            method = method,
            type = c("train_fit", "train_predict", "test_predict"))

  # add object size
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset  = dataset_name,
            obj_size_bytes = imp_model_size,
            method = method)

  if (!is.null(data_train_imp)) data_train_imp <- data_train_imp %>% as.data.frame()
  if (!is.null(data_test_imp)) data_test_imp <- data_test_imp %>% as.data.frame()

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

  # build and evaluate all prediction models
  results_pred_models <- run_pred_models(data_train_imp, data_test_imp,
                                         y_train, y_test, return_var_imp, dataset_name, i, j, method,
                                         positive_class = positive_class,
                                         data_train_X_miss)

  results <- results %>% add_row(results_pred_models)

  return(results)
}
