#' takes a dataset (with or without Mvs) and runs N simulations and returns results
#'
#' takes a dataset (with or without Mvs) and runs N simulations and returns results
#'
#' @param data data (with or without missing values)
#' @param outcome_col name of the outcome column
#' @param N_iter number of iterations to run
#' @param dataset_name dataset name (for organizing the results)
#' @param p_miss proportion of MVs to be simulated (in case of datasets without MVs)
#' @param simulate_MV boolean used to simulate MVs on datasets without MVs (should be false for datasets with MVs)
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#' @param predictor_matrix predictor matrix
#' @param sim_MV_func function to simulate missing values; if NULL, MCAR will be simulated
#' @param save_dataset save the train and test imputed datasets
#' @param run_pred_model run prediction model (or only run imputation)
#'
#' @return results

run_simulations <- function(data, outcome_col, N_iter, dataset_name, p_miss,
                            simulate_MV,
                            positive_class = NULL,
                            predictor_matrix = NULL,
                            sim_MV_func = NULL,
                            save_dataset = FALSE,
                            run_pred_model = TRUE){

  data <- as.data.frame(data)

  # check outcome type
  if(!is.numeric(data[,outcome_col, drop = TRUE]) & is.null(positive_class))
    stop("positive_class is required for non-numeric outcomes")

  if (any(is.na(data[,outcome_col, drop = TRUE])))
    stop("Missing values in the outcome column are not supported. The comparison will not run.")

  # keep cont. vs. categ variables
  continuous_vars <- sapply(data, is.numeric)
  continuous_vars <- colnames(data)[continuous_vars]
  categorical_vars <- colnames(data)[!colnames(data) %in% continuous_vars]

  continuous_vars <- continuous_vars[!continuous_vars %in% outcome_col]
  categorical_vars <- categorical_vars[!categorical_vars %in% outcome_col]

  # check if the dataset has binary vs. categ. vars_to_impute. with more categories
  # if only binary, binarizing and making cont has the same effect --> don't do both
  data_types <- data_type(data)

  has_binary <- data_types %>%
    filter(Variable != outcome_col) %>%
    filter(`Variable type` == "categorical, 2 categories") %>%
    nrow() %>% `!=`(0)

  has_categorical <- data_types %>%
    filter(Variable != outcome_col) %>%
    filter(`Variable type` != "categorical, 2 categories",
           str_detect(`Variable type`, "categorical")) %>%
    nrow() %>% `!=`(0)

  if (has_categorical | has_binary) {
    make_binary <- TRUE

    # check that the columns are factor and if not, transform to factor
    is_factor <- sapply(data[,categorical_vars], is.factor)
    is_not_factor <- names(is_factor)[!is_factor]
    if (length(is_not_factor) > 0) {
      print(sprintf("Variable(s) %s have been transformed to factor.",
                    paste(is_not_factor, collapse = ", ")))
      data <- data %>%
        mutate_at(all_of(is_not_factor), as.factor) %>%
        as.data.frame()
    }

  } else {
    make_binary <- FALSE
  }

  # needed for bagging
  data <- data %>% as.data.frame()

  N <- nrow(data)
  n_test <- floor(N * frac_test)

  # initialize empty dataframes to store results
  results <- init_results()

  i <- i_start # keeps only the iterations when all methods succeed
  j <- j_start # keeps all iterations, even in knn or bagging fails

  while (i <= N_iter){
    start_time_total <- Sys.time()

    print(sprintf("i is: %s", i))

    j <- j + 1
    print(sprintf("j is: %s", j))

    # split train / test
    # set seed - at each iteration the train/test split will be the same (regardless of what methods are run)
    # except if datasets are skipped (j)
    set.seed(seed + i + j)
    id_test <- sample(1:N, n_test)

    data_train <- data[-id_test,]
    data_test <- data[id_test,]

    data_train_X <- data_train[,!colnames(data_train) %in% outcome_col]
    data_test_X <- data_test[,!colnames(data_test) %in% outcome_col]

    # save original data for datasets with missing values
    # save(data_test_X, file = sprintf("data_imp/test_%s_orig_%s.RData", dataset_name, i))

    # produce NAs
    if (simulate_MV){

      if (is.null(sim_MV_func)) sim_MV_func <- simulate_miss_MCAR

      # create missingness on train X
      sim_MV_list_train <- sim_MV_func(data_X = data_train_X,
                                       data_y = data_train[, outcome_col], # only for MAR out
                                       p_miss = 0.3) # p_miss only for MCAR
      data_train_X_miss <- sim_MV_list_train$data_X_miss
      id_all_missing_train <- sim_MV_list_train$id_all_missing
      n_all_missing_train <- sum(id_all_missing_train)
      # remove rows with all values missing
      data_train_X_miss <- data_train_X_miss[!id_all_missing_train,]
      data_train_X <- data_train_X[!id_all_missing_train,]
      # keep outcome (and remove eventual rows with all missing)
      y_train <- data_train[!id_all_missing_train, outcome_col]

      # create missingness on test X
      sim_MV_list_test <- sim_MV_func(data_X = data_test_X,
                                      data_y = data_test[, outcome_col], # only for MAR out
                                      p_miss = 0.3) # p_miss only for MCAR
      data_test_X_miss <- sim_MV_list_test$data_X_miss
      id_all_missing_test <- sim_MV_list_test$id_all_missing
      n_all_missing_test <- sum(id_all_missing_test)
      # remove rows with all values missing
      data_test_X_miss <- data_test_X_miss[!id_all_missing_test,]
      data_test_X <- data_test_X[!id_all_missing_test,]
      # keep outcome (and remove eventual rows with all missing)
      y_test <- data_test[!id_all_missing_test, outcome_col]

      results <- results %>%
        add_row(iteration_i = i, iteration_j = j,
                dataset = dataset_name,
                metric = c("n_all_missing_train", "n_all_missing_test",
                           "n_train", "n_test"),
                value = c(n_all_missing_train, n_all_missing_test,
                          nrow(data_train_X_miss), nrow(data_test_X_miss)))


    } else {
      # the dataset already has missing values
      data_train_X_miss <- data_train_X
      data_test_X_miss <- data_test_X

      data_train_X <- NULL # original dataset with no missingness is not known
      data_test_X <- NULL

      # keep outcome
      y_train <- data_train[, outcome_col]
      y_test <- data_test[, outcome_col]

      # keep number all missing (although it shouldn't be the case)
      id_all_missing_train <- rowSums(!is.na(data_train_X_miss)) == 0
      n_all_missing_train <- sum(id_all_missing_train)

      id_all_missing_test <- rowSums(!is.na(data_test_X_miss)) == 0
      n_all_missing_test <- sum(id_all_missing_test)

      results <- results %>%
        add_row(iteration_i = i, iteration_j = j,
                dataset = dataset_name,
                metric = c("n_all_missing_train", "n_all_missing_test",
                           "n_train", "n_test"),
                value = c(n_all_missing_train, n_all_missing_test,
                          nrow(data_train_X_miss), nrow(data_test_X_miss)))

    }

    # keep number of complete cases
    n_complete_cases_train <- sum(complete.cases(data_train_X_miss))
    n_complete_cases_test <- sum(complete.cases(data_test_X_miss))
    results <- results %>%
      add_row(iteration_i = i, iteration_j = j,
              dataset = dataset_name,
              metric = c("n_complete_cases_train", "n_complete_cases_test"),
              value = c(n_complete_cases_train, n_complete_cases_test))

    # make categorical columns binary after amputing the datasets
    if (make_binary){

      data_train_X_miss_bin_model <- make_col_binary_all(data_train_X_miss, categorical_vars)
      data_train_X_miss_bin <- as.data.frame(data_train_X_miss_bin_model$data)

      data_test_X_miss_bin_model <- make_col_binary_all(data_test_X_miss, categorical_vars,
                                                        data_train_X_miss_bin_model$dropped_levels)
      data_test_X_miss_bin <- as.data.frame(data_test_X_miss_bin_model$data)

    }

    # specify vars_to_impute to impute (to save time) - used in miceRanger and tidy imputations
    if (is.null(predictor_matrix)) {
      vars_to_impute <- colnames(data)[colnames(data) != outcome_col]
    } else {
      vars_to_impute <- names(rowSums(predictor_matrix)[rowSums(predictor_matrix) != 0])
    }

    # mean/mode
    # ---------

    if(!skip_mean_mode){

      print("Imputing mean/mode...")

      results_mean_mode <- run_mean_mode(data_train_X_miss, data_test_X_miss,
                                         y_train, y_test,
                                         data_train_X_miss_orig = data_train_X_miss,
                                         data_test_X_miss_orig = data_test_X_miss,
                                         i, j, dataset_name = dataset_name,
                                         data_train_X = data_train_X,
                                         data_test_X = data_test_X,
                                         return_var_imp = FALSE,
                                         positive_class = positive_class,
                                         save_dataset = save_dataset,
                                         run_pred_model = run_pred_model)

      results <- results %>% rbind(results_mean_mode)
    }

    # first try bagging and knn. If they fail, skip the loop.
    # fail fast - don't waste time to fit missForest if knn fails anyhow

    # caret knn
    # ---------

    if (!skip_caret_knn){

      print("Imputing with knn...")

      if (make_binary){
        results_knn <- run_knn(data_train_X_miss_bin, data_test_X_miss_bin, y_train, y_test,
                               data_train_X_miss_orig = data_train_X_miss,
                               data_test_X_miss_orig = data_test_X_miss,
                               transf_type = "bin",
                               i, j,
                               categorical_vars = categorical_vars,
                               data_train_X_miss_model = data_train_X_miss_bin_model,
                               mapping_tables_train_miss = NULL,
                               data_train_X = data_train_X, data_test_X = data_test_X,
                               return_var_imp = FALSE,
                               dataset_name = dataset_name,
                               positive_class = positive_class,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)

        results <- results %>% rbind(results_knn)
      } else {
        results_knn <- run_knn(data_train_X_miss, data_test_X_miss, y_train, y_test,
                               data_train_X_miss_orig = data_train_X_miss,
                               data_test_X_miss_orig = data_test_X_miss,
                               transf_type = "normal",
                               i, j,
                               categorical_vars = NULL,
                               data_train_X_miss_model = NULL,
                               mapping_tables_train_miss = NULL,
                               data_train_X = data_train_X, data_test_X = data_test_X,
                               return_var_imp = FALSE,
                               dataset_name = dataset_name,
                               positive_class = positive_class,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)

        results <- results %>% rbind(results_knn)
      }

      knn_fail <- results %>%
        filter(str_detect(method, "caret_knn"), fail, iteration_i == i, iteration_j == j) %>%
        nrow()

      if (knn_fail) next()

    }

    # caret bagging
    # -------------

    if (!skip_caret_bagging){

      print("Imputing with bagging...")

      if (make_binary){
        results_bagging <- run_bagging(data_train_X_miss_bin, data_test_X_miss_bin, y_train, y_test,
                                       data_train_X_miss_orig = data_train_X_miss,
                                       data_test_X_miss_orig = data_test_X_miss,
                                       transf_type = "bin",
                                       dataset_name = dataset_name,
                                       i, j,
                                       categorical_vars = categorical_vars,
                                       data_train_X_miss_model = data_train_X_miss_bin_model,
                                       mapping_tables_train_miss = NULL,
                                       data_train_X = data_train_X, data_test_X = data_test_X,
                                       return_var_imp = FALSE,
                                       positive_class = positive_class,
                                       save_dataset = save_dataset,
                                       run_pred_model = run_pred_model)

        results <- results %>% rbind(results_bagging)
      } else {
        results_bagging <- run_bagging(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                       data_train_X_miss_orig = data_train_X_miss,
                                       data_test_X_miss_orig = data_test_X_miss,
                                       transf_type = "normal",
                                       dataset_name = dataset_name,
                                       i, j,
                                       categorical_vars = NULL,
                                       data_train_X_miss_model = NULL,
                                       mapping_tables_train_miss = NULL,
                                       data_train_X = data_train_X, data_test_X = data_test_X,
                                       return_var_imp = FALSE,
                                       positive_class = positive_class,
                                       save_dataset = save_dataset,
                                       run_pred_model = run_pred_model)

        results <- results %>% rbind(results_bagging)
      }

      bag_fail <- results %>%
        filter(str_detect(method, "caret_bagging"), fail, iteration_i == i, iteration_j == j) %>%
        nrow()

      if (bag_fail) next()

    }

    # tidy bagging
    # ------------

    if (!skip_tidy_bagging){

      print("Imputing with tidy bagging...")

      # tidy knn supports factors

      # if (make_binary){
      #   bin_colnames <- colnames(data_train_X_miss_bin)
      #   id_keep <- str_split(bin_colnames, "_binary_all", simplify=T)[,1] %in% vars_to_impute
      #   vars_to_impute_bin <- bin_colnames[id_keep]
      #
      #   results_tidy_bagging <- run_tidy_bagging(data_train_X_miss_bin, data_test_X_miss_bin, y_train, y_test,
      #                                            data_train_X_miss_orig = data_train_X_miss,
      #                                            data_test_X_miss_orig = data_test_X_miss,
      #                                            transf_type = "bin",
      #                                            dataset_name = dataset_name,
      #                                            i, j,
      #                                            categorical_vars = categorical_vars,
      #                                            data_train_X_miss_model = data_train_X_miss_bin_model,
      #                                            mapping_tables_train_miss = NULL,
      #                                            data_train_X = data_train_X, data_test_X = data_test_X,
      #                                            return_var_imp = FALSE,
      #                                            positive_class = positive_class,
      #                                            vars_to_impute = vars_to_impute_bin) # use vars_to_impute as in miceRanger
      #
      #   results <- results %>% rbind(results_tidy_bagging)
      # } else {
      results_tidy_bagging <- run_tidy_bagging(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                               data_train_X_miss_orig = data_train_X_miss,
                                               data_test_X_miss_orig = data_test_X_miss,
                                               transf_type = "normal",
                                               dataset_name = dataset_name,
                                               i, j,
                                               categorical_vars = NULL,
                                               data_train_X_miss_model = NULL,
                                               mapping_tables_train_miss = NULL,
                                               data_train_X = data_train_X, data_test_X = data_test_X,
                                               return_var_imp = FALSE,
                                               positive_class = positive_class,
                                               vars_to_impute = vars_to_impute,
                                               save_dataset = save_dataset,
                                               run_pred_model = run_pred_model) # use vars_to_impute as in miceRanger

      results <- results %>% rbind(results_tidy_bagging)
      # }

      bag_fail <- results %>%
        filter(str_detect(method, "bagging_tidy"), fail, iteration_i == i, iteration_j == j) %>%
        nrow()

      if (bag_fail) next()

    }

    # tidy knn
    # --------

    if (!skip_tidy_knn){

      print("Imputing with tidy knn...")

      # tidy knn supports factors

      # if (make_binary){
      #   bin_colnames <- colnames(data_train_X_miss_bin)
      #   id_keep <- str_split(bin_colnames, "_binary_all", simplify=T)[,1] %in% vars_to_impute
      #   vars_to_impute_bin <- bin_colnames[id_keep]
      #
      #   results_tidy_knn <- run_tidy_knn(data_train_X_miss_bin, data_test_X_miss_bin, y_train, y_test,
      #                                    data_train_X_miss_orig = data_train_X_miss,
      #                                    data_test_X_miss_orig = data_test_X_miss,
      #                                    transf_type = "bin",
      #                                    dataset_name = dataset_name,
      #                                    i, j,
      #                                    categorical_vars = categorical_vars,
      #                                    data_train_X_miss_model = data_train_X_miss_bin_model,
      #                                    mapping_tables_train_miss = NULL,
      #                                    data_train_X = data_train_X, data_test_X = data_test_X,
      #                                    return_var_imp = FALSE,
      #                                    positive_class = positive_class,
      #                                    vars_to_impute = vars_to_impute_bin) # use vars_to_impute as in miceRanger
      #
      #   results <- results %>% rbind(results_tidy_knn)
      # } else {
      results_tidy_knn <- run_tidy_knn(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                       data_train_X_miss_orig = data_train_X_miss,
                                       data_test_X_miss_orig = data_test_X_miss,
                                       transf_type = "normal",
                                       dataset_name = dataset_name,
                                       i, j,
                                       categorical_vars = NULL,
                                       data_train_X_miss_model = NULL,
                                       mapping_tables_train_miss = NULL,
                                       data_train_X = data_train_X, data_test_X = data_test_X,
                                       return_var_imp = FALSE,
                                       positive_class = positive_class,
                                       vars_to_impute = vars_to_impute,
                                       save_dataset = save_dataset,
                                       run_pred_model = run_pred_model) # use vars_to_impute as in miceRanger

      results <- results %>% rbind(results_tidy_knn)
      # }

      knn_fail <- results %>%
        filter(str_detect(method, "knn_tidy"), fail, iteration_i == i, iteration_j == j) %>%
        nrow()

      if (knn_fail) next()

    }

    # tidy linear model
    # -----------------

    if (!skip_tidy_linear){

      print("Imputing with tidy linear...")

      if (make_binary){
        bin_colnames <- colnames(data_train_X_miss_bin)
        id_keep <- str_split(bin_colnames, "_binary_all", simplify=T)[,1] %in% vars_to_impute
        vars_to_impute_bin <- bin_colnames[id_keep]

        results_tidy_linear <- run_tidy_linear(data_train_X_miss_bin, data_test_X_miss_bin, y_train, y_test,
                                               data_train_X_miss_orig = data_train_X_miss,
                                               data_test_X_miss_orig = data_test_X_miss,
                                               transf_type = "bin",
                                               dataset_name = dataset_name,
                                               i, j,
                                               categorical_vars = categorical_vars,
                                               data_train_X_miss_model = data_train_X_miss_bin_model,
                                               mapping_tables_train_miss = NULL,
                                               data_train_X = data_train_X, data_test_X = data_test_X,
                                               return_var_imp = FALSE,
                                               positive_class = positive_class,
                                               vars_to_impute = vars_to_impute_bin,
                                               save_dataset = save_dataset,
                                               run_pred_model = run_pred_model) # use vars_to_impute as in miceRanger

        results <- results %>% rbind(results_tidy_linear)
      } else {
        results_tidy_linear <- run_tidy_linear(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                               data_train_X_miss_orig = data_train_X_miss,
                                               data_test_X_miss_orig = data_test_X_miss,
                                               transf_type = "normal",
                                               dataset_name = dataset_name,
                                               i, j,
                                               categorical_vars = NULL,
                                               data_train_X_miss_model = NULL,
                                               mapping_tables_train_miss = NULL,
                                               data_train_X = data_train_X, data_test_X = data_test_X,
                                               return_var_imp = FALSE,
                                               positive_class = positive_class,
                                               vars_to_impute = vars_to_impute,
                                               save_dataset = save_dataset,
                                               run_pred_model = run_pred_model) # use vars_to_impute as in miceRanger

        results <- results %>% rbind(results_tidy_linear)
      }
    }

    # IterativeImputer
    # ----------------

    if (!skip_IterativeImputer){

      print("Imputing with IterativeImputer...")

      if (make_binary){
        results_IterativeImputer <- run_IterativeImputer(data_train_X_miss_bin, data_test_X_miss_bin, y_train, y_test,
                                                         data_train_X_miss_orig = data_train_X_miss,
                                                         data_test_X_miss_orig = data_test_X_miss,
                                                         transf_type = "bin",
                                                         i, j, dataset_name = dataset_name,
                                                         categorical_vars = categorical_vars,
                                                         data_train_X_miss_model = data_train_X_miss_bin_model,
                                                         mapping_tables_train_miss = NULL,
                                                         data_train_X = data_train_X, data_test_X = data_test_X,
                                                         return_var_imp = FALSE,
                                                         positive_class = positive_class,
                                                         save_dataset = save_dataset,
                                                         run_pred_model = run_pred_model)

        results <- results %>% rbind(results_IterativeImputer)
      } else {
        results_IterativeImputer <- run_IterativeImputer(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                                         data_train_X_miss_orig = data_train_X_miss,
                                                         data_test_X_miss_orig = data_test_X_miss,
                                                         transf_type = "normal",
                                                         i, j, dataset_name = dataset_name,
                                                         categorical_vars = NULL,
                                                         data_train_X_miss_model = NULL,
                                                         mapping_tables_train_miss = NULL,
                                                         data_train_X = data_train_X, data_test_X = data_test_X,
                                                         return_var_imp = FALSE,
                                                         positive_class = positive_class,
                                                         save_dataset = save_dataset,
                                                         run_pred_model = run_pred_model)

        results <- results %>% rbind(results_IterativeImputer)
      }
    }

    # missForest
    # ----------

    if (!skip_missForest){

      print("Imputing with missForest...")

      # always run missForest on untransformed data
      results_missForest <- run_missForest(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                           data_train_X_miss_orig = data_train_X_miss,
                                           data_test_X_miss_orig = data_test_X_miss,
                                           transf_type = "normal",
                                           i, j, dataset_name = dataset_name,
                                           categorical_vars = NULL,
                                           data_train_X_miss_model = NULL,
                                           mapping_tables_train_miss = NULL,
                                           data_train_X = data_train_X, data_test_X = data_test_X,
                                           return_var_imp = TRUE,
                                           predictor_matrix = predictor_matrix,
                                           positive_class = positive_class,
                                           save_dataset = save_dataset,
                                           run_pred_model = run_pred_model)

      results <- results %>% rbind(results_missForest)
    }

    if (!skip_missForest_md10){

      print("Imputing with missForest (max depth 10)...")

      # always run missForest on untransformed data
      results_missForest_md10 <- run_missForest_md10(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                                     data_train_X_miss_orig = data_train_X_miss,
                                                     data_test_X_miss_orig = data_test_X_miss,
                                                     transf_type = "normal",
                                                     i, j, dataset_name = dataset_name,
                                                     categorical_vars = NULL,
                                                     data_train_X_miss_model = NULL,
                                                     mapping_tables_train_miss = NULL,
                                                     data_train_X = data_train_X, data_test_X = data_test_X,
                                                     return_var_imp = TRUE,
                                                     predictor_matrix = predictor_matrix,
                                                     positive_class = positive_class,
                                                     save_dataset = save_dataset,
                                                     run_pred_model = run_pred_model)

      results <- results %>% rbind(results_missForest_md10)
    }

    # miceRanger
    # ----------

    if(!skip_miceRanger){

      print("Imputing with miceRanger...")

      # miceRanger - m = 5
      results_miceRanger <- run_miceRanger(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                           data_train_X_miss_orig = data_train_X_miss,
                                           data_test_X_miss_orig = data_test_X_miss,
                                           i, j, dataset_name = dataset_name,
                                           data_train_X = data_train_X, data_test_X = data_test_X,
                                           return_var_imp = FALSE,
                                           m = 5, vars_to_impute = vars_to_impute,
                                           positive_class = positive_class,
                                           save_dataset = save_dataset,
                                           run_pred_model = run_pred_model)

      results <- results %>% rbind(results_miceRanger)
    }

    # mice RF
    # -------

    if(!skip_mice_RF){

      print("Imputing with mice RF...")

      # mice - m = 5
      results_mice_rf <- run_mice_rf(data_train_X_miss, data_test_X_miss, y_train, y_test,
                                     data_train_X_miss_orig = data_train_X_miss,
                                     data_test_X_miss_orig = data_test_X_miss,
                                     i, j, dataset_name = dataset_name,
                                     data_train_X = data_train_X, data_test_X = data_test_X,
                                     return_var_imp = FALSE,
                                     m = 5, vars_to_impute = vars_to_impute,
                                     positive_class = positive_class,
                                     save_dataset = save_dataset,
                                     run_pred_model = run_pred_model)

      results <- results %>% rbind(results_mice_rf)
    }

    # mice default
    # ------------

    if(!skip_mice_default){

      print("Imputing with mice default...")

      # mice - m = 5
      results_mice <- run_mice(data_train_X_miss, data_test_X_miss, y_train, y_test,
                               data_train_X_miss_orig = data_train_X_miss,
                               data_test_X_miss_orig = data_test_X_miss,
                               i, j, dataset_name = dataset_name,
                               data_train_X = data_train_X, data_test_X = data_test_X,
                               return_var_imp = FALSE,
                               m = 5, vars_to_impute = vars_to_impute,
                               positive_class = positive_class,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)

      results <- results %>% rbind(results_mice)
    }

    # original
    # --------

    if(!skip_original){

      # run this only if original data is complete
      if (!is.null(data_train_X)) {
        print("Models on original dataset...")

        results_original <- run_original(data_train_X, data_test_X, y_train, y_test,
                                         i, j, dataset_name = dataset_name,
                                         return_var_imp = TRUE,
                                         positive_class = positive_class,
                                         save_dataset = save_dataset,
                                         run_pred_model = run_pred_model)

        results <- results %>% rbind(results_original)
      }
    }

    time_total <- as.numeric(difftime(Sys.time(), start_time_total, units = "mins"))
    print(sprintf("Total time iteration: %s minutes", round(time_total, 2)))

    i <- i + 1
  }

  return(results)

}
