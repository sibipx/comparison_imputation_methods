#' Build XGB model and evaluate test performance.
#'
#' Build XGB model and evaluate test performance.
#'
#' @param data_train_imp train set X (imputed)
#' @param data_test_imp test set X (imputed)
#' @param y_train train set - outcome
#' @param y_test test set - outcome
#' @param return_var_imp return variable importance for the prediction model?
#' @param i iteration i
#' @param j iteration j
#' @param dataset_name dataset name
#' @param method imputation method name
#' @param cv crossvalidate (TRUE) or fit with default parameters of ranger (FALSE)
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

evaluate_XGB_model <- function(data_train_imp, data_test_imp, y_train, y_test,
                              return_var_imp, dataset_name, i, j, method,
                              cv = TRUE,
                              positive_class = NULL){

  results <- init_results()

  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x) | is.character(x), "factor", NA_character_))

  var_type <- unlist(lapply(data_train_imp, column_class))

  if (any(var_type == "factor")){

    data_train_bin_model <- make_col_binary_all(data_train_imp, names(var_type[var_type == "factor"]))
    data_train_imp <- as.data.frame(data_train_bin_model$data)

    data_test_bin_model <- make_col_binary_all(data_test_imp, names(var_type[var_type == "factor"]),
                                               data_train_bin_model$dropped_levels)
    data_test_imp <- as.data.frame(data_test_bin_model$data)
  }

  # remove 0 variance variables (in the unlikely case it happens)
  cols_keep <- apply(data_train_imp, 2, var) != 0
  data_train_imp <- data_train_imp[,cols_keep, drop = FALSE]
  cols_keep <- cols_keep[names(cols_keep) %in% colnames(data_test_imp)] # if binarizing columns might not be the same
  data_test_imp <- data_test_imp[,cols_keep, drop = FALSE]

  # if test is missing any binary col, add it as 0
  cols_train_only <- colnames(data_train_imp)[!colnames(data_train_imp) %in% colnames(data_test_imp)]
  cols_train_only <- cols_train_only[str_detect(cols_train_only, "binary_all")]

  if (length(cols_train_only) > 0){
    for (col in cols_train_only) {

      names_old <- colnames(data_test_imp)
      data_test_imp <- cbind(data_test_imp, 0)
      names_new <- c(names_old, col)
      colnames(data_test_imp) <- names_new
    }
  }

  message("Building XBG model...")

  # build XBG model
  #XGB_model <- fit_XGB_CV(data_train_imp, y_train, return_var_imp = return_var_imp)
  XGB_model <- fit_XGB_CV_caret(data_train_imp, y_train, return_var_imp = return_var_imp) # caret

  # bring columns of test in the same order as stored in the model
  #data_test_imp <- data_test_imp[,XGB_model$feature_names]
  data_test_imp <- data_test_imp[,XGB_model$finalModel$feature_names] # caret

  # keep variable importance
  if (return_var_imp){
    #var_imp <- xgb.importance(model = XGB_model)
    var_imp <- xgb.importance(model = XGB_model$finalModel) # caret

    results <- results %>%
      add_row(iteration_i = i,
              iteration_j = j,
              dataset = dataset_name,
              metric = "Variable importance",
              value = var_imp$Gain,
              variable = var_imp$Feature,
              method = method, model = "XGB")
  }

  # evaluate XGB model
  if (is.numeric(y_train)){ # continuous outcome

    # predict on test set
    test_preds <- predict(XGB_model, as.matrix(data_test_imp))
    metrics <- caret::postResample(test_preds, y_test)
    metrics["MAPE"] <- MAPE(test_preds, y_test)
    metrics["SMAPE"] <- SMAPE(test_preds, y_test)

    results <- results %>%
      add_row(iteration_i = i, iteration_j = j,
              dataset = dataset_name,
              metric = names(metrics),
              value = metrics,
              method = method,
              type = "test",
              model = "XGB")

  } else { #y_train is binary

    y_train <- as.factor(y_train)
    y_test <- as.factor(y_test)
    y_test <- ifelse(y_test == positive_class, 1, 0)
    y_test_matrix <- matrix(cbind(y_test, 1 - y_test), ncol = 2)
    colnames(y_test_matrix) <- c(positive_class, levels(y_train)[levels(y_train) != positive_class])

    #test_preds <- predict(XGB_model, as.matrix(data_test_imp), type="prob")
    test_preds <- predict(XGB_model, as.matrix(data_test_imp), type="prob")[,positive_class] # caret


    test_preds_matrix <- as.matrix(cbind(test_preds, 1 - test_preds))
    negative_class <- levels(y_train)[levels(y_train) != positive_class]
    colnames(test_preds_matrix) <- c(positive_class, negative_class)

    metrics <- c("AUROC" = ModelMetrics::auc(y_test, test_preds))
    metrics["AUPRC"] <- AUPRC(test_preds, y_test, 1)
    metrics["slope"] <- cox_first_degree(y_test, test_preds)$slope
    metrics["intercept"] <- cox_first_degree(y_test, test_preds)$intercept
    metrics["mean_calibration"] <- mean(test_preds) / mean(y_test) # estimated over observed
    metrics["BS"] <- BS(test_preds_matrix, y_test_matrix[,colnames(test_preds_matrix)])
    metrics["BSS"] <- 1 - BSnorm(test_preds_matrix, y_test_matrix[,colnames(test_preds_matrix)])

    results <- results %>%
      add_row(iteration_i = i, iteration_j = j,
              dataset = dataset_name,
              metric = names(metrics),
              value = metrics,
              method = method,
              type = "test",
              model = "XGB")
  }

  return(results)
}
