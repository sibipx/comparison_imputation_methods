#' Build RF model and evaluate test performance.
#'
#' Build RF model and evaluate test performance.
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
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

evaluate_RF_model <- function(data_train_imp, data_test_imp, y_train, y_test,
                              return_var_imp, dataset_name, i, j, method,
                              positive_class = NULL){

  results <- init_results()

  message("Building RF model...")

  # build RF model
  RF_model <- fit_RF_tune(data_train_imp, y_train, return_var_imp = return_var_imp)

  # keep variable importance
  if (return_var_imp){
    results <- results %>%
      add_row(iteration_i = i,
              iteration_j = j,
              dataset = dataset_name,
              metric = "Variable importance",
              value = RF_model$variable.importance,
              variable = names(RF_model$variable.importance),
              method = method, model = "RF")
  }

  # evaluate RF model
  if (is.numeric(y_train)){ # continuous outcome

    # predict on test set
    test_preds <- predict(RF_model, data_test_imp)$predictions
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
              model = "RF")

  } else { #y_train is binary

    y_train <- as.factor(y_train)
    y_test <- as.factor(y_test)
    y_test <- ifelse(y_test == positive_class, 1, 0)
    y_test_matrix <- matrix(cbind(y_test, 1 - y_test), ncol = 2)
    colnames(y_test_matrix) <- c(positive_class, levels(y_train)[levels(y_train) != positive_class])

    # predict on test set
    test_preds <- predict(RF_model, data_test_imp)$predictions

    test_preds_matrix <- test_preds[,colnames(y_test_matrix)]

    metrics <- c("AUROC" = ModelMetrics::auc(y_test, test_preds[,positive_class]))
    metrics["AUPRC"] <- AUPRC(test_preds[,positive_class], y_test, 1)
    metrics["slope"] <- cox_first_degree(y_test, test_preds[,positive_class])$slope
    metrics["intercept"] <- cox_first_degree(y_test, test_preds[,positive_class])$intercept
    metrics["mean_calibration"] <- mean(test_preds[,positive_class]) / mean(y_test) # estimated over observed
    metrics["BS"] <- BS(test_preds_matrix, y_test_matrix[,colnames(test_preds_matrix)])
    metrics["BSS"] <- 1 - BSnorm(test_preds_matrix, y_test_matrix[,colnames(test_preds_matrix)])

    results <- results %>%
      add_row(iteration_i = i, iteration_j = j,
              dataset = dataset_name,
              metric = names(metrics),
              value = metrics,
              method = method,
              type = "test",
              model = "RF")
  }

  return(results)
}
