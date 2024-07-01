#' Build linear regression model and evaluate test performance.
#'
#' Build regularized linear regression model and evaluate test performance.
#' Always cross-validate and always return feature importance for linear models
#' (not expensive in computation time)
#'
#' @param data_train_imp train set X (imputed)
#' @param data_test_imp test set X (imputed)
#' @param y_train train set - outcome
#' @param y_test test set - outcome
#' @param i iteration i
#' @param j iteration j
#' @param dataset_name dataset name
#' @param method imputation method name
#' @param alpha alpha parameter as passed to glmnet
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#'
#' @return results

evaluate_LR_model <- function(data_train_imp, data_test_imp, y_train, y_test,
                              dataset_name, i, j, method,
                              alpha = 1,
                              positive_class = NULL){

  results <- init_results()

  message(sprintf("Building regularized linear model with alpha = %s...", alpha))

  if (alpha == 1) {
    model_type <- "LASSO"
  } else if (alpha == 0) {
    model_type <- "Ridge"
  } else {
    model_type <- paste0("LR - alpha = ", alpha)
  }

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

  data_train_imp <- as.matrix(data_train_imp)
  data_test_imp <- as.matrix(data_test_imp)

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

  # rescale train set
  data_train_imp <- scale(data_train_imp)
  # rescale test set using center / scale from train
  train_mean <- attr(data_train_imp, "scaled:center")
  train_sd <- attr(data_train_imp, "scaled:scale")

  # respect order of columns on test before rescaling
  train_mean <- train_mean[colnames(data_test_imp)]
  train_sd <- train_sd[colnames(data_test_imp)]

  data_test_imp <- scale(data_test_imp, center = train_mean, scale = train_sd)

  cl <- makeCluster(n_cpu)
  registerDoParallel(cl)

  if (is.numeric(y_train)){ # continuous outcome

    # fit prediction model on train - with imputed values
    LR_model <- cv.glmnet(data_train_imp, y_train,
                          family = "gaussian",
                          standardize = TRUE,
                          nfolds = n_fold,
                          alpha = alpha,
                          type.measure = "mse",
                          parallel = TRUE)

    # predict on test set
    test_preds <- predict(LR_model, newx = data_test_imp, s = "lambda.min")[,1]
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
              model = model_type)

  } else { #y_train is binary

    y_train <- as.factor(y_train)
    y_test <- as.factor(y_test)
    y_test <- ifelse(y_test == positive_class, 1, 0)
    y_test_matrix <- matrix(cbind(y_test, 1 - y_test), ncol = 2)
    colnames(y_test_matrix) <- c(positive_class, levels(y_train)[levels(y_train) != positive_class])

    # fit prediction model on train - with imputed values
    LR_model <- cv.glmnet(data_train_imp, y_train,
                          family = "binomial",
                          standardize = TRUE,
                          nfolds = n_fold,
                          alpha = alpha,
                          type.measure = "deviance",
                          parallel = TRUE)

    # predict on test set
    test_preds <- predict(LR_model, newx = data_test_imp, s = "lambda.min", type = "response")[,1]

    # make sure positive class is positive class in glmnet
    # for a factor, the last level in alphabetical order is the target class (glmnet help)
    class_levels <- sort(levels(y_train))
    neg_class_glmnet <- class_levels[[1]]
    pos_class_glmnet <- class_levels[[2]]

    if (positive_class != pos_class_glmnet){
      test_preds <- 1 - test_preds
    }

    # make preds matrix
    test_preds_matrix <- matrix(cbind(test_preds, 1 - test_preds), ncol = 2)
    colnames(test_preds_matrix) <- c(positive_class, class_levels[class_levels != positive_class])

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
              model = model_type)
  }

  stopCluster(cl)
  registerDoSEQ()

  # coefficients of the best model
  coefs <- coef(LR_model, s = "lambda.min")[,1]

  if(!is.numeric(y_train)){
    if (positive_class != pos_class_glmnet) coefs = -coefs
  }

  # keep variable importance
  results <- results %>%
    add_row(iteration_i = i,
            iteration_j = j,
            dataset = dataset_name,
            metric = "Variable importance",
            value = coefs,
            variable = names(coefs),
            method = method, model = model_type)


  return(results)
}
