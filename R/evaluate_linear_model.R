#' Build linear regression model and evaluate test performance.
#'
#' Build restricted cubic splines linear regression model and evaluate test performance.
#'
#' @param data_train_imp train set X (imputed)
#' @param data_test_imp test set X (imputed)
#' @param y_train train set - outcome
#' @param y_test test set - outcome
#' @param i iteration i
#' @param j iteration j
#' @param dataset_name dataset name
#' @param method imputation method name
#' @param knots number of knots for the restricted cubic spline
#' @param positive_class in case of binary outcome (classification) the name of the positive class
#' @param data_train_X_miss original training data with missing values (used for deciding if a variable
#' is subject to cubic splines)
#' @param dropped_levels_train dropped levels used for binarizing the trainig data
#' @param use_package which package to use for fitting models
#'
#' @return results

evaluate_linear_model <- function(data_train_imp, data_test_imp, y_train, y_test,
                                  dataset_name, i, j, method,
                                  knots = 3,
                                  positive_class = NULL,
                                  data_train_X_miss = NULL,
                                  dropped_levels_train = NULL,
                                  use_package = "rcs"){

  results <- init_results()

  message("Building linear model with restricted cubic splines...")

  model_type <- "cubic_spline"

  # binarize the categorical variables if necessary
  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x) | is.character(x), "factor", NA_character_))

  var_type <- unlist(lapply(data_train_imp, column_class))

  if (any(var_type == "factor")){

    if (is.null(dropped_levels_train)){

      data_train_bin_model <- make_col_binary_all(data_train_imp, names(var_type[var_type == "factor"]))
      data_train_imp <- as.data.frame(data_train_bin_model$data)

      data_test_bin_model <- make_col_binary_all(data_test_imp, names(var_type[var_type == "factor"]),
                                                 data_train_bin_model$dropped_levels)
      data_test_imp <- as.data.frame(data_test_bin_model$data)

    } else {
      data_train_bin_model <- make_col_binary_all(data_train_imp, names(var_type[var_type == "factor"]),
                                                  dropped_levels_train)
      data_train_imp <- as.data.frame(data_train_bin_model$data)

      data_test_bin_model <- make_col_binary_all(data_test_imp, names(var_type[var_type == "factor"]),
                                                 dropped_levels_train)
      data_test_imp <- as.data.frame(data_test_bin_model$data)
    }
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

  # rescale train set
  data_train_imp <- scale(data_train_imp)
  # rescale test set using center / scale from train
  train_mean <- attr(data_train_imp, "scaled:center")
  train_sd <- attr(data_train_imp, "scaled:scale")

  ## respect order of columns on test before rescaling
  train_mean <- train_mean[colnames(data_test_imp)]
  train_sd <- train_sd[colnames(data_test_imp)]

  data_test_imp <- scale(data_test_imp, center = train_mean, scale = train_sd)

  data_train_imp <- data_train_imp %>% as.data.frame()
  data_test_imp <- data_test_imp %>% as.data.frame()

  if (use_package == "ns"){

    # separate continuous and categorical variables using the original dataframe.
    # Splines on scores for example (like GCS)
    # that are imputed with values with few values with decimals are problematic
    # for splines (error) even if they have a large number of unique values

    # choose continuous variables to add knots
    # check frequency table for continuous variables as some are too few to add three knots
    var_type_x_orig <- unlist(lapply(data_train_X_miss, column_class))

    if (any(var_type_x_orig == "factor")){
      # use previous model of binarizing
      if (is.null(dropped_levels_train)){
        data_orig_bin_model <- make_col_binary_all(data_train_X_miss, names(var_type[var_type == "factor"]),
                                                   data_train_bin_model$dropped_levels)
        data_train_X_miss <- as.data.frame(data_orig_bin_model$data)
      } else {
        data_orig_bin_model <- make_col_binary_all(data_train_X_miss, names(var_type[var_type == "factor"]),
                                                   dropped_levels_train)
        data_train_X_miss <- as.data.frame(data_orig_bin_model$data)
      }
    }

    continuous_categorical_class <- function(x) ifelse(length(table(x)) > 15, "numeric",
                                                       ifelse(length(table(x)) <= 15, "factor", NA_character_))

    continuous_categorical_type <- unlist(lapply(data_train_X_miss, continuous_categorical_class))
    continuous_vars <- colnames(data_train_X_miss)[continuous_categorical_type == "numeric"]
    categorical_vars <- colnames(data_train_X_miss)[!colnames(data_train_X_miss) %in% continuous_vars]

    # create dataframes containing the outcome
    outcome_train <- as.data.frame(y_train)
    colnames(outcome_train) <- outcome_col
    data_train_with_outcome <- cbind(data_train_imp, outcome_train)

    outcome_test <- as.data.frame(y_test)
    colnames(outcome_test) <- outcome_col
    data_test_with_outcome <- cbind(data_test_imp, outcome_test)

    # build cubic spline model form
    # knots = 3
    # add knots for continuous variables only
    if (length(continuous_vars) == 0) { # no continuous variable
      form.binary <- paste0(categorical_vars, collapse = " + ")
      form <- as.formula(                      # Create formula
        paste(outcome_col, "~", form.binary))
    } else if (length(categorical_vars) == 0) { # no categorical
      form.continuous <- paste0("ns(",continuous_vars, paste0(",", knots, ")"), collapse = " + ")
      form <- as.formula(                      # Create formula
        paste(outcome_col, "~", form.continuous))
    } else { # contain both categorical and continuous
      form.continuous <- paste0("ns(",continuous_vars, paste0(",", knots, ")"), collapse = " + ")
      form.binary <- paste0(categorical_vars, collapse = " + ")

      form <- as.formula(                      # Create formula
        paste(outcome_col, "~", paste0(form.continuous, " + ", form.binary)))
    }

  } else { # rcs
    # separate continuous and categorical variables
    continuous_categorical_class <- function(x) ifelse(length(table(x)) > 2, "numeric",
                                                       ifelse(length(table(x)) <= 2, "factor", NA_character_))

    continuous_categorical_type <- unlist(lapply(data_train_imp, continuous_categorical_class))
    continuous_vars <- colnames(data_train_imp)[continuous_categorical_type == "numeric"]
    categorical_vars <- colnames(data_train_imp)[!colnames(data_train_imp) %in% continuous_vars]

    # create dataframes containing the outcome
    outcome_train <- as.data.frame(y_train)
    colnames(outcome_train) <- outcome_col
    data_train_with_outcome <- cbind(data_train_imp, outcome_train)

    outcome_test <- as.data.frame(y_test)
    colnames(outcome_test) <- outcome_col
    data_test_with_outcome <- cbind(data_test_imp, outcome_test)

    if (length(continuous_vars) == 0) { # no continuous variables
    variables_to_add_knots <- NULL
    } else {
      knots_location <- as.data.frame(lapply(data_train_imp[, continuous_vars], function(x) formatC(quantile(unlist(x), c(.1, .5, .9)))))
      knots_location <- as.data.frame(t(knots_location))
      knots_location <- rownames_to_column(knots_location, var = "feature")
      knots_location <- knots_location %>%
        filter(!(`10%`== `50%`| `50%` == `90%`| `10%` == `90%`)) %>%
        mutate(form.knots = paste0("rcs(", feature, ",", paste0("c(", paste(`10%`, `50%`, `90%`, sep = ","), "))")))

    variables_to_add_knots <- unique(knots_location$feature)
    }

    # separate variables with and without knots

    variables_not_to_add_knots <- colnames(data_train_imp)[!colnames(data_train_imp) %in% variables_to_add_knots]


    # build cubic spline model form
    # knots = 3
    # add knots for continuous variables only
    # choose continuous variables to add knots
    # check frequency table for continuous variables as some are too few to add three knots

    if (length(variables_to_add_knots) == 0) { # no continuous variable
      form.binary <- paste0(variables_not_to_add_knots, collapse = " + ")
      form <- as.formula(                      # Create formula
        paste(outcome_col, "~", form.binary))
    } else if (length(variables_not_to_add_knots) == 0) { # no categorical
      form.continuous <- paste(knots_location$form.knots, collapse = " + ")
      form <- as.formula(                      # Create formula
        paste(outcome_col, "~", form.continuous))
    } else { # contain both categorical and continuous
      form.continuous <- paste(knots_location$form.knots, collapse = " + ")

      form.binary <- paste0(variables_not_to_add_knots, collapse = " + ")

      form <- as.formula(                      # Create formula
        paste(outcome_col, "~", paste0(form.continuous, " + ", form.binary)))
    }

  }

  # build model
  if (is.numeric(y_train)){ # continuous outcome

    # fit prediction model on train - with imputed values
    LR_extend_model <- lm(form, data_train_with_outcome)

    # predict on test set
    test_preds <- predict(LR_extend_model, data_test_imp)
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
    LR_extend_model <- glm(form, data_train_with_outcome, family = "binomial")

    # predict on test set
    test_preds <- predict(LR_extend_model, newdata = data_test_imp, type = "response")

    # make sure positive class is positive class in glm
    # As a factor: ‘success’ is interpreted as the factor not having the first level
    # (and hence usually of having the second level) (binomial help)
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

  # coefficients of the best model
  coefs <- coef(LR_extend_model)

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
