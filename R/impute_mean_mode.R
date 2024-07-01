#' train set mean / mode imputation
#'
#' @param data data to impute

impute_mean_mode_train <- function(data) {

  imputation_values <- list()

  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x) | is.character(x), "factor", NA_character_))

  var_type <- unlist(lapply(data, column_class))

  # impute categorical
  for (col in names(var_type[var_type == "factor"])) {

    col_values <- data[,col, drop = TRUE]
    col_values <- col_values[!is.na(col_values)]

    mode_col <- unique(col_values)[col_values %>%
                                     match(unique(col_values)) %>%
                                     tabulate() %>%
                                     which.max()]
    mode_col <- as.character(mode_col)

    # keep mode value for test
    imputation_values[col] <- mode_col

    # impute train
    data[is.na(data[,col]),col] <- mode_col
  }

  # impute continuous
  for (col in names(var_type[var_type == "numeric"])) {

    col_values <- data[,col, drop = TRUE]
    mean_col <- mean(col_values, na.rm = TRUE)

    # keep mean value for test
    imputation_values[col] <- mean_col

    # impute train
    data[is.na(data[,col]),col] <- mean_col
  }

  if (sum(is.na(data)) != 0)
    stop("Something went wrong. The dataset is NOT completely imputed")

  return(list(data = data, imputation_values = imputation_values))

}

#' test set mean / mode imputation
#'
#' @param data data to impute
#' @param imputation_values imputation values learned to the training set

impute_mean_mode_test <- function(data, imputation_values) {

  for (col in colnames(data)) {

    data[is.na(data[,col]),col] <- imputation_values[[col]]

  }

  if (sum(is.na(data)) != 0)
    stop("Something went wrong. The dataset is NOT completely imputed")

  return(data)

}
