# makes friendly percent
make_percent <- function(x) paste0(round(x, 4) * 100, "%")

# silent min and max
min_quiet <- function(x) suppressWarnings(min(x))
max_quiet <- function(x) suppressWarnings(max(x))

# metrics
MAPE <- function (y_pred, y_true) mean(abs((y_true - y_pred)/y_true))
SMAPE <- function(y_pred, y_true) mean(abs(y_pred - y_true)/((abs(y_pred) + abs(y_true))/2))

AUPRC <- function(preds, y_true, positive_class){
  curves <- precrec::evalmod(scores = preds,
                             labels = y_true,
                             posclass = positive_class)
  aucs <- precrec::auc(curves)
  AUPRC <- aucs[aucs$curvetypes == "PRC", "aucs"]

  return(AUPRC)
}

#' Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Brier Score
#' @keywords internal
#' @noRd

BS <- function (probabilities, y) {
  mean(rowSums((probabilities - y)^2))
}

#' Normalized Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Normalized Brier Score
#' @keywords internal
#' @noRd

BSnorm <- function (probabilities, y) {
  # reference is a "no skill learner" that predicts the class prevalence
  BS_reference <- BS(matrix(rep(colMeans(y), nrow(y)), nrow = nrow(y), byrow = TRUE), y)
  if (BS_reference == 0){ # avoid division by 0
    return(0)
  } else {
    return(BS(probabilities, y) / BS_reference)
  }
}

make_col_binary_all <- function(data, cols, dropped_levels = NULL) {

  clean_strings <- function(x){
    x <- trimws(x, which = c("both"))
    x <- gsub("%", "perc", x)
    x <- gsub("[[:punct:]]|[[:space:]]", "_", x)
    x <- gsub('^\\_*|\\_*$', '', x) # remove leading / trailing _ (if left)

    return(x)
  }

  data <- data %>%
    mutate_at(all_of(cols), clean_strings)

  if (is.null(dropped_levels)) dropped_levels <- list()

  for (col in cols){

    col_orig <- data[,col]

    if (is.null(dropped_levels[[col]])){
      col_levels <- unique(col_orig)
      col_levels <- col_levels[!is.na(col_levels)]
      # sort levels alphabetically to always remove the first one (in "0"/"1", "0" is removed)
      col_levels <- sort(col_levels)
      dropped_level <- col_levels[[1]]

      dropped_levels[[col]] <- dropped_level
    } else {
      dropped_level <- dropped_levels[[col]]
    }

    # create binary variables
    data <- data %>%
      mutate(id = row_number()) %>%
      pivot_wider(names_from = all_of(col), names_prefix = paste0(col, "_binary_all_"),
                  values_from = all_of(col), values_fill = 0, values_fn = function(x) 1) %>%
      select(-id)

    # drop 1 level
    col_name_drop <- paste(col, "binary_all", dropped_level, sep = "_")
    data[col_name_drop] <- NULL

    # make NA when the initial value was NULL (missing)
    NA_column <- paste0(col, "_binary_all_", "NA")

    if (NA_column %in% colnames(data)) {
      data <- data %>%
        mutate_at(vars(starts_with(paste0(col, "_binary_all_"))), ~ if_else(!!sym(NA_column) == 1, NA_real_, .)) %>%
        select(-all_of(NA_column))
    }

  }

  return(list(data = data, dropped_levels = dropped_levels))

}

make_col_categ <- function(data, cols, dropped_levels){

  data <- as.data.frame(data)
  colnames_data <- colnames(data)

  for (col in cols){

    # detect all binary columns
    cols_binary <- colnames_data[str_starts(colnames_data, paste0(col, "_binary_all"))]

    # recover dropped level
    dropped_level <- dropped_levels[[col]]
    col_name_dropped <- paste(col, "binary_all", dropped_level, sep = "_")

    data[col_name_dropped] <- 1 - rowSums(data[,cols_binary, drop = FALSE])
    cols_binary <- c(cols_binary, col_name_dropped)
    levels_categ <- str_remove(cols_binary, paste0(col, "_binary_all_"))

    data[,col] <- apply(data[,cols_binary], 1, function(x) levels_categ[which.max(x)])
    data[, col] <- factor(data[, col])
    data[,cols_binary] <- NULL

  }

  return(data)
}

# needed for knn
unscale <- function(X, scale = NULL, center = NULL){
  if (is.null(scale)) scale = attr(X,'scaled:scale')
  if (is.null(center)) scale = attr(X,'scaled:center')

  t(apply(X, 1, function(r)r*scale + center))
}

make_col_cont_all <- function(data, cols, outcome, mapping_tables = NULL) {

  #message(sprintf("Making cont columns from all values for %s", paste(cols, collapse = ", ")))

  if (is.null(mapping_tables)) mapping_tables <- list()

  for (col in cols){

    col_orig <- data[,col]

    levels <- as.character(unique(col_orig))
    levels <- levels[!is.na(levels)]

    if(is.numeric(outcome)) {

      # if mapping_tables is provided use it for transfomration
      # if not, calculate it (training data)
      if (is.null(mapping_tables[[col]])){
        mapping_table <- tibble(col_orig = col_orig, outcome = outcome) %>%
          filter(!is.na(col_orig)) %>%
          group_by(col_orig) %>%
          summarise(mean_outcome = mean(outcome)) %>%
          arrange(mean_outcome) %>%
          mutate(id = row_number() - 1) %>%
          select(col_orig, id)

        mapping_tables[[col]] <- mapping_table
      } else {
        mapping_table <- mapping_tables[[col]]
      }

      data[, col] <- mapping_table$id[match(data[, col], mapping_table$col_orig)]

    } else { # categorical vars

      # if mapping_tables is provided use it for transfomration
      # if not, calculate it (training data)
      if (is.null(mapping_tables[[col]])){

        # postivie class = lowest proprtion class
        table_outcome <- outcome %>% table()
        pos_class <- names(table_outcome)[table_outcome == min(table_outcome)]

        mapping_table <- tibble(col_orig = col_orig, outcome = outcome) %>%
          mutate_all(as.factor) %>%
          filter(!is.na(col_orig)) %>%
          group_by(col_orig, outcome, .drop = FALSE) %>% # drop = FALSE to avoid 0 in pos_class (needs factor cols!)
          count() %>%
          ungroup() %>%
          group_by(col_orig) %>%
          mutate(p = n/sum(n)) %>%
          ungroup() %>%
          filter(outcome == pos_class) %>%
          arrange(p) %>%
          mutate(id = row_number() - 1) %>%
          select(col_orig, id)

        mapping_tables[[col]] <- mapping_table

      } else {
        mapping_table <- mapping_tables[[col]]
      }

      data[, col] <- mapping_table$id[match(data[, col], mapping_table$col_orig)]

    }

  }

  return(list(data = data, mapping_tables = mapping_tables))

}

make_col_categ_from_cont <- function(data, cols, mapping_tables){

  data <- as.data.frame(data)
  colnames_data <- colnames(data)

  #message(sprintf("Making categorical columns from continuous columns for: %s", paste(cols, collapse = ", ")))

  for (col in cols){

    mapping_table <- mapping_tables[[col]]

    data[, col] <- mapping_table$col_orig[match(round(data[, col]), mapping_table$id)]
    data[, col] <- factor(data[, col])

  }

  return(data)
}

# creates dataframe with columns Variable and Variable type
# Variable type is continuous or categorical with k categories
data_type <- function(data){
  data_types <- sapply(data, class)

  types_df <- tibble(Variable = colnames(data),
                     `Variable type` = NA_character_)

  for (col in colnames(data)){
    if (any(c("numeric", "integer") %in% data_types[[col]])) {
      type <- "continuous"
    } else if (any(c("factor", "character") %in% data_types[[col]]))  {
      values <- data[[col]]
      n_cat <- length(unique(values[!is.na(values)]))
      type <- sprintf("categorical, %s categories", n_cat)
    } else {
      type <- "UNINDETIFIED"
    }

    types_df[types_df$Variable == col, 2] <- type
  }

  return(types_df)
}

# collapses m imputed dataframes in one dataframe,
# taking mean or mode for each variable and observation across dataframes
aggregate_mice_results <- function(x){

  calculate_mode <- function(v){
    max_level <- max(table(v, useNA = "no"))
    summary_col <- summary(as.factor(v))
    # if there are several classes with equal number of samples, sample one at random
    mode_val <- sample(names(which(max_level == summary_col)), 1)

    return(mode_val)
  }

  result_df <- data.frame(matrix(ncol = ncol(x[[1]]), nrow = nrow(x[[1]])))
  colnames(result_df) <- colnames(x[[1]])

  for (c in colnames(x[[1]])){
    if (is.factor(x[[1]][[c]]) | is.character(x[[1]][[c]])){ # categorical vars

      col_over_dfs <- lapply(x, function(x) as.character(x[[c]]))
      col_over_dfs <- data.frame(col_over_dfs)
      mode_col <- apply(col_over_dfs, 1, calculate_mode)

      if (is.factor(x[[1]][[c]])){
        mode_col <- factor(mode_col, levels = levels(x[[1]][[c]]))
      }

      result_df[c] <- mode_col

    } else { # continuous vars

      col_over_dfs <- lapply(x, function(x) x[[c]])
      col_over_dfs <- data.frame(col_over_dfs)
      mean_col <- apply(col_over_dfs, 1, mean)

      result_df[c] <- mean_col

    }

  }

  return(result_df)
}
