#' Evaluate variable-wise performance.
#'
#' Evaluate variable-wise performance.
#'
#' @param data_imp imputed dataset
#' @param data_miss dataset with missing values
#' @param data_orig original complete dataset
#' @param method method name
#' @param type train or test
#' @param i iteration i
#' @param j iteration j
#' @param dataset_name dataset name
#'
#' @return results

evaluate_var_wise <- function(data_imp, data_miss, data_orig,
                              method,
                              type, dataset_name, i, j){

  results <- init_results()

  # imputation error
  err_var_wise <- evaluate_imputation_error(data_imp, data_miss, data_orig)

  err_var_wise <- err_var_wise %>% mutate(method = method)

  err_var_wise <- err_var_wise %>%
    pivot_longer(cols = c("MSE", "NMSE", "MER", "macro_F1", "F1_score"), names_to = "metric") %>%
    filter(!is.na(value)) %>%
    mutate(iteration_i = i, iteration_j = j, type = type, dataset = dataset_name)

  results <- results %>%
    add_row(err_var_wise)

  return(results)
}
