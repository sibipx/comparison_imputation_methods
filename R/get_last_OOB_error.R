get_last_OOB_error <- function(missForest_imp_model){

  last_iter <- max(missForest_imp_model$OOB_err$iteration[!is.na(missForest_imp_model$OOB_err$NMSE)])

  if (missForest_imp_model$maxiter != last_iter & last_iter != 1){
    last_iter <- last_iter - 1
  }

  OOB_err_last <- missForest_imp_model$OOB_err[missForest_imp_model$OOB_err$iteration == last_iter,]

  OOB_err_last <- OOB_err_last %>%
    rename(iteration_i = iteration) %>%
    pivot_longer(cols = c("MSE", "NMSE", "MER", "macro_F1", "F1_score"),
                 names_to = "metric") %>%
    filter(!is.na(value))

  return(OOB_err_last)
}
