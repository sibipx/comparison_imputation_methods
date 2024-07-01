#' Initializes empty results dataframe
#'
#' Initializes empty results dataframe (explained in README.md)
#'
#' @return Empty results dataframe

init_results <- function(){

  results <- tibble(iteration_i = numeric(),
                    iteration_j = numeric(),
                    dataset = character(),
                    metric = character(),
                    value = numeric(),
                    variable = character(),
                    method = character(),
                    type = character(),
                    fail = logical(),
                    time_sec = numeric(),
                    obj_size_bytes = numeric(),
                    model = character())

  return(results)
}
