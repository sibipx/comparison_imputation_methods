#' Build cross-validated RF model using train data
#'
#' Build cross-validated RF model using train data. Uses tuneRanger to do CV.
#'
#' @param data_train train set X (imputed)
#' @param y_train train set - outcome
#' @param return_var_imp return variable importance for the prediction model?
#'
#' @return final RF model

fit_RF_tune <- function(data_train, y_train, return_var_imp = FALSE){

  # set measure and task ifo type of outcome
  if (is.numeric(y_train)){
    measure <- list(mse)
    task <- makeRegrTask(data = as.data.frame(cbind(data_train, y_train)),
                         target = "y_train")
  } else {
    measure <- list(logloss)
    task <- makeClassifTask(data = as.data.frame(cbind(data_train, y_train)),
                            target = "y_train")
  }

  # set parametes ifo return_var_imp
  if (return_var_imp){
    parameters <- list(replace = TRUE,
                      respect.unordered.factors = "order",
                      importance = "permutation",
                      scale.permutation.importance = TRUE)
  } else {
    parameters <- list(replace = TRUE,
                      respect.unordered.factors = "order")
  }

  # build RF model
  res <- tuneRanger(task, measure = measure,
                    num.trees = 500,
                    iters.warmup = iters_warmup,
                    iters = iters_optim,
                    show.info = FALSE,
                    tune.parameters = c("mtry", "min.node.size"),
                    parameters = parameters)

  RF_model <- res$model$learner.model

  return(RF_model)
}
