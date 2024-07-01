# functions to optimize hyperparameters for XGB using MBO
# inspired from: https://www.r-bloggers.com/2021/01/bayesian-model-based-optimization-in-r/

obj_fun <- makeSingleObjectiveFunction(
  name = "xgb_cv_bayes",
  fn =   function(x){
    cv <- xgb.cv(params = list(booster          = "gbtree",
                               eta              = x["eta"],
                               max_depth        = x["max_depth"],
                               min_child_weight = x["min_child_weight"],
                               #gamma            = x["gamma"],
                               subsample        = x["subsample"],
                               colsample_bytree = x["colsample_bytree"],
                               objective        = dynGet("objective", inherits = TRUE), # objective minimized when fitting
                               metrics = dynGet("cv_metric", inherits = TRUE)), # metrics is used for CV
                 data = dynGet("data_train_xgb", inherits = TRUE), # get from global.Env()
                 nround = dynGet("n_rounds", inherits = TRUE), # Set this large and use early stopping
                 nthread = n_cpu, # parallel
                 nfold =  n_fold,
                 prediction = FALSE,
                 showsd = FALSE,
                 early_stopping_rounds = 25, # If evaluation metric does not improve on out-of-fold sample for 25 rounds, stop
                 verbose = 0,
                 print_every_n = 500)

    if (is.numeric(dynGet("y_train", inherits = TRUE))){
      cv_eval <- cv$evaluation_log %>% pull(test_rmse_mean) %>% min()
    } else {
      cv_eval <- cv$evaluation_log %>% pull(test_logloss_mean) %>% min()
    }

    cv_eval
  },
  par.set = makeParamSet(
    makeNumericParam("eta",                    lower = 0.001, upper = 0.1),
    #makeNumericParam("gamma",                  lower = 0,     upper = 5),
    makeIntegerParam("max_depth",              lower= 1,      upper = 10),
    makeIntegerParam("min_child_weight",       lower= 1,    upper = 10),
    makeNumericParam("subsample",              lower = 0.2,  upper = 1),
    makeNumericParam("colsample_bytree",       lower = 0.2,  upper = 1)
  ),
  minimize = TRUE
)

do_bayes <- function(n_design = NULL, opt_steps = NULL, of = obj_fun, plot = FALSE) {
  des <- generateDesign(n = n_design,
                        par.set = getParamSet(of),
                        fun = lhs::randomLHS)
  control <- makeMBOControl() %>%
    setMBOControlTermination(., iters = opt_steps)
  ## kriging with a matern(3,2) covariance function is the default surrogate model for numerical domains
  ## but if you wanted to override this you could modify the makeLearner() call below to define your own
  ## GP surrogate model with more or less smoothness, or use an entirely different method
  run <- mbo(fun = of,
             design = des,
             learner = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE)),
             control = control,
             show.info = FALSE)
  if (plot){
    opt_plot <- run$opt.path$env$path %>%
      mutate(Round = row_number()) %>%
      mutate(type = case_when(Round <= n_design ~ "Design",
                              TRUE ~ "mlrMBO optimization")) %>%
      ggplot(aes(x= Round, y= y, color= type)) +
      geom_point() +
      labs(title = "mlrMBO optimization")
  } else {
    opt_plot <- NULL
  }
  #print(run$x)
  return(list(run = run, plot = opt_plot))
}
