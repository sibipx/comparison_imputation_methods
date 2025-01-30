# libraries and functions
library(MASS)
library(data.table)
library(tidyverse)
library(caret)
library(reticulate)
library(miceRanger)
library(missForestPredict)
library(ranger)
library(tuneRanger)
library(glmnet)
library(doParallel)
library(readxl)
library(splines)
library(rms)
library(pROC)
library(xgboost)
library(mlrMBO)
library(ParamHelpers)
library(DiceKriging)
library(precrec)

# source all files in the R directory
files_to_source <- list.files("R/", pattern = "*.R", full.names = TRUE)
invisible(lapply(files_to_source, function(x) source(x, chdir = TRUE)))

n_cpu <- detectCores()

# datasets directory
data_path <- "data"

config_python <- TRUE

if (config_python){

  if (Sys.info()["sysname"] == "Windows"){
    # python imputation config
    use_python("C:/Python/Python310/python.exe")

  } else {
    use_python("/apps/leuven/rocky8/skylake/2021a/software/Python/3.10.8-GCCcore-10.3.0-bare/bin")
  }

  source_python("R/impute_IterativeImputer.py")
}

# number of iterations (train/test splits)
N_iter <- 100

# fraction for test set
frac_test <- 1/3

# proportion missing to be simulated on complete datasets (MCAR)
p_miss <- 0.3

# control maximum iteration for missForest and mice (for quick tests only, left on default of each package)
maxiter_miceRanger <- 5
maxiter_missForest <- 10
maxiter_mice <- 5

# for tuning RF, set warmup iterations and optimization iterations (default tuneRanger is 30 and 70)
iters_warmup <- 20
iters_optim <- 30

# use n_fold CV for glmnet and xgb
n_fold <- 5

# parameters to skip imputation methods
skip_caret_knn <- TRUE
skip_caret_bagging <- TRUE

skip_mean_mode <- FALSE
skip_IterativeImputer <- FALSE
skip_missForest <- FALSE
skip_missForest_md10 <- FALSE
skip_miceRanger <- FALSE
skip_mice_RF <- FALSE
skip_mice_default <- FALSE
skip_tidy_linear <- FALSE
skip_tidy_bagging <- FALSE
skip_tidy_knn <- FALSE
skip_original <- FALSE # run models with no imputation when complete data is available

if (!config_python) skip_IterativeImputer <- TRUE

# parameters to skip prediction models
skip_LASSO <- TRUE

skip_RF <- FALSE
skip_Ridge <- FALSE
skip_XGB <- FALSE
skip_cubic_splines <- FALSE

# default seed
seed <- 2024

# start i and j for iterations (used to split the diabetes run in 2 jobs)
i_start <-  1
j_start <- 0

# parameter to save datasets for distribution comparison
# (storage requirements can be high for the large datasets over many iterations)
save_dataset <- FALSE
# parameter to run prediction models (if FALSE, one can only save imputations and compare distributions)
run_pred_model <- TRUE
