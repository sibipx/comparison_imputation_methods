
# libraries
library(randomForest)
library(missRanger)
library(caret)
library(mice)
#library(NADIA)
library(CALIBERrfimpute)
library(miceRanger)
#library(imputeMissings)
library(missForestPredict)

library(tidyverse)

# iris contains 4 continuous variables and one categorical (3 categories)
data(iris)
set.seed(2024)
iris_miss <- missForestPredict::produce_NA(iris, 0.1)

# init table to store evaluation results
results_evaluate_RF_model <- tibble(`method` = character(),
                        `(1) Can impute a single new observation?` = character(),
                        `(2) Supports unsupervised imputation (= outcome not present)?` = character(),
                        `(3) Supports both continuous and categorical variables?` = character(),
                        `(4) Allows for new missingness patterns?` = character())

# How are the methods evaluated?
# ------------------------------

# (1) Can impute a single new observation?
# The first observation (a dataframe with one row) is passed to the main function
# or to any additional (prediction) function, if available and documented in the package documentation.
# If a complete observation is returned, this test passes.

# (2) Supports unsupervised imputation (= outcome not present)?
# This criteria is implicitly evaluated, as most methods do not require an outcome.
# The user can choose to pass a dataframe with or without an outcome.
# This criteria fails only if the function requires an outcome explicitly (like rfImpute does)

# (3) Supports both continuous and categorical variables?
# A dataframe with both continuous and categorical variables is passed (iris has one factor variable)
# Imputation of the categorical column is checked (by visualizing the output)

# (4) Allows for new missingness patterns?
# A dataframe with missing values on all but one complete variable is passed.
# A new observation is created (first observation of the dataframe).
# The complete variable is set as NA for this observation.
# This new observation is imputed. If a complete observation is returned, the test passes.
# If criteria (1) is not met, crtieria (4) is implicitely considered not met.

# R - randomForest (rfImpute)
# ---------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
# ?rfImpute documentation states: y	Response vector (NA's not allowed).
# Note: Imputation has not (yet) been implemented for the unsupervised case.
randomForest_crit_2 <- "NO"

# rfImpute needs the outcome, let's create a fake outcome
fake_ouctome <- c(rep("YES", floor(dim(iris)[[1]]/3)),
                  rep("NO", dim(iris)[[1]] - floor(dim(iris)[[1]]/3)))
fake_ouctome <- as.factor(fake_ouctome)

# (1) Can impute a single new observation?
randomForest::rfImpute(iris_miss[3,], fake_ouctome[3,])
# NOK, and no predict function available
randomForest_crit_1 <- "NO"

# (3) Supports both continuous and categorical variables?
randomForest::rfImpute(iris_miss, fake_ouctome)
# ok
randomForest_crit_3 <- "YES"

# (4) Allows for new missingness patterns?
# criteria 4 is NO if criteria 1 is NO
randomForest_crit_4 <- "NO"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - randomForest (rfImpute)",
          `(1) Can impute a single new observation?` = randomForest_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = randomForest_crit_2,
          `(3) Supports both continuous and categorical variables?` = randomForest_crit_3,
          `(4) Allows for new missingness patterns?` = randomForest_crit_4)

# R - missRanger (missRanger)
# ---------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
missRanger_crit_2 <- "YES"

# (3) Supports both continuous and categorical variables?
missRanger::missRanger(iris_miss)
# ok
missRanger_crit_3 <- "YES"

# (1) Can impute a single new observation?
missRanger::missRanger(iris_miss[3,])
# no error, not imputed, no predict function available
missRanger_crit_1 <- "NO"

# (4) Allows for new missingness patterns?
# criteria 4 is NO if criteria 1 is NO
missRanger_crit_4 <- "NO"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - missRanger (missRanger)",
          `(1) Can impute a single new observation?` = missRanger_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = missRanger_crit_2,
          `(3) Supports both continuous and categorical variables?` = missRanger_crit_3,
          `(4) Allows for new missingness patterns?` = missRanger_crit_4)

# R - mice (mice method=”rf”)
# ---------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
mice_crit_2 <- "YES"

# (3) Supports both continuous and categorical variables?
mice_RF <- mice::mice(iris_miss, method = c("rf", "rf", "rf", "rf", "rf"),
                m = 3, maxit = 5, ntree = 100)
complete(mice_RF)
# ok
mice_crit_3 <- "YES"

# (1) Can impute a single new observation?
mice::mice(iris_miss[3,], method = c("rf", "rf", "rf", "rf", "rf"),
           m = 3, maxit = 5, ntree = 100)
# nok
imp_obs <- mice.mids(mice_RF, newdata = iris_miss[3,])
imp_obs$imp
# ok 3 imputated values for Petal.Width

mice_crit_1 <- "YES"

# (4) Allows for new missingness patterns?
# criteria 4 is NO if criteria 1 is NO
mice_crit_4 <- "NO"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - mice (method=”rf”)",
          `(1) Can impute a single new observation?` = mice_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = mice_crit_2,
          `(3) Supports both continuous and categorical variables?` = mice_crit_3,
          `(4) Allows for new missingness patterns?` = mice_crit_4)

# R - CALIBERrfimpute (mice method=c(”rfcat”, “rfcont))
# ----------------------------------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
CALIBER_crit_2 <- "YES"

# (3) Supports both continuous and categorical variables?

# Note: library(CALIBERrfimpute) is needed for these methods
mice_RF_caliber <- mice::mice(iris_miss,
                              method = c("rfcont", "rfcont", "rfcont", "rfcont", "rfcat"),
                              m = 3, maxit = 5)
complete(mice_RF_caliber) %>% head
# ok
CALIBER_crit_3 <- "YES"

# (1) Can impute a single new observation?
mice::mice(iris_miss[3,],
           method = c("rfcont", "rfcont", "rfcont", "rfcont", "rfcat"),
           m = 3, maxit = 5)
# NOK
# try with NADIA
# NADIA::mice.reuse(mice_RF_caliber, iris_miss[3,], maxit = 5)
# NOK

CALIBER_crit_1 <- "NO"

# (4) Allows for new missingness patterns?
# criteria 4 is NO if criteria 1 is NO
CALIBER_crit_4 <- "NO"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - CALIBERrfimpute",
          `(1) Can impute a single new observation?` = CALIBER_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = CALIBER_crit_2,
          `(3) Supports both continuous and categorical variables?` = CALIBER_crit_3,
          `(4) Allows for new missingness patterns?` = CALIBER_crit_4)

# R – imputeMissings impute(method = "randomForest")
# --------------------------------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
imputeMissings_crit_2 <- "YES"

# (3) Supports both continuous and categorical variables?
imputeMissings_models <- imputeMissings::compute(iris_miss, method = "randomForest")
imputeMissings::impute(iris_miss, object = imputeMissings_models)
# ok
imputeMissings_crit_3 <- "YES"

# (1) Can impute a single new observation?
imputeMissings::impute(iris_miss[3,], object = imputeMissings_models)
# NOK, error because it tries to initialize with median / mode from 1 observation (from test set iso train set)
imputeMissings_crit_1 <- "NO"

# (4) Allows for new missingness patterns?
imputeMissings_crit_4 <- "NO"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R – imputeMissings impute(method = 'randomForest')",
          `(1) Can impute a single new observation?` = imputeMissings_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = imputeMissings_crit_2,
          `(3) Supports both continuous and categorical variables?` = imputeMissings_crit_3,
          `(4) Allows for new missingness patterns?` = imputeMissings_crit_4)

# missForest
# ----------

missForest_crit_1 <- "NO"
missForest_crit_2 <- "YES"
missForest_crit_3 <- "YES"
missForest_crit_4 <- "NO"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - missForest",
          `(1) Can impute a single new observation?` = missForest_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = missForest_crit_2,
          `(3) Supports both continuous and categorical variables?` = missForest_crit_3,
          `(4) Allows for new missingness patterns?` = missForest_crit_4)

# R - caret (bagImp)
# ------------------

# (2) Supports unsupervised imputation (= outcome not present)?
caret_crit_2 <- "YES"

# (3) Supports both continuous and categorical variables?
bag_imp_model <- caret::preProcess(iris_miss, method = c("bagImpute"))
iris_imp_bag <- predict(bag_imp_model, iris_miss)
# NOK

# remove Species, re-do
bag_imp_model <- caret::preProcess(iris_miss[,1:4], method = c("bagImpute"))
iris_imp_bag <- predict(bag_imp_model, iris_miss[,1:4])
# ok after factor variable is removed

caret_crit_3 <- "NO"

# (1) Can impute a single new observation?
(new_observation <- iris_miss[3,1:4])
#new_observation[1,"Species"] <- NA
(new_observation_imp <- predict(bag_imp_model, new_observation))
# ok

caret_crit_1 <- "YES"

# (4) Allows for new missingness patterns?
# keep Petal.Width complete
iris_miss_2 <- iris_miss
iris_miss_2$Petal.Width <- iris$Petal.Width

bag_imp_model <- caret::preProcess(iris_miss_2[,1:4], method = c("bagImpute"))

# make new observation with Petal.Width missing
(new_observation <- iris_miss[3,1:4])
new_observation[1,4] <- NA
new_observation

(new_observation_imp <- predict(bag_imp_model, new_observation))
# ok

caret_crit_4 <- "YES"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - caret (bagImp)",
          `(1) Can impute a single new observation?` = caret_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = caret_crit_2,
          `(3) Supports both continuous and categorical variables?` = caret_crit_3,
          `(4) Allows for new missingness patterns?` = caret_crit_4)

# R - tidymodels (bagging)
# ------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
tidy_crit_2 <- "YES"

# (3) Supports both continuous and categorical variables?
bag_imp_model <- recipes::recipe(x = iris_miss) %>%
  recipes::step_impute_bag(all_of(colnames(iris_miss)),
                           impute_with = colnames(iris_miss)) %>%
  recipes::prep(training = iris_miss)
iris_imp_bag <- bag_imp_model %>%
  recipes::bake(new_data = iris_miss)
iris_imp_bag[147,]
# ok

tidy_crit_3 <- "YES"

# (1) Can impute a single new observation?
(new_observation <- iris_miss[3,])
#new_observation[1,"Species"] <- NA
(new_observation_imp <- bag_imp_model %>%
    recipes::bake(new_data = new_observation))
# ok

tidy_crit_1 <- "YES"

# (4) Allows for new missingness patterns?
# keep Petal.Width complete
iris_miss_2 <- iris_miss
iris_miss_2$Petal.Width <- iris$Petal.Width

bag_imp_model <- recipes::recipe(x = iris_miss_2) %>%
  recipes::step_impute_bag(all_of(colnames(iris_miss_2)),
                           impute_with = colnames(iris_miss_2)) %>%
  recipes::prep(training = iris_miss_2)

# make new observation with Petal.Width missing
(new_observation <- iris_miss[3,])
new_observation[1,4] <- NA
new_observation

(new_observation_imp <- bag_imp_model %>%
    recipes::bake(new_data = new_observation))
# ok

tidy_crit_4 <- "YES"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - tidymodels (bagging)",
          `(1) Can impute a single new observation?` = tidy_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = tidy_crit_2,
          `(3) Supports both continuous and categorical variables?` = tidy_crit_3,
          `(4) Allows for new missingness patterns?` = tidy_crit_4)

# missForestPredict
# -----------------

missForest_v2_crit_1 <- "YES"
missForest_v2_crit_2 <- "YES"
missForest_v2_crit_3 <- "YES"
missForest_v2_crit_4 <- "YES"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R - missForestPredict",
          `(1) Can impute a single new observation?` = missForest_v2_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = missForest_v2_crit_2,
          `(3) Supports both continuous and categorical variables?` = missForest_v2_crit_3,
          `(4) Allows for new missingness patterns?` = missForest_v2_crit_4)

# Python - scikit-learn 1.0 (IterativeImputer, estimator=ExtraTreesRegressor)
# ---------------------------------------------------------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
IterativeImputer_crit_2 <- "YES"
# (1) Can impute a single new observation?
IterativeImputer_crit_1 <- "YES"
# (3) Supports both continuous and categorical variables?
IterativeImputer_crit_3 <- "NO"
# (4) Allows for new missingness patterns?
IterativeImputer_crit_4 <- "YES"

# R – miceRanger (miceRanger)
# ---------------------------

# (2) Supports unsupervised imputation (= outcome not present)?
miceRanger_crit_2 <- "YES"

# (3) Supports both continuous and categorical variables?
miceObj <- miceRanger::miceRanger(iris_miss, m = 5, returnModels = TRUE,
                                  verbose = TRUE)
imp_data <- miceRanger::impute(iris_miss, miceObj, verbose = TRUE)
imp_data$imputedData
# ok

miceRanger_crit_3 <- "YES"

# (1) Can impute a single new observation?
(new_observation <- iris_miss[3,])
miceRanger::impute(new_observation,miceObj,verbose=TRUE)
# NOK, error
(new_observation <- data.table(new_observation))
new_observation_imp <- miceRanger::impute(new_observation,miceObj,verbose=TRUE)
new_observation_imp$imputedData
# returns 5 imputed observations, ok
# but gives warning:
# Warning message:
# In impute(new_observation, miceObj, verbose = TRUE) :
#   At least 1 column in data contains all missing values. Imputations may be questionable.

miceRanger_crit_1 <- "YES"

# (4) Allows for new missingness patterns?
# keep Species complete
iris_miss_2 <- iris_miss
iris_miss_2$Species <- iris$Species

miceObj_2 <- miceRanger::miceRanger(iris_miss_2, m = 5, returnModels = TRUE,
                                    verbose = TRUE)

# make new observation with Species missing
(new_observation_2 <- iris_miss[3,])
new_observation_2[1,5] <- NA
(new_observation_2 <- data.table(new_observation_2))

new_observation_imp_2 <- miceRanger::impute(new_observation_2,miceObj_2,verbose=TRUE)
new_observation_imp_2$imputedData
# OK

miceRanger_crit_4 <- "YES"

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "R – miceRanger (miceRanger)",
          `(1) Can impute a single new observation?` = miceRanger_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = miceRanger_crit_2,
          `(3) Supports both continuous and categorical variables?` = miceRanger_crit_3,
          `(4) Allows for new missingness patterns?` = miceRanger_crit_4)

results_evaluate_RF_model <- results_evaluate_RF_model %>%
  add_row(`method` = "Python - scikit-learn 1.0 (IterativeImputer, estimator=RandomForestRegressor)",
          `(1) Can impute a single new observation?` = IterativeImputer_crit_1,
          `(2) Supports unsupervised imputation (= outcome not present)?` = IterativeImputer_crit_2,
          `(3) Supports both continuous and categorical variables?` = IterativeImputer_crit_3,
          `(4) Allows for new missingness patterns?` = IterativeImputer_crit_4)

save(results_evaluate_RF_model,
     file = "results/evaluate_RF_methods_results.RData")
