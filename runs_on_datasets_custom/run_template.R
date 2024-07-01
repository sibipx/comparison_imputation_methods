# load config
source("config.R", chdir = TRUE)

# MODIFY - load data
# ------------------

# load data as data.frame (matrix is not supported, tibble and data.frame are supported)
# for example:
# data <- read_csv(...)

# MODIFY - data preparation
# -------------------------

# add any pre-processing steps for the data

# MODIFY - dataset specific config
# --------------------------------

dataset_name <- "dataset_name" # give your dataset a distinct name to be shown in results
outcome_col <- "outcome_col" # the name of the outcome column in data
positive_class <- "positive_class" # the name of the positive class for binary outcome (can be NULL for continuous outcome)

# set this to TRUE to simulate MCAR missing values; TRUE only works for complete datasets
# leave on FALSE if the dataset already has missing values
# a mixture of dataset with missing values and simulation of additional missing values is not possible
simulate_MV <- FALSE

# create descriptives for MV
# --------------------------

if (!simulate_MV) dataset_name <- paste0(dataset_name, " - MV")

data_miss <- map(data, ~sum(is.na(.))) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "number_missing") %>%
  mutate(`Percentage missing` = make_percent(number_missing/dim(data)[[1]])) %>%
  arrange(desc(number_missing)) %>%
  filter(number_missing > 0) %>%
  rename(`No. missing` = number_missing)

data_miss <- data_miss %>%
  left_join(data_type(data), by = "Variable") %>%
  mutate(Dataset = dataset_name)

save(data_miss, file = sprintf("results_custom/descriptives_MV_%s.RData", dataset_name))

# run
# ---

# create predictor matrix (to impute only missing variables)
# used only by missForest and miceRangers for datasets that have complete variables - faster
# can be set to NULL if desired
predictor_matrix <- missForestPredict::create_predictor_matrix(data[,colnames(data) != outcome_col])
vars_to_skip <- colnames(data)[!colnames(data) %in% data_miss$Variable & !colnames(data) == outcome_col]
predictor_matrix[vars_to_skip,] <- 0

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix)
save(all_results, file = sprintf("results_custom/results_%s.RData", dataset_name))

# TODO: make a markdown file for visualizing results



