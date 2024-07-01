# load config
source("config.R", chdir = TRUE)

# load data
# data is downloaded from here: https://hbiostat.org/data/
load(paste0(data_path, "/diabetic_data_processed.rda"))

# dataset specific config
# -----------------------

outcome_col <- "readmitted"
dataset_name <- "diabetes"
positive_class <- "YES"

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

save(data_miss, file = sprintf("results/descriptives_MV_%s.RData", dataset_name))

# run
# ---

# create predictor matrix (to impute only missing variables)
predictor_matrix <- missForestPredict::create_predictor_matrix(data[,colnames(data) != outcome_col])
vars_to_skip <- colnames(data)[!colnames(data) %in% data_miss$Variable & !colnames(data) == outcome_col]
predictor_matrix[vars_to_skip,] <- 0

# part 1 - run iter 1 to 50
N_iter <- 50

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix)

save(all_results, file = sprintf("results/results_%s.RData", dataset_name))
