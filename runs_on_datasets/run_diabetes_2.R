# load config
source("config.R", chdir = TRUE)

# load data
# ---------
load(paste0(data_path, "/diabetic_data_processed.rda"))

# exclude columns with missing values
data_miss <- map(data, ~sum(is.na(.))) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "number_missing") %>%
  mutate(percentage_missing = number_missing/dim(data)[[1]]) %>%
  arrange(desc(number_missing)) %>%
  filter(number_missing > 0)

cols_missing <- data_miss$feature[data_miss$percentage_missing > 0]

data <- data %>%
  select(-all_of(cols_missing))

data <- as.data.frame(data)

# dataset specific config
# -----------------------

outcome_col <- "readmitted"
dataset_name <- "diabetes"
positive_class <- "YES"

simulate_MV <- TRUE

# part 2 - run iter 26 to 50
i_start <- 26
j_start <- 25

N_iter <- 50

# run
# ---

predictor_matrix <- NULL

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix)

save(all_results, file = sprintf("results/results_2_%s.RData", dataset_name))

