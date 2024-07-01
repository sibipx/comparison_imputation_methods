# load config
source("config.R", chdir = TRUE)

# dataset & MV type config
dataset_name <- "sim_75_7_noise"
MV_type <- "MCAR"
sim_func <- simulate_miss_MCAR

# outcome
outcome_col <- "y_outcome"
positive_class <- "YES"

# load data
load(sprintf("%s/data_%s.RData", data_path, dataset_name))

# make dataset name
dataset_name <- sprintf("%s - %s", dataset_name, MV_type)
print(sprintf("Dataset %s", dataset_name))

skip_original <- TRUE # skip original (run separately)

# run simulations
all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = TRUE, positive_class = positive_class,
                               predictor_matrix = NULL,
                               sim_MV_func = sim_func)

filename <- sprintf("results/results_%s.RData", dataset_name)
save(all_results, file = filename)
