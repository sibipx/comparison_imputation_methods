# load config
source("config.R", chdir = TRUE)

# dataset & MV type config
dataset_name <- "sim_90_1"
MV_type <- "MCAR"
sim_func <- simulate_miss_MCAR

# outcome
outcome_col <- "y_outcome"
positive_class <- "YES"

# load data
load(sprintf("%s/data_%s.RData", data_path, dataset_name))

# make dataset name
dataset_name_2 <- sprintf("%s - %s", dataset_name, MV_type)
print(sprintf("Dataset %s", dataset_name))

# # create predictor matrix (to impute only missing variables)
# predictor_matrix <- missForestPredict::create_predictor_matrix(data[,colnames(data) != outcome_col])
# vars_to_skip <- c("V2", "V4")
# predictor_matrix[vars_to_skip,] <- 0

# run only on original
skip_caret_knn <- TRUE
skip_caret_bagging <- TRUE

skip_mean_mode <- TRUE
skip_IterativeImputer <- TRUE
skip_missForest <- TRUE
skip_missForest_md10 <- TRUE
skip_miceRanger <- TRUE
skip_mice_RF <- TRUE
skip_mice_default <- TRUE
skip_tidy_linear <- TRUE
skip_tidy_bagging <- TRUE
skip_tidy_knn <- TRUE
skip_original <- FALSE # only run original

# run simulations
all_results <- run_simulations(data, outcome_col, N_iter, dataset_name_2, p_miss,
                               simulate_MV = TRUE, positive_class = positive_class,
                               predictor_matrix = NULL,
                               sim_MV_func = NULL)

# remove the stats only for missing data
all_results <- all_results %>%
  filter(!metric %in% c("n_all_missing_train", "n_all_missing_test", "n_train", "n_test",
                        "n_complete_cases_train", "n_complete_cases_test"))

all_results_final <- all_results

for (mv in c("MAR_2", "MAR_2_out", "MAR_circ", "MAR_circ_out", "MNAR")){
  # replace previous type with new type
  all_results <- all_results %>%
    mutate(dataset = str_replace(dataset, MV_type, mv))

  all_results_final <- all_results_final %>%
    add_row(all_results)

  MV_type <- mv
}

all_results <- all_results_final

filename <- sprintf("results/results_%s_orig.RData", dataset_name)
save(all_results, file = filename)
