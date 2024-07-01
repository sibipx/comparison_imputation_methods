# load config
source("config.R", chdir = TRUE)

# load data
library(medicaldata)
data <- medicaldata::covid_testing

# data preparation
# ----------------

# remove variables not useful for prediction
data <- data %>%
  select(-c(subject_id, fake_first_name, fake_last_name, gender, age, col_rec_tat, rec_ver_tat,
            pan_day, test_id, ct_result))
# age and gender are fake
# The col_rec_tat variable records the time elapsed (in hours) between sample collection and receipt in the lab.
# The rec__ver_tat variable records the time elapsed (in hours) between sample receipt in the lab and result verification/posting.
# pan day is the pandemic day - no sense for prediction
# test_id - only 1 value (almost)

# collapse outcome (positive + invalid)
data <- data %>%
  mutate(result = if_else(result == "invalid", "positive", result))

# collapse the clinic_name (all levels < 1%)
clinic_name_prev <- data %>%
  count(clinic_name) %>%
  arrange(desc(n)) %>%
  mutate(p = n/sum(n))

clinic_name_other <- clinic_name_prev %>%
  filter(n < 300) %>%
  pull(clinic_name)

data <- data %>%
  mutate(clinic_name = if_else(clinic_name %in% clinic_name_other, "other", clinic_name))

# make unidentified NA for demo_group
data <- data %>%
  mutate(demo_group = if_else(demo_group == "unidentified", NA_character_, demo_group))

# collapse admit after surgery
data <- data %>%
  mutate(patient_class = if_else(patient_class %in% c("admit after surgery-ip", "admit after surgery-obs", "day surgery"),
                                 "inpatient", patient_class))

# collapse sparse categories
data <- data %>%
  mutate(payor_group = if_else(payor_group %in% c("charity care", "self pay", "medical assistance"),
                               "other", payor_group),
         demo_group = if_else(demo_group %in% c("misc adult", "other adult"), "adult", demo_group))

# make categorical variables factor
data <- data %>%
  mutate_at(vars(clinic_name, result, demo_group, drive_thru_ind, orderset,
                 payor_group, patient_class), as.factor)

# dataset specific config
# -----------------------

outcome_col <- "result"
dataset_name <- "covid"
positive_class <- "positive"

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
  mutate(Dataset = "covid")

save(data_miss, file = sprintf("results/descriptives_MV_%s.RData", dataset_name))

# run
# ---

# create predictor matrix (to impute only missing variables)
predictor_matrix <- missForestPredict::create_predictor_matrix(data[,colnames(data) != outcome_col])
vars_to_skip <- colnames(data)[!colnames(data) %in% data_miss$Variable & !colnames(data) == outcome_col]
predictor_matrix[vars_to_skip,] <- 0

# for quick test
# preds <- c("clinic_name", "drive_thru_ind", "patient_class")
# data <- data[1:3000, c(preds, outcome_col)]
# predictor_matrix <- predictor_matrix[preds, preds]

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix)

save(all_results, file = sprintf("results/results_%s.RData", dataset_name))
