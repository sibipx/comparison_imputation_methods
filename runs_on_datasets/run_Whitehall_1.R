# load config
source("config.R", chdir = TRUE)

# load data
# data is downloaded from here (dataset no. 20):
# https://www.uniklinik-freiburg.de/imbi/stud-le/multivariable-model-building.html#c134656
data <- read_csv(paste0(data_path, "/whitehall1.csv"),
                 col_types = cols(.default = col_double(),
                                 # all10 = col_factor(),
                                  chd = col_factor()))

# make oucome YES / NO as 0/1 is not supported in caret
data <- data %>%
  mutate(all10 = case_when(all10 == 0 ~ "NO",
                           all10 == 1 ~ "YES",
                           TRUE ~ NA_character_),
         all10 = as.factor(all10))

# data preparation
# ----------------

# make gradd1, gradd2, gradd3 continuos variable (it is in fact ordinal, but we will treat it as cont.)
#data <- data %>%
#  mutate(grade = case_when(gradd1 == 1 ~ 1,
#                           gradd2 == 1 ~ 2,
#                           gradd3 == 1 ~ 3,
#                           TRUE ~ 0)) %>% # I assume the Other grade was lowest (0)
#  select(-c(gradd1, gradd2, gradd3))
## becomes the same as jobgrade
#data %>% count(grade, jobgrade)

# remove _st - always 1; _t0 - always 0; pyar and _t are the same; serno = serial no.
#  _d and _t should be death and time of death (no censoring)
data <- data %>%
  dplyr::select(-c(`_st`, `_t0`, pyar, serno, `_d`, `_t`, gradd1, gradd2, gradd3))

# chd - congestive heart disease
# all10 - indicating if a person died
# map = mean arterial pressure = (sysbp + 2 * diasbp) / 3

# dataset specific config
# -----------------------
dataset_name <- "whitehall1"
outcome_col <- "all10"
positive_class <- "YES"

simulate_MV <- TRUE

# run
# ---

predictor_matrix <- NULL

# for quick test
# preds <- c("jobgrade", "chd", "map", "diasbp")
# data <- data[1:3000, c(preds, outcome_col)]

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)
save(all_results, file = sprintf("results/results_%s.RData", dataset_name))
