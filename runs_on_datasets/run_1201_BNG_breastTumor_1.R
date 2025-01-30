# load config
source("config.R", chdir = TRUE)

# # load data
# library(pmlbr)
# data <- fetch_data("1201_BNG_breastTumor")
# # save data
# save(data, file = "data/1201_BNG_breastTumor.rda")
# read data
load(paste0(data_path, "/1201_BNG_breastTumor.rda"))
str(data)

# keep only half of the data
set.seed(seed)
n_keep <- floor(nrow(data) * 0.5)
id_keep <- sample(1:nrow(data), n_keep)
data <- data[id_keep,]
# data preparation
# ----------------

# make categorical variables categorical
# docu here: https://epistasislab.github.io/pmlb/profile/1201_BNG_breastTumor.html
data <- data %>%
  mutate_at(vars(node.caps, breast, irradiation, recurrence, menopause), as.factor)

#hist(data$target, breaks = 100)
#data_type(data)

# dataset specific config
# -----------------------
dataset_name <- "breast tumor"
outcome_col <- "target"

simulate_MV <- TRUE

# part 1 - run iter 1 to 50
N_iter <- 50

# run
# ---

predictor_matrix <- NULL

# for quick test
# preds <- c("node.caps", "age", "deg.malig")
# data <- data[1:3000, c(preds, outcome_col)]
# predictor_matrix <- predictor_matrix[preds, preds]

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = NULL,
                               predictor_matrix = predictor_matrix,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)
save(all_results, file = sprintf("results/results_%s.RData", dataset_name))
