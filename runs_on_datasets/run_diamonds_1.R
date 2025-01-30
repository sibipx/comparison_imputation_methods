# load config
source("config.R", chdir = TRUE)

# data preparation
# ----------------

data(diamonds)
data <- diamonds
rm(diamonds)

# correct outliers
#data %>% filter(y > 30)
#data %>% filter(z > 30)

data <- data %>%
  mutate(y = case_when(y > 50 ~ 8.09,
                     y > 30 ~ 5.15,
                     TRUE ~ y),
         z = case_when(z > 30 ~ 0.6 * 5.15,
                     TRUE ~ z))

#data %>% filter(x == 0 | y == 0)
#data %>% filter(z == 0)

# data %>%
#   mutate(foo =  2 * z / (x + y)) %>%
#   ggplot(aes(depth, foo)) +
#   geom_point()

data <- data %>%
  group_by(carat,cut) %>%
  mutate(x = case_when(x == 0 ~ mean(x[x!=0]),
                       TRUE ~ x),
         y = case_when(y == 0 ~ mean(y[y!=0]),
                       TRUE ~ y)) %>%
  ungroup()

data <- data %>%
  mutate(z = case_when(z == 0 ~ depth * (x + y) / 200,
                       TRUE ~ z))

data <- data %>%
  group_by(carat,cut) %>%
  mutate(table = case_when(table > 90 ~ mean(table[table < 90]),
                           TRUE ~ table)) %>%
  ungroup()

# code ordinal variables as continuous
data <- data %>%
  mutate(cut = as.numeric(cut),
         color = as.numeric(color),
         clarity = as.numeric(clarity))

# rename columns x, y, z to avoid error:
# Error in miceObj$finalImps[[x]] : no such index at level 2
# the error seems to be fixed in 1.5.1 version on github
# but only 1.5.0 is available on cran for the moment
data <- data %>%
  rename(dia_x = x,
         dia_y = y,
         dia_z = z)

# make dataframe (needed for caret)
data <- as.data.frame(data)

# dataset specific config
# -----------------------
dataset_name <- "diamonds"
outcome_col <- "price"

simulte_MV <- TRUE

# part 1 - run iter 1 to 50
N_iter <- 50

# run
# ---

predictor_matrix <- NULL

# for quick test
# preds <- c("carat", "cut", "color")
# data <- data[1:3000, c(preds, outcome_col)]
# predictor_matrix <- predictor_matrix[preds, preds]

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulte_MV, positive_class = NULL,
                               predictor_matrix = predictor_matrix,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)

save(all_results, file = sprintf("results/results_%s.RData", dataset_name))
