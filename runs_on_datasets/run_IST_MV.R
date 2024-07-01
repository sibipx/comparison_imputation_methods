# load config
source("config.R", chdir = TRUE)

# load data
# data is downloaded from here (Electronic supplementary material):
# https://link.springer.com/article/10.1186/1745-6215-12-101
data <- read_csv(paste0(data_path, "/13063_2010_637_MOESM1_ESM.CSV"),
                 col_types = cols(.default = col_character(),
                 RDELAY = col_number(),
                 AGE = col_number(),
                 RSBP = col_number(),
                 HOURLOCAL = col_number()))

# data preparation
# ----------------

# select variables at baseline + COUNTRY + DDEAD (outcome).
# don't keep date and minutes
data <- data %>%
  select(c(RDELAY, RCONSC, SEX, AGE, RSLEEP, RATRIAL, RCT, RVISINF, RHEP24, RASP3,
         RSBP, RDEF1, RDEF2, RDEF3, RDEF4, RDEF5, RDEF6, RDEF7, RDEF8, STYPE,
         HOURLOCAL, DAYLOCAL, RXASP, RXHEP, DDEAD, COUNTRY))

#data %>% count(DDEAD) %>% arrange(desc(n)) %>% mutate(p = n/sum(n))
#data %>% count(is.na(RXHEP)) %>% arrange(desc(n)) %>% mutate(p = n/sum(n))
#hist(data$HOURLOCAL)

# make ordinal variable RXHEP continuous
data <- data %>%
  mutate(RXHEP = case_when(RXHEP == "N" ~ 0, # none
                           RXHEP == "L" ~ 1, # low
                           TRUE ~ 2)) # H and M are highest, both 12.500 units

# drop NA's and UNKNOWN on outcome
data <- data %>%
  mutate(DDEAD = if_else(DDEAD == "U", NA_character_, DDEAD))

data %>% count(DDEAD) %>% arrange(desc(n)) %>% mutate(p = n/sum(n)) %>% mutate(p = make_percent(p))
# 25 0.13% with NA or UNKNOWN outcome

data <- data %>%
  filter(!is.na(DDEAD))

# country code - collapse sparse categories
country_prev <- data %>% count(COUNTRY) %>% arrange(desc(n)) %>% mutate(p = n/sum(n))
countries_exclude <- country_prev %>%
  filter(n < 300) %>%
  pull(COUNTRY)

# collapse countries that contribute with less than 300 patients
data <- data %>%
  mutate(COUNTRY = if_else(COUNTRY %in% countries_exclude, "Other", COUNTRY))

# make all C ("can't assess") for RDEF (deficit assessment) as missing
data <- data %>%
  mutate(across(starts_with('RDEF'), function(x) if_else(x == "C", NA_character_, x)))

# make categorical variables factor
data <- data %>%
  mutate_at(vars(RCONSC, SEX, RSLEEP, RATRIAL, RCT, RVISINF, RHEP24, RASP3,
                 RDEF1, RDEF2, RDEF3, RDEF4, RDEF5, RDEF6, RDEF7, RDEF8, STYPE,
                 DAYLOCAL, RXASP, DDEAD, COUNTRY), as.factor)

# dataset specific config
# -----------------------

dataset_name <- "IST"
outcome_col <- "DDEAD"
positive_class <- "Y"

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

# missing values
#map(data, ~sum(is.na(.))) %>%
#  as_tibble() %>%
#  pivot_longer(cols = everything(), names_to = "Variable", values_to = "number_missing") %>%
#  mutate(`Percentage missing` = make_percent(number_missing/dim(data)[[1]])) %>%
#  arrange(desc(number_missing)) %>%
#  filter(number_missing > 0)

# delete 3 variables with missing values
#data <- data %>%
#  select(-c(RATRIAL, RASP3, RHEP24))

#data_type(data) %>%
#  filter(`Variable type`  != "continuous",
#         `Variable type` != "categorical, 2 categories")

# run
# ---

# don't run mice default on large datasets (tooo slow)
skip_mice_default <- TRUE

# create predictor matrix (to impute only missing variables)
predictor_matrix <- missForestPredict::create_predictor_matrix(data[,colnames(data) != outcome_col])
vars_to_skip <- colnames(data)[!colnames(data) %in% data_miss$Variable & !colnames(data) == outcome_col]
predictor_matrix[vars_to_skip,] <- 0

# for quick test
# preds <- c("AGE", "STYPE", "RCT")
# data <- data[1:3000, c(preds, outcome_col)]
# predictor_matrix <- predictor_matrix[preds, preds]

print(sprintf("Dataset %s", dataset_name))

skip_mice_default <- TRUE

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix)
save(all_results, file = sprintf("results/results_%s.RData", dataset_name))
