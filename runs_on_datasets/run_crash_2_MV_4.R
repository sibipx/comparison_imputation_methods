# load config
source("config.R", chdir = TRUE)

# load data
# data is downloaded from here: https://hbiostat.org/data/
load(paste0(data_path, "/crash2.rda"))
data <- crash2
rm(crash2)

# data preparation
# ----------------

# calculate outcome - death in hospital within 4 weeks of injury
# https://www.wikijournalclub.org/wiki/CRASH-2
data <- data %>%
  mutate(time_lag = as.numeric(difftime(ddeath, trandomised, units = "days")),
         death = case_when(is.na(time_lag) ~ "No",
                           time_lag <= 28 ~ "Yes",
                           TRUE ~ "No")) %>%
  select(-time_lag)

# remove non-features
data <- data %>%
  select(-c(entryid, trandomised, outcomeid, ddeath, scauseother, ddischarge, boxid,
            packnum, cause, status, condition))

# make categorical variables categorical
# docu here: https://epistasislab.github.io/pmlb/profile/1201_BNG_breastTumor.html
data <- data %>%
  mutate_at(vars(bheadinj, bneuro, bchest, babdomen, bpelvis, bpe, bdvt, bstroke,
                 bbleed, bmi, bgi, bloading, bmaint, btransf, bvii, death), as.factor)

# collapse source levels - only 6 telephone entered manually
data <- data %>%
  mutate(source = as.character(source),
         source = case_when(source == "electronic CRF by email" ~ "electronic",
                            source == "electronic CRF" ~ "electronic",
                            source == "paper CRF enteredd in electronic CRF" ~ "paper_or_telephone",
                            source == "telephone" ~ "paper_or_telephone",
                            source == "telephone entered manually" ~ "paper_or_telephone",
                            TRUE ~ NA_character_),
         source = as.factor(source))

#data %>% count(source) %>% arrange(desc(n)) %>% mutate(p = n/sum(n))
#data %>% count(is.na(status)) %>% arrange(desc(n))
#hist(data$ncell)

# remove some sparse variables (less than 300 in smallest category)
data <- data %>%
  select(-c(bpe, bdvt, bstroke, bmi, bgi, bloading))

# make labelled variables cont
data <- data %>%
  mutate_at(vars(injurytime, sbp, rr, cc, hr, gcseye, gcsmotor, gcsverbal, gcs,
                 ndaysicu, ncell, nplasma, nplatelets, ncryo), as.numeric)

# data_type(data) -> foo

# dataset specific config
# -----------------------

outcome_col <- "death"
dataset_name <- "CRASH 2"
positive_class <- "Yes"

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

# part 4 - run iter 76 to 100
i_start <- 76
j_start <- 75

N_iter <- 100

# create predictor matrix (to impute only missing variables)
predictor_matrix <- missForestPredict::create_predictor_matrix(data[,colnames(data) != outcome_col])
vars_to_skip <- colnames(data)[!colnames(data) %in% data_miss$Variable & !colnames(data) == outcome_col]
predictor_matrix[vars_to_skip,] <- 0

# for quick test
# preds <- c("injurytype", "nplasma", "nplatelets")
# data <- data[1:3000, c(preds, outcome_col)]
# predictor_matrix <- predictor_matrix[preds, preds]

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)

save(all_results, file = sprintf("results/results_4_%s.RData", dataset_name))
