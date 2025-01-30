# load config
source("config.R", chdir = TRUE)

# data preparation
# ----------------

# Data source: https://archive.ics.uci.edu/ml/datasets/Diabetes+130-US+hospitals+for+years+1999-2008

data <- read_csv(paste0(data_path, "/diabetic_data.csv"), col_types = cols(
  .default = col_character(),
  encounter_id = col_double(),
  patient_nbr = col_double(),
  time_in_hospital = col_double(),
  num_lab_procedures = col_double(),
  num_procedures = col_double(),
  num_medications = col_double(),
  number_outpatient = col_double(),
  number_emergency = col_double(),
  number_inpatient = col_double(),
  number_diagnoses = col_double()
))

# keep only half of the data
set.seed(seed)
n_keep <- floor(nrow(data) * 0.5)
id_keep <- sample(1:nrow(data), n_keep)
data <- data[id_keep,]

data <- data %>%
  dplyr::select(-c(encounter_id, patient_nbr))

data <- data %>%
  mutate(across(all_of(colnames(data)), ~ ifelse(. == "?", NA_character_, .)))

# make outcome binary
#data %>% count(readmitted) %>% mutate(p = n/sum(n))
data <- data %>%
  mutate(readmitted = if_else(readmitted == "<30", "YES", "NO"))

data <- data %>%
  mutate(gender = if_else(gender == "Unknown/Invalid", NA_character_, gender),
         max_glu_serum = if_else(max_glu_serum == "None", NA_character_, max_glu_serum),
         A1Cresult = if_else(A1Cresult == "None", NA_character_, A1Cresult),
         #discharge - #18 - NULL, 25 - Not Mapped, 26 - Unknown/Invalid
         discharge_disposition_id = if_else(discharge_disposition_id %in% c("18", "25", "26"),
                                            NA_character_, discharge_disposition_id),
         # admission - # 5 - Not Available, 6 - NULL, 8 - Not Mapped
         admission_type_id = if_else(admission_type_id %in% c("5", "6", "8"), NA_character_,
                                     admission_type_id),
         # 9 - Not Available, 15 - Not Available, 17 - NULL, 20 - Not Mapped, 21 - Unknown/Invalid
         admission_source_id = if_else(admission_source_id %in% c("9", "15", "17", "20", "21"),
                                       NA_character_, admission_source_id)
  )

# exclude encounters resulting in death (expired) or discharge to palliative care (hospice)
data <- data %>%
  filter(!discharge_disposition_id %in% c("13", "14", "11", "19", "20", "21"))

#13 - Hospice / home, 14 - Hospice / medical facility
#11 - Expired
#19 - "Expired at home. Medicaid only, hospice."
#20 - "Expired in a medical facility. Medicaid only, hospice."
#21 - "Expired, place unknown. Medicaid only, hospice."

# process primary diagnosis (as in Table 2)
data <- data %>%
  mutate(diag_1_main = str_split(diag_1, "\\.", simplify=TRUE)[,1],
         diag_1_main_num = as.numeric(diag_1_main),
         diag_categ = case_when(diag_1_main_num >= 390 & diag_1_main_num <= 459 ~ "Circulatory",
                                diag_1_main_num == 785 ~ "Circulatory",
                                diag_1_main_num >= 460 & diag_1_main_num <= 519 ~ "Respiratory",
                                diag_1_main_num == 786 ~ "Respiratory",
                                diag_1_main_num >= 520 & diag_1_main_num <= 579 ~ "Digestive",
                                diag_1_main_num == 787 ~ "Digestive",
                                diag_1_main_num == 250 ~ "Diabetes",
                                diag_1_main_num >= 800 & diag_1_main_num <= 999 ~ "Injury",
                                diag_1_main_num >= 710 & diag_1_main_num <= 739 ~ "Musculoskeletal",
                                diag_1_main_num >= 580 & diag_1_main_num <= 629 ~ "Genitourinary",
                                diag_1_main_num == 788 ~ "Genitourinary",
                                diag_1_main_num >= 140 & diag_1_main_num <= 239 ~ "Neoplasms",
                                diag_1_main_num >= 790 & diag_1_main_num <= 799 ~ "Neoplasms",
                                diag_1_main_num >= 240 & diag_1_main_num <= 249 ~ "Neoplasms",
                                diag_1_main_num >= 251 & diag_1_main_num <= 279 ~ "Neoplasms",
                                diag_1_main_num >= 680 & diag_1_main_num <= 709 ~ "Neoplasms",
                                diag_1_main_num >= 1 & diag_1_main_num <= 139 ~ "Neoplasms",
                                diag_1_main_num %in% c(780, 781, 784, 782) ~ "Neoplasms",
                                is.na(diag_1_main) ~ NA_character_,
                                TRUE ~ "Other")) %>%
  dplyr::select(-c(diag_1, diag_2, diag_3, diag_1_main, diag_1_main_num))

# sparse variables - exclude variables where the most prevalent category > 99% of the values
data_sparsity <- data %>%
  select_if(is.character) %>%
  map(~max(prop.table(table(.)))) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "class_prevalence") %>%
  arrange(desc(class_prevalence))

sparse_cols <- data_sparsity$feature[data_sparsity$class_prevalence >= 0.99]
sparse_cols <- sparse_cols[sparse_cols != "readmitted"]

data <- data %>%
  dplyr::select(-all_of(sparse_cols))

# make age continuous
data <- data %>%
  mutate(age = str_split(age, pattern = "-", n = Inf, simplify = TRUE)[,1],
         age = str_replace_all(age, "\\[", ""),
         age = as.numeric(age))

# remove weight - extreme missingness - problematic for knn
data <- data %>% dplyr::select(-weight)

# consider medication changes as ordinal
data <- data %>%
  mutate_at(c("metformin",
              "repaglinide", "glimepiride", "glipizide", "glyburide", "pioglitazone",
              "rosiglitazone",
              "insulin"),
            ~ case_when(. == "No" ~ 1,
                        . == "Down" ~ 2,
                        . == "Steady" ~ 3,
                        . == "Up" ~ 4,
                        TRUE ~ NA_real_))

# make max_glu_serum and A1Cresult ordinal
data <- data %>%
  mutate(max_glu_serum = case_when(is.na(max_glu_serum) ~ NA_real_,
                                   max_glu_serum == "Norm" ~ 1,
                                   max_glu_serum == ">200" ~ 2,
                                   max_glu_serum == ">300" ~ 3,
                                   TRUE ~ 4),
         A1Cresult = case_when(is.na(A1Cresult) ~ NA_real_,
                               A1Cresult == "Norm" ~ 1,
                               A1Cresult == ">7" ~ 2,
                               A1Cresult == ">8" ~ 3,
                                   TRUE ~ 4))

# drop 2 variables not used in the original paper either
#  payer code is not so important, admission source is included and better than adm type id
data <- data %>%
  dplyr::select(-c(payer_code, admission_type_id))

# collapse discharge_disposition_id as in the paper
data <- data %>%
  mutate(discharge_disposition_id = case_when(is.na(discharge_disposition_id) ~ NA_character_,
                                              discharge_disposition_id %in% c("1", "6", "8") ~ "Home",
                                              TRUE ~ "Other"))

# collapse admission_source_id as in the paper
data <- data %>%
  mutate(admission_source_id = case_when(is.na(admission_source_id) ~ NA_character_,
                                         admission_source_id == "7" ~ "Emergency",
                                         admission_source_id %in% c("1", "2", "23") ~ "Referral",
                                              TRUE ~ "Other"))

# medical specialty
data <- data %>%
  mutate(medical_specialty = case_when(medical_specialty %in% c("InternalMedicine") ~ "InternalMedicine",
                                       medical_specialty %in% c("Cardiology", "Cardiology-Pediatric") ~ "Cardiology",
                                       medical_specialty %in% c("Surgeon") ~ "Surgery",
                                       str_starts(medical_specialty, "Surgery") ~ "Surgery",
                                       medical_specialty %in% c("Family/GeneralPractice") ~ "InternalMedicine",
                                       is.na(medical_specialty) ~ NA_character_,
                                       TRUE ~ "Other"))

# make categorical variables factor
data <- data %>%
  mutate_at(vars(race, gender, discharge_disposition_id, admission_source_id,
                 medical_specialty, change, diabetesMed, readmitted, diag_categ), as.factor)

(data_miss <- map(data, ~sum(is.na(.))) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "number_missing") %>%
  mutate(percentage_missing = number_missing/dim(data)[[1]]) %>%
  arrange(desc(number_missing)) %>%
  filter(number_missing > 0))

# remove observations with missing values for:
# feature                  number_missing percentage_missing
# diag_categ                           20          0.000201
# gender                                3          0.0000302

data <- data %>%
  filter(!is.na(diag_categ),
         !is.na(gender))

# save processed data to be used in MV script
save(data, file = "data/diabetic_data_processed.rda")

# exclude columns with missing values
data_miss <- map(data, ~sum(is.na(.))) %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "number_missing") %>%
  mutate(percentage_missing = number_missing/dim(data)[[1]]) %>%
  arrange(desc(number_missing)) %>%
  filter(number_missing > 0)

cols_missing <- data_miss$feature[data_miss$percentage_missing > 0]

data <- data %>%
  dplyr::select(-all_of(cols_missing))

data <- as.data.frame(data)

# dataset specific config
# -----------------------

outcome_col <- "readmitted"
dataset_name <- "diabetes"
positive_class <- "YES"

simulate_MV <- TRUE

# part 1 - run iter 1 to 50
N_iter <- 25

# run
# ---

predictor_matrix <- NULL

print(sprintf("Dataset %s", dataset_name))

all_results <- run_simulations(data, outcome_col, N_iter, dataset_name, p_miss,
                               simulate_MV = simulate_MV, positive_class = positive_class,
                               predictor_matrix = predictor_matrix,
                               save_dataset = save_dataset,
                               run_pred_model = run_pred_model)

save(all_results, file = sprintf("results/results_%s.RData", dataset_name))

