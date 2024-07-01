# load config
source("config.R", chdir = TRUE)

set.seed(seed)
datasets <- generate_datasets()

for (d in names(datasets)){

  data <- datasets[[d]]
  data$y_outcome <- as.factor(data$y_outcome)

  filename <- sprintf("%s/data_%s.RData", data_path, d)
  save(data, file = filename)

  # create dataset with no noise by only keeping the nonoise variables (V1 to V4)
  data <- data[,c("V1", "V2", "V3", "V4", "y_outcome")]

  d_no_noise <- str_replace(d, "_noise", "")
  filename <- sprintf("%s/data_%s.RData", data_path, d_no_noise)
  save(data, file = filename)

}
