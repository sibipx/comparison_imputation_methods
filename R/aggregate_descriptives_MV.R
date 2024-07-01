

aggregate_descriptives <- function(results_dir){

  descriptives_MV <- data.frame()

  filenames <- list.files(results_dir, pattern="descriptives_MV_*", full.names=TRUE)
  filenames <- filenames[!str_detect(filenames, "all_datasets")]

  for (f in filenames){
    load(f)

    descriptives_MV <- descriptives_MV %>%
      rbind(data_miss)
  }

  return(descriptives_MV)
}
