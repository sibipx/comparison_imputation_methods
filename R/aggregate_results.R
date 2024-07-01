
aggregrate_results <- function(results_dir){

  results_all_datasets <- data.frame()

  filenames <- list.files(results_dir, pattern="^results_.*\\.RData$", full.names=TRUE)
  filenames <- filenames[!str_detect(filenames, "all_datasets")]

  for (f in filenames){
    load(f)
    results_all_datasets <- results_all_datasets %>%
      rbind(all_results)

    rm(all_results)
  }

  return(results_all_datasets)

}

