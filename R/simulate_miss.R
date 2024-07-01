#' Simulates MCAR missingness
#'
#' @param data_X dataframe without outcome
#' @param p_miss proportion missing
#'
#' @return data_X_miss, id_all_missing

simulate_miss_MCAR <- function(data_X, data_y = NULL, p_miss){

  data_X_miss <- missForestPredict::produce_NA(data_X, proportion = p_miss)

  # check if there are rows for which all variables are missing and log them
  id_all_missing <- rowSums(!is.na(data_X_miss)) == 0

  return(list(data_X_miss = data_X_miss,
              id_all_missing = id_all_missing))
}

#' Helper function to simulate missingness in var1 in function of values of var2
#'
#' @param data_X dataframe without outcome
#' @param var1 variable in which to simulate misingness
#' @param var2 variable in function of which to simulate misingness
#' @param p_miss_groups vector of size 2 of probabilities for missingness in 2 groups
#'
#' @return dataframe with missing values in columns suffixed with _mar

miss_var1_ifo_var2 <- function(data_X, var1, var2, p_miss_groups){

  var1_q <- ensym(var1)
  var2_q <- ensym(var2)

  lim_breaks <- c(min(data_X[, var2, drop = TRUE])-1,
                  mean(data_X[, var2, drop = TRUE]),
                  max(data_X[, var2, drop = TRUE])+1)

  data_X <- data_X %>%
    mutate(mar = !!var1_q,
           frac = as.numeric(as.character(cut(!!var2_q, breaks = lim_breaks,
                                              labels = p_miss_groups)))) %>%
    group_by(frac) %>%
    mutate(mar = replace(mar, row_number(mar) %in%
                           sample(1:n(), round(unique(frac)*n())), NA_real_)) %>%
    ungroup()

  data_X <- data_X %>%
    rename_with(~paste(var1, ., sep = "_"), all_of("mar"))

  return(data_X)
}

#' Helper function to simulate missingness in var1 in function of values of var2 and outcome
#'
#' @param data_X dataframe without outcome
#' @param var1 variable in which to simulate misingness
#' @param var2 variable in function of which to simulate misingness
#' @param outcome_col outcome column name
#'
#' @return dataframe with missing values in columns suffixed with _mar

miss_var1_ifo_var2_and_outcome <- function(data_X_Y, var1, var2, outcome_col){

  var1_q <- ensym(var1)
  var2_q <- ensym(var2)
  outcome_col_q <- ensym(outcome_col)

  lim_breaks <- c(min(data_X_Y[, var2, drop = TRUE])-1,
                  mean(data_X_Y[, var2, drop = TRUE]),
                  max(data_X_Y[, var2, drop = TRUE])+1)

  data_X_Y <- data_X_Y %>%
    mutate(mar = !!var1_q,
           frac = as.character(cut(!!var2_q, breaks = lim_breaks,
                                   labels = c("low", "high"))),
           p_miss = case_when(frac == "low" & !!outcome_col_q == "YES" ~ 0.1,
                              frac == "low" & !!outcome_col_q  == "NO" ~ 0.36,
                              frac == "high" & !!outcome_col_q  == "YES" ~ 0.2,
                              frac == "high" & !!outcome_col_q  == "NO" ~ 0.3,
                              TRUE ~ NA_real_)) %>%

    group_by(p_miss) %>%
    mutate(mar = replace(mar, row_number(mar) %in%
                           sample(1:n(), round(unique(p_miss)*n())), NA_real_)) %>%
    ungroup()

  data_X_Y <- data_X_Y %>%
    rename_with(~paste(var1, ., sep = "_"), all_of("mar"))

  return(data_X_Y)
}

#' Simulates MAR missingness on 2 variables in function of 2 other variables
#'
#' @param data_X dataframe without outcome
#' @param p_miss proportion missing
#'
#' @return data_X_miss, id_all_missing

simulate_miss_MAR_2 <- function(data_X, data_y = NULL, p_miss){

  # V1 ifo V2
  # p_missing for V1 = 0.1 in values of V2 lower than the mean of V2
  # p_missing for V1 = 0.5 in values of V2 greater than the mean of V2
  data_X <- miss_var1_ifo_var2(data_X, "V1", "V2", p_miss_groups = c(0.1, 0.5))

  # V3 ifo V4
  # p_missing for V3 = 0.1 in values of V4 lower than the mean of V4
  # p_missing for V3 = 0.5 in values of V4 greater than the mean of V4
  data_X <- miss_var1_ifo_var2(data_X, "V3", "V4", p_miss_groups = c(0.1, 0.5))

  data_X <- data_X %>%
    select(-c(V1, V3, frac)) %>%
    rename(V1 = V1_mar,
           V3 = V3_mar) %>%
    as.data.frame()

  # add MCAR missingness on noise variables (if they are part of the dataset)
  cols_noise <- colnames(data_X)[!colnames(data_X) %in% c("V1", "V2", "V3", "V4")]

  if (length(cols_noise) > 0){

    data_noise <- data_X[cols_noise]
    data_noise <- missForestPredict::produce_NA(data_noise, proportion = p_miss)
    data_X <- cbind(data_X[,c("V1", "V2", "V3", "V4")],
                    data_noise) %>%
      as.data.frame()
  }

  # check if there are rows for which all variables are missing and log them
  # (should always be none for this scenario)
  id_all_missing <- rowSums(!is.na(data_X)) == 0

  return(list(data_X_miss = data_X,
              id_all_missing = id_all_missing))
}

#' Simulates MAR missingness on each variable in function of the next variable
#'
#' @param data_X dataframe without outcome
#' @param data_y outcome vector
#' @param p_miss proportion missing
#'
#' @return data_X_miss, id_all_missing

simulate_miss_MAR_circ <- function(data_X, data_y = NULL, p_miss){

  data_X <- miss_var1_ifo_var2(data_X, "V1", "V2", p_miss_groups = c(0.1, 0.5))
  data_X <- miss_var1_ifo_var2(data_X, "V2", "V3", p_miss_groups = c(0.1, 0.5))
  data_X <- miss_var1_ifo_var2(data_X, "V3", "V4", p_miss_groups = c(0.1, 0.5))
  data_X <- miss_var1_ifo_var2(data_X, "V4", "V1", p_miss_groups = c(0.1, 0.5))

  data_X <- data_X %>%
    select(-c(V1, V2, V3, V4, frac)) %>%
    rename(V1 = V1_mar,
           V2 = V2_mar,
           V3 = V3_mar,
           V4 = V4_mar) %>%
    as.data.frame()

  # add MCAR missingness on noise variables (if they are part of the dataset)
  cols_noise <- colnames(data_X)[!colnames(data_X) %in% c("V1", "V2", "V3", "V4")]

  if (length(cols_noise) > 0){

    data_noise <- data_X[cols_noise]
    data_noise <- missForestPredict::produce_NA(data_noise, proportion = p_miss)
    data_X <- cbind(data_X[,c("V1", "V2", "V3", "V4")],
                    data_noise) %>%
      as.data.frame()
  }

  # check if there are rows for which all variables are missing and log them
  id_all_missing <- rowSums(!is.na(data_X)) == 0

  return(list(data_X_miss = data_X,
              id_all_missing = id_all_missing))
}

#' Simulates MAR missingness on 2 variables in function of 2 other variables and outcome
#'
#' @param data_X dataframe without outcome
#' @param data_y outcome vector
#' @param p_miss proportion missing
#'
#' @return data_X_miss, id_all_missing

simulate_miss_MAR_circ_out <- function(data_X, data_y, p_miss){

  data_X_Y <- cbind(data_X, data_y)

  data_X_Y <- miss_var1_ifo_var2_and_outcome(data_X_Y, "V1", "V2", "data_y") # V1 ifo V2
  data_X_Y <- miss_var1_ifo_var2_and_outcome(data_X_Y, "V2", "V3", "data_y") # V2 ifo V3
  data_X_Y <- miss_var1_ifo_var2_and_outcome(data_X_Y, "V3", "V4", "data_y") # V3 ifo V4
  data_X_Y <- miss_var1_ifo_var2_and_outcome(data_X_Y, "V4", "V1", "data_y") # V4 ifo V1


  data_X <- data_X_Y %>%
    select(-c(V1, V2, V3, V4, frac, data_y, p_miss)) %>%
    rename(V1 = V1_mar,
           V2 = V2_mar,
           V3 = V3_mar,
           V4 = V4_mar) %>%
    as.data.frame()

  # add MCAR missingness on noise variables (if they are part of the dataset)
  cols_noise <- colnames(data_X)[!colnames(data_X) %in% c("V1", "V2", "V3", "V4")]

  if (length(cols_noise) > 0){

    data_noise <- data_X[cols_noise]
    data_noise <- missForestPredict::produce_NA(data_noise, proportion = p_miss)
    data_X <- cbind(data_X[,c("V1", "V2", "V3", "V4")],
                    data_noise) %>%
      as.data.frame()
  }

  # check if there are rows for which all variables are missing and log them
  id_all_missing <- rowSums(!is.na(data_X)) == 0

  return(list(data_X_miss = data_X,
              id_all_missing = id_all_missing))
}


#' Simulates MAR missingness on 2 variables in function of 2 other variables and the outcome
#'
#' @param data_X training data without outcome
#' @param data_y outcome vector
#' @param p_miss proportion missing
#'
#' @return data_X_miss, id_all_missing

simulate_miss_MAR_2_out <- function(data_X, data_y, p_miss){

  data_X_Y <- cbind(data_X, data_y)

  # V1 ifo V2
  data_X_Y <- miss_var1_ifo_var2_and_outcome(data_X_Y, "V1", "V2", "data_y")

  # V3 ifo V4
  data_X_Y <- miss_var1_ifo_var2_and_outcome(data_X_Y, "V3", "V4", "data_y")

  data_X <- data_X_Y %>%
    select(-c(V1, V3, frac, p_miss, data_y)) %>%
    rename(V1 = V1_mar,
           V3 = V3_mar) %>%
    as.data.frame()

  # add MCAR missingness on noise variables (if they are part of the dataset)
  cols_noise <- colnames(data_X)[!colnames(data_X) %in% c("V1", "V2", "V3", "V4")]

  if (length(cols_noise) > 0){

    data_noise <- data_X[cols_noise]
    data_noise <- missForestPredict::produce_NA(data_noise, proportion = p_miss)
    data_X <- cbind(data_X[,c("V1", "V2", "V3", "V4")],
                    data_noise) %>%
      as.data.frame()
  }

  # check if there are rows for which all variables are missing and log them
  # (should always be none for this scenario)
  id_all_missing <- rowSums(!is.na(data_X)) == 0

  return(list(data_X_miss = data_X,
              id_all_missing = id_all_missing))
}


#' Simulates MNAR missingness on all variables with higher probability in the higher values of the variable
#'
#' @param data_X training data without outcome
#' @param data_y outcome vector
#' @param p_miss proportion missing
#'
#' @return data_X_miss, id_all_missing

simulate_miss_MNAR_hard <- function(data_X, data_y, p_miss){


  data_X <- miss_var1_ifo_var2(data_X, "V1", "V1", p_miss_groups = c(0.1, 0.5)) # V1 ifo V1
  data_X <- miss_var1_ifo_var2(data_X, "V2", "V2", p_miss_groups = c(0.1, 0.5)) # V2 ifo V2
  data_X <- miss_var1_ifo_var2(data_X, "V3", "V3", p_miss_groups = c(0.1, 0.5)) # V3 ifo V3
  data_X <- miss_var1_ifo_var2(data_X, "V4", "V4", p_miss_groups = c(0.1, 0.5)) # V4 ifo V4

  data_X <- data_X %>%
    select(-c(V1, V2, V3, V4, frac)) %>%
    rename(V1 = V1_mar,
           V2 = V2_mar,
           V3 = V3_mar,
           V4 = V4_mar) %>%
    as.data.frame()

  # add MCAR missingness on noise variables (if they are part of the dataset)
  cols_noise <- colnames(data_X)[!colnames(data_X) %in% c("V1", "V2", "V3", "V4")]

  if (length(cols_noise) > 0){

    data_noise <- data_X[cols_noise]
    data_noise <- missForestPredict::produce_NA(data_noise, proportion = p_miss)
    data_X <- cbind(data_X[,c("V1", "V2", "V3", "V4")],
                    data_noise) %>%
      as.data.frame()
  }

  # check if there are rows for which all variables are missing and log them
  # (should always be none for this scenario)
  id_all_missing <- rowSums(!is.na(data_X)) == 0

  return(list(data_X_miss = data_X,
              id_all_missing = id_all_missing))
}


#' Simulates MNAR missingness on all variables with higher probability in the higher values of the variable
#'
#' @param data_X training data without outcome
#' @param data_y outcome vector
#' @param p_miss proportion missing
#'
#' @return data_X_miss, id_all_missing

simulate_miss_MNAR_easy <- function(data_X, data_y, p_miss){


  data_X <- miss_var1_ifo_var2(data_X, "V1", "V1", p_miss_groups = c(0.1, 0.5)) # V1 ifo V1
  data_X <- miss_var1_ifo_var2(data_X, "V2", "V2", p_miss_groups = c(0.5, 0.1)) # V2 ifo V2
  data_X <- miss_var1_ifo_var2(data_X, "V3", "V3", p_miss_groups = c(0.1, 0.5)) # V3 ifo V3
  data_X <- miss_var1_ifo_var2(data_X, "V4", "V4", p_miss_groups = c(0.5, 0.1)) # V4 ifo V4

data_X <- data_X %>%
  select(-c(V1, V2, V3, V4, frac)) %>%
  rename(V1 = V1_mar,
         V2 = V2_mar,
         V3 = V3_mar,
         V4 = V4_mar) %>%
  as.data.frame()

# add MCAR missingness on noise variables (if they are part of the dataset)
cols_noise <- colnames(data_X)[!colnames(data_X) %in% c("V1", "V2", "V3", "V4")]

if (length(cols_noise) > 0){

  data_noise <- data_X[cols_noise]
  data_noise <- missForestPredict::produce_NA(data_noise, proportion = p_miss)
  data_X <- cbind(data_X[,c("V1", "V2", "V3", "V4")],
                  data_noise) %>%
    as.data.frame()
}

# check if there are rows for which all variables are missing and log them
# (should always be none for this scenario)
id_all_missing <- rowSums(!is.na(data_X)) == 0

return(list(data_X_miss = data_X,
            id_all_missing = id_all_missing))
}
