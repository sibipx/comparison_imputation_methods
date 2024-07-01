
# function to generate one dataset

# dist: either "normal" or "binary50" (50 refers to prevalence)
# coefs: the vector of true coefficients (use the one that matches the rest of the specifications)
# npred: number of predictors, 4 or 16
# corx: correlation between predictors, 0 or 0.4

generate_dataset <- function(dist, coefs, npred, corx, ntrain){

  sig16 <- matrix(rep(corx,npred^2), npred,npred)
  diag(sig16) <- 1

  # make 0 correlation between variables if more than 4 predictors
  if (npred > 4){
    sig16[,5:ncol(sig16)] <- 0
    sig16[5:ncol(sig16),] <- 0
    diag(sig16) <- 1
  }

  # generate dataset with ntrain observations
  data <- as.data.frame(mvrnorm(n=ntrain, mu = c(rep(0,npred)), Sigma = sig16))
  if (dist=="binary50"){
    data = as.data.frame(1*(data>=0))
  }

  if (npred == 4) {
    data$pr <- 1/(1+exp(-1*(coefs[2]*data$V1 + coefs[3]*data$V2 +
                              coefs[4]*data$V3 + coefs[5]*data$V4 + coefs[1])))
  }

  if (npred == 16) {
    data$pr = 1/(1+exp(-1*(coefs[2]*data$V1 + coefs[3]*data$V2 + coefs[4]*data$V3 + coefs[5]*data$V4 +
                             coefs[6]*data$V5 + coefs[7]*data$V6 + coefs[8]*data$V7 + coefs[9]*data$V8 +
                             coefs[10]*data$V9 + coefs[11]*data$V10 + coefs[12]*data$V11 + coefs[13]*data$V12 +
                             coefs[14]*data$V13 + coefs[15]*data$V14 + coefs[16]*data$V15 + coefs[17]*data$V16 + coefs[1])))
  }

  data$y_outcome <- rbinom(dim(data)[1],1,data$pr)
  data <- data[order(data$y_outcome, decreasing = FALSE), ]

  data$y_outcome <- ifelse(data$y_outcome == 1, "YES", "NO")

  data$pr <- NULL

  return(data)
}

# generates multiple datasets according to fixed scenarios defined in the function
generate_datasets <- function(){

  scenarios <- tibble(Name = character(),
                      Predictors = double(),
                      Distribution = character(),
                      Correlation = double(),
                      AUC = double(),
                      Strenght = character(),
                      Noise = character()
  )

  scenarios <- scenarios %>%
    # balanced, no noise
    # add_row(Name = "sim_75_1", Distribution = "Continous", Predictors = 4,
    #         Correlation = 0.1, AUC = 0.75,
    #         Strenght = "Balanced", Noise = "No") %>%
    # add_row(Name = "sim_75_7", Distribution = "Continous", Predictors = 4,
    #         Correlation = 0.7, AUC = 0.75,
    #         Strenght = "Balanced", Noise = "No") %>%
    # add_row(Name = "sim_90_1", Distribution = "Continous", Predictors = 4,
    #         Correlation = 0.1, AUC = 0.9,
    #         Strenght = "Balanced", Noise = "No") %>%
    # add_row(Name = "sim_90_7", Distribution = "Continous", Predictors = 4,
    #         Correlation = 0.7, AUC = 0.9,
    #         Strenght = "Balanced", Noise = "No") %>%
    # balanced noise - generate only datasets with noise and take first 4 variable for no-noise
    add_row(Name = "sim_75_1_noise", Distribution = "Continous", Predictors = 4,
            Correlation = 0.1, AUC = 0.75,
            Strenght = "Balanced", Noise = "Yes") %>%
    add_row(Name = "sim_75_7_noise", Distribution = "Continous", Predictors = 4,
            Correlation = 0.7, AUC = 0.75,
            Strenght = "Balanced", Noise = "Yes") %>%
    add_row(Name = "sim_90_1_noise", Distribution = "Continous", Predictors = 4,
            Correlation = 0.1, AUC = 0.9,
            Strenght = "Balanced", Noise = "Yes") %>%
    add_row(Name = "sim_90_7_noise", Distribution = "Continous", Predictors = 4,
            Correlation = 0.7, AUC = 0.9,
            Strenght = "Balanced", Noise = "Yes")

  # Coefficients per scenario (manually tweaked)
  sim_75_1 = c(-1.61,  rep(0.47, 4))
  sim_75_1_noise = c(-1.61,  rep(0.47, 4), rep(0,12))

  sim_75_7 = c(-1.59, rep(0.3, 4))
  sim_75_7_noise = c(-1.59, rep(0.3, 4), rep(0,12))

  sim_90_1 = c(-2.6,  rep(1.05, 4))
  sim_90_1_noise = c(-2.6,  rep(1.05, 4), rep(0,12))

  sim_90_7 = c(-2.45, rep(0.66, 4))
  sim_90_7_noise = c(-2.45, rep(0.66, 4), rep(0,12))

  sim_datasets <- list()

  for (row in 1:dim(scenarios)[[1]]) {

    strategy <- scenarios[row,]
    dist <- ifelse(strategy$Distribution == 'Binary','binary50','normal')
    corx <- as.numeric(strategy$Correlation)

    ntrain <- 4000

    dataset <- generate_dataset(dist=dist,
                                coefs=get(strategy$Name),
                                npred=length(get(strategy$Name))-1,
                                corx=corx,
                                ntrain=ntrain)

    sim_datasets[[strategy$Name]] <- dataset
  }
  return(sim_datasets)
}
