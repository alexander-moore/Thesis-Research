## investigation

## Real Experiments in CE Data
# Documentation:
# https://www.bls.gov/cex/2017/csxintvwdata.pdf

library(readr)
library(dplyr)
library(keras)
library(kableExtra)
library(knitr)
library(ggplot2)
library(tensorflow)
library(data.table)

# Relevant functions
count_nas <- function(df) {
  isna <- apply(df, 2, is.na)
  tots <- apply(isna, 2, sum)
  tots
}

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

create_validation_split <- function(x_train, y_train) {
  # Validation Set
  val_indices <- sample(1:nrow(x_train), .2*nrow(df))
  
  x_val <<- x_train[val_indices,]
  partial_x_train <<- x_train[-val_indices,]
  
  y_val <<- y_train[val_indices]
  partial_y_train <<- y_train[-val_indices]
}

normalize_data <- function(train_data, test_data) {
  mean <- apply(train_data, 2, mean)
  std <- apply(train_data, 2, sd)
  
  x_train <<- scale(train_data, center = mean, scale = std)
  x_test <<- scale(test_data, center = mean, scale = std)
}

# custom `adam` optimizer, learning rate up for grabs
# .001 default, .005 testing (methods were using 600 epo)
adam_lr <- optimizer_adam(lr = 0.1, beta_1 = 0.9, beta_2 = 0.999,
                          epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                          clipvalue = NULL)

dat <- read_csv("Data/imputed_CE.csv")

# Assuming data is in the correct shape, here's how the real testing goes down
it <- 50

statistic_tracker <- data.frame(true_mean = numeric(it), 
                                oracle_mean = numeric(it),
                                pi_naive_mean = numeric(it),
                                median_imp_mean = numeric(it),
                                lin_imp_mean = numeric(it),
                                nn_imp_mean = numeric(it),
                                nn_pi_imp_mean = numeric(it),
                                nn_wmse_imp_mean = numeric(it))

# Prep label
df <- select(dat, c(FINCBTAX, pi, AGE_REF, BATHRMQ, BEDROOMQ, EDUC_REF, FAM_SIZE,
                    TOTEXPCQ))

#df <- df[1:1000,]

#label <- "FINCBTAX"
#label_index <- which(colnames(df) == label)

mu_y <- mean(df$FINCBTAX)
mu_y # == mean american household income :)

testing <- TRUE
#epo <- 200

for (i in 1:it) {
  
  # Split the CE data into training and testing. Might want some kind of scheme to 
  # get the right amount of high-pi and low-pi observations in each
  # (testing is unlabelled, but we know the right answer)
  
  #########
  # True mean
  statistic_tracker$true_mean[i] <- mu_y
  
  #########
  # Oracle mean
  # this is a standard estimator which is done on the full (nonmissing) data to compare
  hat_N_sample <- sum(1 / df$pi)
  statistic_tracker$oracle_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * df$FINCBTAX)
  
  #########
  # Drop some labels - weighted to high y
  zeros <- rep(0, nrow(df))
  rec <- pmax(zeros, df$FINCBTAX)
  
  indices <- sample(1:nrow(df), .20*nrow(df), prob = rec)
  
  # Make df with all features (noisy)
  dropped_obs <- df[indices,]
  reduced_df <- df[-indices,] 
  
  #########
  # pi-weighted naive mean
  # non-imputation of estimate which accounts for complex design but ignores systematic
  hat_N_respondent <- sum(1 / (reduced_df$pi))
  statistic_tracker$pi_naive_mean[i] <- (1 / hat_N_respondent)*sum((1 / reduced_df$pi) * 
                                                                     reduced_df$FINCBTAX)
  
  # Compute median imputation mean estimate
  
  #########
  # Median imputation: fill missing values with median
  len <- dim(dropped_obs)[1]
  median_list <- rep(median(reduced_df$FINCBTAX), len)
  labels <- as.vector(reduced_df$FINCBTAX)
  median_list <- as.vector(median_list)
  
  imputed_list <- c(labels,median_list)
  
  statistic_tracker$median_imp_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * 
                                                                     imputed_list)
  
  #########
  # Linear regression imputation: MSE 1/pi weighted
  red_weight <- 1 / reduced_df$pi
  lin_dat <- select(reduced_df, -c(pi))
  lin_dropped <- select(dropped_obs, -c(pi))
  
  linear_model <- lm(lin_dat$FINCBTAX ~ ., data = lin_dat, weights = red_weight)
  
  lm_y_hat <- predict(linear_model, lin_dropped) # THROWS WARN
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$lin_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                           + sum(lm_y_hat / dropped_obs$pi))
  
  #########
  # Mean accoridng to imputed data via naive neural network (ignore complex design)
  y_train <- reduced_df$FINCBTAX
  reduced_df_nolab <- select(reduced_df, -c(pi, FINCBTAX))
  
  y_test <- dropped_obs$FINCBTAX
  dropped_obs_nolab <- select(dropped_obs, -c(pi, FINCBTAX))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  if(testing){
    # Intentionally over-train on the training data to find the validation minimum
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu", 
                  input_shape = dim(x_train)[[2]]) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = adam_lr,
      loss = "mse",
      metrics = c("mae")
    )
    
    epo <- 150
    history <- model %>% fit(
      partial_x_train,
      partial_y_train,
      epochs = epo, 
      verbose = 0,
      validation_data = list(x_val, y_val)
    )
    
    goodtrain <- which.min(history$metrics$val_loss)
    print(goodtrain)
    print("of")
    print(epo)
    
    # Train a new model for the validation-minimizing number of epochs
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu", 
                  input_shape = dim(x_train)[[2]]) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = adam_lr,
      loss = "mse",
      metrics = c("mae")
    )
    
    history <- model %>% fit(
      x_train,
      y_train,
      epochs = goodtrain, 
      verbose = 0
      #batch_size = 32,
      #validation_data = list(x_val, y_val)
    )
    plot(history)
  } else {
    
    # Train a new model for the validation-minimizing number of epochs
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu", 
                  input_shape = dim(x_train)[[2]]) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = adam_lr,
      loss = "mse",
      metrics = c("mae")
    )
    
    history <- model %>% fit(
      x_train,
      y_train,
      epochs = 419, 
      verbose = 0
      #callbacks = EarlyStopping()
      #batch_size = 32,
      #validation_data = list(x_val, y_val)
    )
  }
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                          + sum(nn_y_hat / dropped_obs$pi))
  
  print("Finished nn imp mean")
  
  
  
  
  # WSME NN 
  #######
  # New Split
  # Inherit train and test
  train <- reduced_df
  test <- dropped_obs
  
  # record training data pi
  pi_vec <- train$pi
  
  # Inherit train and test
  train <- reduced_df
  y_train <- train$FINCBTAX
  x_train <- select(train, -c(FINCBTAX))
  
  test <- dropped_obs
  y_test <- test$FINCBTAX
  x_test <- select(test, -c(FINCBTAX))
  
  # Split train into train' and val
  val_indices <- sample(1:nrow(train), .20*nrow(df))
  
  x_val <- x_train[val_indices,]
  partial_x_train <- x_train[-val_indices,]
  
  y_val <- y_train[val_indices]
  partial_y_train <- y_train[-val_indices]
  
  
  # Extract partial pi from full train x
  #range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  pi_prime <- pi_vec[-val_indices]
  obs_weights <- 1 / pi_prime
  
  full_obs_weights <- 1 / pi_vec
  
  # remove pi from x_test and x_train, partial_x_train, x_val
  x_test <- select(x_test, -c(pi))
  x_train <- select(x_train, -c(pi))
  x_val <- select(x_val, -c(pi))
  partial_x_train <- select(partial_x_train, -c(pi))
  
  # Normalize all WRT train
  mean <- apply(x_train, 2, mean)
  std <- apply(x_train, 2, sd)
  
  x_train <- scale(x_train, center = mean, scale = std)
  x_test <- scale(x_test, center = mean, scale = std)
  x_val <- scale(x_val, center = mean, scale = std)
  partial_x_train <- scale(partial_x_train, center = mean, scale = std)
  
  #######
  
  if(testing){
    # intentionally overtrain
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu", 
                  input_shape = dim(partial_x_train)[[2]]) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = adam_lr,
      loss = "mse",
      metrics = c("mae")
    )
    
    epo <- 100
    history <- model %>% fit(
      partial_x_train,
      partial_y_train,
      sample_weight = obs_weights,
      epochs = epo,
      verbose = 0,
      validation_data = list(x_val, y_val)
    )
    
    goodtrain <- which.min(history$metrics$val_loss)
    print(goodtrain)
    print("of")
    print(epo)
    
    # retrain 
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu", 
                  input_shape = dim(x_train)[[2]]) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = adam_lr,
      loss = "mse",
      metrics = c("mae")
    )
    
    history <- model %>% fit(
      x_train,
      y_train,
      epochs = goodtrain, 
      verbose = 0,
      sample_weight = full_obs_weights
    )
    plot(history)
    
  } else {
    
    # retrain 
    model <- keras_model_sequential() %>%
      layer_dense(units = 64, activation = "relu", 
                  input_shape = dim(x_train)[[2]]) %>%
      layer_dense(units = 64, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      optimizer = adam_lr,
      loss = "mse",
      metrics = c("mae")
    )
    
    history <- model %>% fit(
      x_train,
      y_train,
      epochs = 209, 
      verbose = 0,
      sample_weight = full_obs_weights
    )
  }
  
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_wmse_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                               + sum(nn_y_hat / dropped_obs$pi))
  
  print("Finished nn wmse mean")
  
  
  
  print(i)
}

write.csv(statistic_tracker, file = "C:\\Users\\Alexander\\Documents\\thesis stat tracker\\LRepo_8.csv")


################################################
# Compare results
# oracle only






dat <- statistic_tracker

mse_table <- dat[1,]
for (i in 1:dim(dat)[2]) {
  
  matdat <- as.matrix(dat)
  
  mse_table[i] <- mean( (matdat[,i] - matdat[,1])^2 )
}

mse_table

# then do MSE of method divided by MSE of oracle to get measure of goodness
# (closer to 1 means closer to oracle means good)

oracle_ratio_table <- dat[1,]

for (i in 1:dim(dat)[2]) {
  
  oracle_ratio_table[i] <- mse_table[i] / mse_table[2]
}

oracle_ratio_table


# compute bias:
# mean of statistic vector - true mean
# divide this differnce by true mean
# multiple by 100 to get "percent relative bais", a normalized bias measure

prb_table <- dat[1,]
mu_y <- mean(dat[,1])

for (i in 1:dim(dat)[2]) {
  
  prb_table[i] <- 100 * ((mean(dat[,i]) - mu_y) / mu_y)
}

prb_table

mse_t <- transpose(mse_table)
oracle_t <- transpose(oracle_ratio_table)
prb_t <- transpose(prb_table)

binded <- cbind(mse_t, oracle_t, prb_t)
colnames(binded) <- c("MSE", "Oracle Ratio", "Relative Bias")
rownames(binded) <- colnames(statistic_tracker)

test <- binded
test <- signif(test, digits = 3)

test %>%
  kable() %>%
  kable_styling()

if(FALSE){
  # mean and SD ratios
  df <- read.csv("Data/AWS_running_short.csv")
  df <- df[,-1]
  
  means <- apply(df, FUN = mean, MARGIN = 2)
  means
  
  sds <- apply(df, FUN = sd, MARGIN = 2)
  sds
  
  mean_ratio <- means / means[2]
  mean_ratio
  
  sd_ratio <- sds / sds[2]
  sd_ratio
  
  # fULL
  df <- read.csv("Data/AWS_running_full.csv")
  df <- df[,-1]
  
  means <- apply(df, FUN = mean, MARGIN = 2)
  means
  
  sds <- apply(df, FUN = sd, MARGIN = 2)
  sds
  
  mean_ratio <- means / means[2]
  mean_ratio
  
  sd_ratio <- sds / sds[2]
  sd_ratio
  
}

