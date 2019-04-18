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
  # THIS HAS BEEN UPDATED TO SAMPLE 20% OF X_TRAIN ROWS AS INDECES 
  val_indices <- sample(1:nrow(x_train), .15*nrow(df))
  
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
use_me <- optimizer_adam(lr = 0.008, beta_1 = 0.9, beta_2 = 0.999,
                         epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                         clipvalue = NULL)

dat <- read_csv("Data/imputed_CE.csv")

# Assuming data is in the correct shape, here's how the real testing goes down
it <- 20

statistic_tracker <- data.frame(true_mean = numeric(it), 
                                oracle_mean = numeric(it),
                                pi_naive_mean = numeric(it),
                                median_imp_mean = numeric(it),
                                lin_imp_mean = numeric(it),
                                lin_oracle = numeric(it),
                                nn_imp_mean = numeric(it),
                                nn_oracle = numeric(it),
                                nn_pi_imp_mean = numeric(it),
                                nn_pi_oracle = numeric(it),
                                nn_resamp_imp_mean = numeric(it),
                                nn_resamp_oracle = numeric(it),
                                nn_wmse_imp_mean = numeric(it),
                                nn_wmse_oracle = numeric(it),
                                nn_deriv_imp_mean = numeric(it),
                                nn_deriv_oracle = numeric(it))

# Prep label
df <- select(dat, c(FINCBTAX, pi, AGE_REF, BATHRMQ, BEDROOMQ, EDUC_REF, FAM_SIZE, 
                    FSALARYX))

#df <- df[1:1000,]

#label <- "FINCBTAX"
#label_index <- which(colnames(df) == label)

mu_y <- mean(df$FINCBTAX)
mu_y # == mean american household income :)

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
  
  # Make oracle df without noisy parameters
  odf <- select(df, pi) # put good insight params here
  o_dropped_obs <- odf[indices,]
  o_reduced_df <- odf[-indices,]
  
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
  
  # Intentionally over-train on the training data to find the validation minimum
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = use_me,
    loss = "mse",
    metrics = c("mae")
  )
  
  epo <- 300
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = epo, 
    verbose = 0,
    #batch_size = 8,  
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
    optimizer = use_me,
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
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                          + sum(nn_y_hat / dropped_obs$pi))
  
  print("Finished nn imp mean")
  #########
  # Mean according to imputed data via neural network w pi feature
  y_train <- reduced_df$FINCBTAX
  reduced_df_nolab <- select(reduced_df, -c(FINCBTAX))
  #reduced_df_nolab$pi <- 1 / reduced_df_nolab$pi
  
  y_test <- dropped_obs$FINCBTAX
  dropped_obs_nolab <- select(dropped_obs, -c(FINCBTAX))
  #dropped_obs_nolab$pi <- 1 / dropped_obs_nolab$pi
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  # intentionally over-train the model
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  
  model %>% compile(
    optimizer = use_me,
    loss = "mse",
    metrics = c("mae")
  )
  
  epo <- 300
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = epo,
    verbose = 0,
    #batch_size = 32,
    validation_data = list(x_val, y_val)
  )
  
  goodtrain <- which.min(history$metrics$val_loss)
  goodtrain
  print(goodtrain)
  print("of")
  print(epo)
  
  # re-train the new model to the val-min
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = use_me,
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
  
  x_test <- as.matrix(x_test)
  nn_pi_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_pi_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                             + sum(nn_pi_y_hat / dropped_obs$pi))
  
  print("Finished nn pi imp mean")
  
  
  ## Mean according to dataset imputed via neural network with custom weighted MSE loss
  # CAUTION: had to do some tricks to extract obs_weights for training (and not validition). uses column 4 for pi, 
  # which might change if features change
  
  # need to pull weights off of training data: this is getting split into validation.
  
  y_train <- reduced_df$FINCBTAX
  reduced_df_nolab <- select(reduced_df, -c(FINCBTAX))  
  
  y_test <- dropped_obs$FINCBTAX
  dropped_obs_nolab <- select(dropped_obs, -c(FINCBTAX))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  create_validation_split(x_train, y_train)
  
  # SHOULD EXTRACT SCALARS BEFORE NORMALIZE!
  obs_weights <- 1 / partial_x_train[,1] 
  partial_x_train <- partial_x_train[,-1]
  
  normalize_wmse_data <- function(train_data, val_data, test_data) {
    mean <- apply(train_data, 2, mean)
    std <- apply(train_data, 2, sd)
    
    x_train <<- scale(train_data, center = mean, scale = std)
    x_val <<- scale(val_data, center = mean, scale = std)
    x_test <<- scale(test_data, center = mean, scale = std)
  }
  
  normalize_wmse_data(x_train, x_val, x_test)

  x_val <- x_val[,-1]
  x_test <- as.matrix(x_test)
  x_test <- x_test[,-1]
  
  x_train <- x_train[,-1]
  
  # intentionally overtrain
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(partial_x_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = use_me,
    loss = "mse",
    metrics = c("mae")
  )
  
  epo <- 50
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    sample_weight = obs_weights,
    epochs = epo,
    verbose = 0,
    #batch_size = 64,
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
    optimizer = use_me,
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
  
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_wmse_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                               + sum(nn_y_hat / dropped_obs$pi))
  
  print("Finished nn wmse mean")
  
  
  ##########
  # Mean according to imputed dataset via neural network with weighted resample
  # on the full sample. the missing values in the resample are then imputed and 
  # the imputation mean is taken on the new data set
  # (without pi feature)
  dropped_obs_NA_lab <- dropped_obs
  dropped_obs_NA_lab$FINCBTAX <- NA
  
  orig_df <- rbind(reduced_df, dropped_obs_NA_lab)
  
  # re-sample by inclusion probability 
  weight_vec <- 1 / as.numeric(orig_df$pi)  
  
  orig_tbl <- as_tibble(orig_df)
  
  # RESAMPLE ON DF NOT REDUCED_DF
  resamp_df <- sample_n(tbl = orig_tbl, size = nrow(orig_tbl), replace = TRUE, weight = weight_vec)
  
  # re-partition into complete cases, and cases to be imputed
  resamp_reduced_df <- resamp_df[-which(is.na(resamp_df$FINCBTAX)),]
  resamp_dropped_obs <- resamp_df[which(is.na(resamp_df$FINCBTAX)),]
  
  y_train <- resamp_reduced_df$FINCBTAX
  resamp_reduced_df_nolab <- select(resamp_reduced_df, -c(FINCBTAX))
  
  y_test <- resamp_dropped_obs$FINCBTAX
  resamp_dropped_obs_nolab <- select(resamp_dropped_obs, -c(FINCBTAX))
  
  resamp_reduced_df_nolab <- as.matrix(resamp_reduced_df_nolab)
  resamp_dropped_obs_nolab <- as.matrix(resamp_dropped_obs_nolab)
  
  x_train <- resamp_reduced_df_nolab
  x_test <- resamp_dropped_obs_nolab
  
  normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = use_me,
    loss = "mse",
    metrics = c("mae")
  )
  
  epo <- 300
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
  
  # retrain 
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = use_me,
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
  
  x_test <- as.matrix(x_test)
  nn_resamp_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_resamp_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                                 + sum(nn_resamp_y_hat / resamp_dropped_obs$pi))
  
  print("Finished resamp imp mean")
  
  
  ########
  # Derived-parameter NN imputation, where derived parameters are ??
  
  y_train <- reduced_df$FINCBTAX
  reduced_df_nolab <- select(reduced_df, -c(FINCBTAX))
  
  y_test <- dropped_obs$FINCBTAX
  dropped_obs_nolab <- select(dropped_obs, -c(FINCBTAX))
  
  x1pi <- (reduced_df_nolab$FSALARYX)*(reduced_df_nolab$pi)
  x2pi <- (reduced_df_nolab$AGE_REF)*(reduced_df_nolab$pi)
  
  dx1pi <- (dropped_obs_nolab$FSALARYX)*(dropped_obs_nolab$pi)
  dx2pi <- (dropped_obs_nolab$AGE_REF)*(dropped_obs_nolab$pi)
  
  reduced_df_nolab <- cbind(reduced_df_nolab, x1pi, x2pi)
  dropped_obs_nolab <- cbind(dropped_obs_nolab, dx1pi, dx2pi)
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = use_me,
    loss = "mse",
    metrics = c("mae"))
  
  epo <- 300
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = epo,
    verbose = 0,
    #batch_size = 32, 
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
    optimizer = use_me,
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
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_deriv_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$FINCBTAX / reduced_df$pi) 
                                                                + sum(nn_y_hat / dropped_obs$pi))
  print("Finished derived param imp mean")
  
  print(i)
}

write.csv(statistic_tracker, file = "C:\\Users\\Alexander\\Documents\\thesis stat tracker\\force_o_1.csv")


################################################
# Compare results
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

# Impute the truly missing labels for fun?