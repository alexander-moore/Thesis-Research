# Compare distributions of mean via different imputation methods on artifical data

library(keras)
library(dplyr)
library(ggplot2)

N = 10^6
n = 10^4
it <- 100

p_y_ep_control <- 5

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

create_split <- function(df, y) {
  smp_size <- floor(.75*nrow(df))
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  x_train <<- df[train_ind, ]
  x_test <<- df[-train_ind, ]
  
  y_train <<- y[train_ind]
  y_test <<- y[-train_ind]
}

create_validation_split <- function(x_train, y_train) {
  # Validation Set
  val_indices <- 1:1000
  
  x_val <<- x_train[val_indices,]
  partial_x_train <<- x_train[-val_indices,]
  
  y_val <<- y_train[val_indices]
  partial_y_train <<- y_train[-val_indices]
}

p_1 <- rnorm(N, mean = 11, sd = 4)
p_2 <- rnorm(N, mean = 8, sd = 4)
#p_3 <- rnorm(N, mean = 5, sd = 1)
p_3 <- rexp(N, rate = .03)

p_y_ep <- rnorm(N, mean = 0, sd = p_y_ep_control)
p_y <- p_1*p_2 + p_3 + p_y_ep
p_y <- p_y + abs(min(p_y)) + .1

p_pi_ep <- rnorm(N, mean = 0, sd = 2)
temp_pi <- sqrt(p_y) + p_pi_ep
temp_pi <- rescale(temp_pi)
p_pi <- temp_pi * (n / sum(temp_pi))

p_df <- cbind(p_1, p_2, p_3, p_y, p_pi)
p_tbl <- as_tibble(p_df)

# Verify sum of pi over population approx n
sum(p_tbl$p_pi) #= 10000 = n

statistic_tracker <- data.frame(true_mean = numeric(it), 
                                oracle_mean = numeric(it),
                                naive_mean  = numeric(it),
                                pi_naive_mean = numeric(it),
                                median_imp_mean = numeric(it),
                                lin_imp_mean = numeric(it),
                                nn_imp_mean = numeric(it),
                                nn_pi_imp_mean = numeric(it),
                                nn_resamp_imp_mean = numeric(it),
                                nn_wmse_imp_mean = numeric(it))
# Want to estimate:
mu_y <- (1 / N) * sum(p_tbl$p_y)

# monte carlo simulation of draws from population with comparison methods
for (i in 1:it) {
  statistic_tracker$true_mean[i] <- mu_y
  
  sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, 
                                      weight = p_pi)
  
  df <- sample_population_by_pi %>% rename(x_1 = p_1, #Since we are not dealing with p_ anymore
                                           x_2 = p_2,
                                           x_3 = p_3,
                                           pi = p_pi,
                                           y = p_y)
  # verify sum of 1/pi over sample df = N
  #sum(1/df$pi) #= 997824.5, approx 10^6
  
  # sample mean
  #mean(df$y) #= 26.258
  
  # Oracle mean, \bar y ^o
  # this is a standard estimator which is done on the full (nonmissing) data to compare
  # N changed to sum(1/df$pi) , called hat N
  hat_N_sample <- sum(1 / df$pi)
  statistic_tracker$oracle_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * df$y)
  
  # Drop some labels - weighted to high y
  
  indices <- sample(1:nrow(df), .175*nrow(df), prob = df$y) #weights are stdized to 1 by sample()
  dropped_obs <- df[indices,]
  reduced_df <- df[-indices,] # These have a large differnce: 296 for dropped and 276 for reduced df
  
  #mean of complete cases
  statistic_tracker$naive_mean[i] <- mean(reduced_df$y)
  
  # pi-corrected naive mean
  # should be unbiased but very large variance. should come forward when gen func change
  ## NOTE DIFFERENCE OF HAT N RESPOND AND HAT N SAMPLE
  hat_N_respondent <- sum(1 / (reduced_df$pi)) # largely underestimates the true N
  statistic_tracker$pi_naive_mean[i] <- (1 / hat_N_respondent)*sum((1 / reduced_df$pi) * 
                                                                     reduced_df$y)
  
  # Naive imputation: fill missing values with median
  len <- dim(dropped_obs)[1]
  median_list <- rep(median(reduced_df$y), len)
  labels <- as.vector(reduced_df$y)
  median_list <- as.vector(median_list)
  
  imputed_list <- c( matrix(c(labels,median_list), nrow=2, byrow=TRUE) ) 
  statistic_tracker$median_imp_mean[i] <- mean(imputed_list)
  
  # mean according to imputed data via linear regression
  
  # adding weight = 1/pi . doing a weighted MSE. this is better than naive Lin Regress
  linear_model <- lm(y ~ x_1 + x_2 + x_3, data = reduced_df, weights = reduced_df$pi)
  lm_y_hat <- predict(linear_model, dropped_obs)
  ## FIX HERE 
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$lin_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                           + sum(lm_y_hat / dropped_obs$pi))
  
  # Mean accoridng to imputed data via neural network (3 diff methods here, 
  # can start with naive NN for baseline)
  y_train <- reduced_df$y
  reduced_df_nolab <- select(reduced_df, -c(pi, y))
  
  y_test <- dropped_obs$y
  dropped_obs_nolab <- select(dropped_obs, -c(pi,y))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 8, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 300, #high variance; study further
    verbose = 0,
    batch_size = 512,  #512
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  mean(nn_y_hat)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                          + sum(nn_y_hat / dropped_obs$pi))
  
  # Mean according to imputed data via neural network w pi feature
  y_train <- reduced_df$y
  reduced_df_nolab <- select(reduced_df, -c(y))
  
  y_test <- dropped_obs$y
  dropped_obs_nolab <- select(dropped_obs, -c(y))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 8, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  #plot_model(model, to_file='model_plot.png', show_shapes=True, show_layer_names=True)
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 300,
    verbose = 0,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_pi_y_hat <- predict(model, x_test)
  
  mean(nn_pi_y_hat)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_pi_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                             + sum(nn_pi_y_hat / dropped_obs$pi))
  
  # Mean according to imputed dataset via neural network with weighted resample
  # on the full sample. the missing values in the resample are then imputed and 
  # the imputation mean is taken on the new data set
  # (without pi feature)
  ###################
  # in the future, instead of making another n-size df, what if we scaled back up to N
  # with the same method. so re-creating the population by sampling w replacement
  ###################
  
  dropped_obs_NA_lab <- dropped_obs
  dropped_obs_NA_lab$y <- NA
  
  orig_df <- rbind(reduced_df, dropped_obs_NA_lab)
  
  weight_vec <- 1 / as.numeric(orig_df$pi)
  
  orig_tbl <- as_tibble(orig_df)
  
  # RESAMPLE ON DF NOT REDUCED_DF
  resamp_df <- sample_n(tbl = orig_tbl, size = nrow(orig_tbl), replace = TRUE, weight = weight_vec)
  
  # re-partition into complete cases, and cases to be imputed
  resamp_reduced_df <- resamp_df[-which(is.na(resamp_df$y)),]
  resamp_dropped_obs <- resamp_df[which(is.na(resamp_df$y)),]
  
  y_train <- resamp_reduced_df$y
  resamp_reduced_df_nolab <- select(resamp_reduced_df, -c(y))
  
  y_test <- resamp_dropped_obs$y
  resamp_dropped_obs_nolab <- select(resamp_dropped_obs, -c(y))
  
  resamp_reduced_df_nolab <- as.matrix(resamp_reduced_df_nolab)
  resamp_dropped_obs_nolab <- as.matrix(resamp_dropped_obs_nolab)
  
  x_train <- resamp_reduced_df_nolab
  x_test <- resamp_dropped_obs_nolab
  
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 8, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  #plot_model(model, to_file='model_plot.png', show_shapes=True, show_layer_names=True)
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 300,
    verbose = 0,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_resamp_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_resamp_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                                 + sum(nn_resamp_y_hat / resamp_dropped_obs$pi))
  
  
  ## Mean according to dataset imputed via neural network with custom weighted MSE loss
  # CAUTION: had to do some tricks to extract obs_weights for training (and not validition). uses column 4 for pi, 
  # which might change if features change
  
  # need to pull weights off of training data: this is getting split into validation.
  obs_weights <- reduced_df$pi
  
  y_train <- reduced_df$y
  reduced_df_nolab <- select(reduced_df, -c(y))  # NEED TO DROP PI LATER. CANT DROP NOW SINCE NEED TO EXTRACT PI FROM TRAINING DATA
  
  y_test <- dropped_obs$y
  dropped_obs_nolab <- select(dropped_obs, -c(pi,y))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  create_validation_split(x_train, y_train)
  
  obs_weights <- partial_x_train[,4]  # this is non-generalizable and will be a problem if we change the number of features. 
  partial_x_train <- partial_x_train[,-4]
  x_train <- x_train[,-4]
  x_val <- x_val[,-4]
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 8, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    sample_weight = obs_weights,
    epochs = 300,
    verbose = 0,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_wmse_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                               + sum(nn_y_hat / dropped_obs$pi))
  
  
  print(i)
}

# Save the mean table so that we don't have to always re-run it
#write.csv(statistic_tracker, file = "C:\\Users\\Alexander\\Documents\\thesis stat tracker\\100_1.csv")

dat <- read.csv("c:/Users/Alexander/Documents/thesis stat tracker/176_full.csv")
dat <- dat[,-1]

#### COMPUTING MSE (and making table)
# MSE against TRUE mean for each method:
# take dif of each with true. square each. sum all. divide by iterations

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

# ask "kristen bott" about computation. might have answers to cloud computing or server computing
# andrew might have some ideas too


