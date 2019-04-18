# model selection 2
## Adding noise features, new generative function, and 
# model selection oracle

# Compare distributions of mean via different imputation methods on artifical data

library(keras)
library(dplyr)
library(ggplot2)
library(tensorflow)

N = 10^5
n = 10^3
it <- 100

p_y_ep_control <- 1

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

p_1 <- runif(N, -30, 30)
p_2 <- runif(N, -30, 30)

nulls <- matrix(rnorm(20*N, 0, 8), ncol = 20)

p_y_ep <- rnorm(N, mean = 0, sd = p_y_ep_control)
#p_y <- p_1*p_2 + p_3 + p_y_ep

p_y <- p_1 + abs(p_2) + p_y_ep #added noise for reasonability
#mpy <- mean(p_y)
#p_y[p_y > mpy] <- p_y[p_y > mpy] + mpy

p_y[p_y > mean(p_y)] <- mean(p_y)

p_y <- p_y + p_y_ep
p_y <- abs(p_y)

p_pi_ep <- rnorm(N, mean = 0, sd = 1)
temp_pi <- sqrt(p_y) + p_pi_ep
temp_pi <- rescale(temp_pi)
p_pi <- temp_pi * (n / sum(temp_pi))

p_df <- cbind(p_1, p_2, p_y, p_pi, nulls)
p_tbl <- as_tibble(p_df)

# Verify sum of pi over population approx n
sum(p_tbl$p_pi) == n#= 10000 = n

statistic_tracker <- data.frame(true_mean = numeric(it), 
                                oracle_mean = numeric(it),
                                pi_naive_mean = numeric(it),
                                median_imp_mean = numeric(it),
                                lin_imp_mean = numeric(it),
                                lin_oracle = numeric(it),
                                nn_oracle = numeric(it),
                                nn_pi_oracle = numeric(it),
                                nn_resamp_oracle = numeric(it),
                                nn_wmse_oracle = numeric(it),
                                nn_deriv_oracle = numeric(it))
# Want to estimate:
mu_y <- (1 / N) * sum(p_tbl$p_y)
mu_y
# monte carlo simulation of draws from population with comparison methods
for (i in 1:it) {
  statistic_tracker$true_mean[i] <- mu_y
  
  sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, 
                                      weight = p_pi)
  
  df <- sample_population_by_pi %>% rename(x_1 = p_1, #Since we are not dealing with population p_ anymore
                                           x_2 = p_2,
                                           pi = p_pi,
                                           y = p_y)
  
  #########
  # Oracle mean
  # this is a standard estimator which is done on the full (nonmissing) data to compare
  hat_N_sample <- sum(1 / df$pi)
  statistic_tracker$oracle_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * df$y)
  
  #########
  # Drop some labels - weighted to high y
  
  indices <- sample(1:nrow(df), .2*nrow(df), prob = df$y) #weights are stdized to 1 by sample()
  dropped_obs <- df[indices,]
  reduced_df <- df[-indices,] # These have a large differnce: 296 for dropped and 276 for reduced df
  
  # Make oracle df without noisy parameters
  odf <- df[,1:4]
  o_dropped_obs <- odf[indices,]
  o_reduced_df <- odf[-indices,]
  
  #########
  # pi-corrected naive mean
  # should be unbiased but very large variance. should come forward when gen func change
  ## NOTE DIFFERENCE OF HAT N RESPOND AND HAT N SAMPLE
  hat_N_respondent <- sum(1 / (reduced_df$pi)) # largely underestimates the true N
  statistic_tracker$pi_naive_mean[i] <- (1 / hat_N_respondent)*sum((1 / reduced_df$pi) * 
                                                                     reduced_df$y)
  
  #########
  # Median imputation: fill missing values with median
  len <- dim(dropped_obs)[1]
  median_list <- rep(median(reduced_df$y), len)
  labels <- as.vector(reduced_df$y)
  median_list <- as.vector(median_list)
  
  imputed_list <- c(labels,median_list)
  
  statistic_tracker$median_imp_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * 
                                                                     imputed_list)
  
  #########
  # mean according to imputed data via linear regression (1/pi-weighted)
  red_weight <- 1 / reduced_df$pi
  lin_dat <- select(reduced_df, -c(pi))
  lin_dropped <- select(dropped_obs, -c(pi))
  
  linear_model <- lm(y ~ ., data = lin_dat, weights = red_weight)
  
  lm_y_hat <- predict(linear_model, lin_dropped)
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$lin_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                           + sum(lm_y_hat / dropped_obs$pi))
  
  # Linear Regression Oracle Method
  red_weight <- 1 / o_reduced_df$pi
  lin_dat <- select(o_reduced_df, -c(pi))
  lin_dropped <- select(o_dropped_obs, -c(pi))
  
  linear_model <- lm(y ~ ., data = lin_dat, weights = red_weight)
  
  lm_y_hat <- predict(linear_model, lin_dropped)
  hat_N_sample <- sum(1/odf$pi)
  statistic_tracker$lin_oracle[i] <- (1 / hat_N_sample)*(sum(o_reduced_df$y / o_reduced_df$pi) 
                                                         + sum(lm_y_hat / o_dropped_obs$pi))
  
  
  # nn oracle
  y_train <- o_reduced_df$y
  reduced_df_nolab <- select(o_reduced_df, -c(pi, y))
  
  y_test <- o_dropped_obs$y
  dropped_obs_nolab <- select(o_dropped_obs, -c(pi,y))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 170, 
    verbose = 0,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_oracle[i] <- (1 / hat_N_sample)*(sum(o_reduced_df$y / o_reduced_df$pi) 
                                                        + sum(nn_y_hat / o_dropped_obs$pi))
  
  
  #oracle nn pi feature
  y_train <- o_reduced_df$y
  reduced_df_nolab <- select(o_reduced_df, -c(y))
  reduced_df_nolab$pi <- 1 / reduced_df_nolab$pi
  
  y_test <- o_dropped_obs$y
  dropped_obs_nolab <- select(o_dropped_obs, -c(y))
  dropped_obs_nolab$pi <- 1 / dropped_obs_nolab$pi
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 300,
    verbose = 0,
    batch_size = 32,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_pi_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_pi_oracle[i] <- (1 / hat_N_sample)*(sum(o_reduced_df$y / o_reduced_df$pi) 
                                                           + sum(nn_pi_y_hat / o_dropped_obs$pi))
  
  # oracle resample
  dropped_obs_NA_lab <- o_dropped_obs
  dropped_obs_NA_lab$y <- NA
  
  orig_df <- rbind(o_reduced_df, dropped_obs_NA_lab)
  
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
    layer_dense(units = 32, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 200,
    verbose = 0,
    batch_size = 32,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_resamp_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_resamp_oracle[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                                 + sum(nn_resamp_y_hat / resamp_dropped_obs$pi))
  
  # weighted MSE oracle
  y_train <- o_reduced_df$y
  reduced_df_nolab <- select(o_reduced_df, -c(y))  # NEED TO DROP PI LATER. CANT DROP NOW SINCE NEED TO EXTRACT PI FROM TRAINING DATA
  
  y_test <- o_dropped_obs$y
  dropped_obs_nolab <- select(o_dropped_obs, -c(y))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  #normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  obs_weights <- 1 / partial_x_train[,3]
  partial_x_train <- partial_x_train[,-3]
  x_train <- x_train[,-3]
  x_val <- x_val[,-3]
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    sample_weight = obs_weights,
    epochs = 250,
    verbose = 0,
    batch_size = 32,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  x_test <- x_test[,-3]
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_wmse_oracle[i] <- (1 / hat_N_sample)*(sum(o_reduced_df$y / o_reduced_df$pi) 
                                                             + sum(nn_y_hat / o_dropped_obs$pi))
  
  
  # Oracle derived params
  y_train <- o_reduced_df$y
  reduced_df_nolab <- select(o_reduced_df, -c(y))
  
  y_test <- o_dropped_obs$y
  dropped_obs_nolab <- select(o_dropped_obs, -c(y))
  
  x1pi <- (reduced_df_nolab$x_1)*(reduced_df_nolab$pi)
  x2pi <- (reduced_df_nolab$x_2)*(reduced_df_nolab$pi)
  
  dx1pi <- (dropped_obs_nolab$x_1)*(dropped_obs_nolab$pi)
  dx2pi <- (dropped_obs_nolab$x_2)*(dropped_obs_nolab$pi)
  
  reduced_df_nolab <- cbind(reduced_df_nolab, x1pi, x2pi)
  dropped_obs_nolab <- cbind(dropped_obs_nolab, dx1pi, dx2pi)
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  normalize_data(x_train, x_test)
  create_validation_split(x_train, y_train)
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae"))
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 300, #high variance; study further
    verbose = 0,
    batch_size = 32,  #what should this be
    validation_data = list(x_val, y_val)  
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_deriv_oracle[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                                + sum(nn_y_hat / dropped_obs$pi))
  
  
  print(i)
}

# Save the mean table so that we don't have to always re-run it
write.csv(statistic_tracker, file = "C:\\Users\\Alexander\\Documents\\thesis stat tracker\\AWS_ALEXHERE_2.csv")
dat <- statistic_tracker

#dat <- read.csv("c:/Users/Alexander/Documents/thesis stat tracker/10_full.csv")
#dat <- dat[,-1]

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

library(kableExtra)
library(knitr)

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
