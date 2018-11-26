library(keras)
library(dplyr)
library(survey)

# First ones are all done with PI_EPSILON variance.
# Second ones are all done with Y_EPSILON variance

##################
# We will test out naive, bootstrap, customLoss, and forest methods
# with variable Y noise (epsilon)
##################

#
## Creating a population to sample from 
N <- 10^6
n <- 10000
ep_sig_control <- 2

p_1 <- rnorm(N, mean = 30, sd = 2.5)
p_2 <- rnorm(N, mean = 15, sd = 2)
p_3 <- rnorm(N, mean = 5, sd = 1)

epsilon <- rnorm(N, mean = 0, sd = ep_sig_control)
p_y <- sqrt(p_1*p_2) + p_3 + epsilon


epsilon_list <- c(0, 1, 2, 3)
var_ep_noise_df_suitcase <- list()

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

# variable pi noise data sets
for (i in 1:4) {
  ep <- epsilon_list[i]
  pi_noise <- rnorm(N, mean = 0, sd = ep)
  temp_pi <- sqrt(p_y) + pi_noise
  
  temp_pi <- rescale(temp_pi)
  p_pi <- temp_pi * (n / sum(temp_pi))
  
  p_df <- cbind(p_1, p_2, p_3, p_pi, p_y)
  p_tbl <- as_tibble(p_df)
  
  sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, weight = p_pi)
  
  var_ep_noise_df_suitcase[[i]] <- sample_population_by_pi %>% rename(x_1 = p_1, #Since we are not dealing with p_ anymore
                                                         x_2 = p_2,
                                                         x_3 = p_3,
                                                         pi = p_pi,
                                                         y = p_y)
}

# variable y noise data sets
N <- 10^6
n <- 10000
ep_sig_control <- 2

p_1 <- rnorm(N, mean = 30, sd = 2.5)
p_2 <- rnorm(N, mean = 15, sd = 2)
p_3 <- rnorm(N, mean = 5, sd = 1)

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

y_noise_list <- c(1, 2, 3, 4)
var_y_noise_df_suitcase <- list()

pi_noise <- rnorm(N, mean = 0, sd = ep_sig_control)

for (i in 1:4) {
  ep <- y_noise_list[i]
  y_noise <- rnorm(N, mean = 0, sd = ep)
  p_y <- sqrt(p_1*p_2) + p_3 + y_noise
  
  temp_pi <- sqrt(p_y) + pi_noise
  
  temp_pi <- rescale(temp_pi)
  p_pi <- temp_pi * (n / sum(temp_pi))
  
  p_df <- cbind(p_1, p_2, p_3, p_pi, p_y)
  p_tbl <- as_tibble(p_df)
  
  sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, weight = p_pi)
  
  var_y_noise_df_suitcase[[i]] <- sample_population_by_pi %>% rename(x_1 = p_1, #Since we are not dealing with p_ anymore
                                                                     x_2 = p_2,
                                                                     x_3 = p_3,
                                                                     pi = p_pi,
                                                                     y = p_y)
}



# Utility initialization
create_split <- function(df) {
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

normalize_data <- function(x_train, x_test) {
  mean <- apply(x_train, 2, mean)
  std <- apply(x_train, 2, sd)
  x_train <<- scale(x_train, center = mean, scale = std)
  x_test <<- scale(x_test, center = mean, scale = std)
}

##################################################
# Naive network, no pi
# Traverse all of the datasets
##################################################
# First we do Variable Epsilon Noise

loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_ep_noise_df_suitcase)) {
  
  df <- var_ep_noise_df_suitcase[[i]]
  
  y <- df$y
  
  df <- select(df, -c(pi, y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec

# Here we do Variable Y Noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_y_noise_df_suitcase)) {
  
  df <- var_y_noise_df_suitcase[[i]]
  
  y <- df$y
  
  df <- select(df, -c(pi, y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec
#######################################################
# Naive network, with pi
# Traverse all of the datasets
#######################################################

# First we do Variable Epsilon Noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_ep_noise_df_suitcase)) {
  
  df <- var_ep_noise_df_suitcase[[i]]
  
  y <- df$y
  
  df <- select(df, -c(y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec

# here it is with Variable Y Noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_y_noise_df_suitcase)) {
  
  df <- var_y_noise_df_suitcase[[i]]
  
  y <- df$y
  
  df <- select(df, -c(y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec

############################################################
# Bootstrap data Naive network, without pi
# Traverse all of the datasets
############################################################
# First we do Variable Epsilon Noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_ep_noise_df_suitcase)) {
  
  df <- var_ep_noise_df_suitcase[[i]]
  
  weight_vec <- 1 / as.numeric(df$pi)
  
  df <- as_tibble(df)
  
  df <- sample_n(tbl = df, size = nrow(df), replace = TRUE, weight = weight_vec)
  
  y <- df$y
  
  df <- select(df, -c(pi, y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec

# Here we do Variable Y noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_y_noise_df_suitcase)) {
  
  df <- var_y_noise_df_suitcase[[i]]
  
  weight_vec <- 1 / as.numeric(df$pi)
  
  df <- as_tibble(df)
  
  df <- sample_n(tbl = df, size = nrow(df), replace = TRUE, weight = weight_vec)
  
  y <- df$y
  
  df <- select(df, -c(pi, y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec

#########################################################
# Bootstrap data Naive network, with pi
# Traverse all of the datasets
#########################################################
# Starting with Variable Epsilon Noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_ep_noise_df_suitcase)) {
  
  df <- var_ep_noise_df_suitcase[[i]]
  
  weight_vec <- 1 / as.numeric(df$pi)
  
  df <- as_tibble(df)
  
  df <- sample_n(tbl = df, size = nrow(df), replace = TRUE, weight = weight_vec)
  
  y <- df$y
  
  df <- select(df, -c(y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec

# Here we do Variable Y Noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_y_noise_df_suitcase)) {
  
  df <- var_y_noise_df_suitcase[[i]]
  
  weight_vec <- 1 / as.numeric(df$pi)
  
  df <- as_tibble(df)
  
  df <- sample_n(tbl = df, size = nrow(df), replace = TRUE, weight = weight_vec)
  
  y <- df$y
  
  df <- select(df, -c(y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec











########################################################################
########################################################################
# Using the sample_weight argument to weight the loss function by the inclusion probability
# 
# Starting with variable epsilon noise data frames

loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_ep_noise_df_suitcase)) {
  
  df <- var_ep_noise_df_suitcase[[i]]
  
  y <- df$y
  
  pi_weights <- df$pi
  
  df <- select(df, -c(pi, y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    sample_weight = pi_weights,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec

# here we do variable Y noise with sample_weight of pi
# Here we do Variable Y Noise
loss_results_vec <- rep(NA, 4)
mae_results_vec <- rep(NA, 4)

for (i in 1:length(var_y_noise_df_suitcase)) {
  
  df <- var_y_noise_df_suitcase[[i]]
  
  y <- df$y
  
  pi_weights <- df$pi
  
  df <- select(df, -c(pi, y))
  
  df <- as.matrix(df)
  
  # df processing
  create_split(df)
  create_validation_split(x_train, y_train)
  normalize_data(x_train, x_test)
  
  # model definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 6, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 6, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 40,
    batch_size = 512,
    sample_weight = pi_weights,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)
  
  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}

loss_results_vec
mae_results_vec