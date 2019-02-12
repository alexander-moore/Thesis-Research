# Compare distributions of mean via different imputation methods on artifical data

library(keras)
library(dplyr)

N = 10^6
n = 10^4
it <- 50

p_y_ep_control <- 2

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

p_1 <- rnorm(N, mean = 30, sd = 2.5)
p_2 <- rnorm(N, mean = 15, sd = 2)
p_3 <- rnorm(N, mean = 5, sd = 1)

p_y_ep <- rnorm(N, mean = 0, sd = p_y_ep_control)
p_y <- sqrt(p_1*p_2) + p_3 + p_y_ep

p_pi_ep <- rnorm(N, mean = 0, sd = 2)
temp_pi <- sqrt(p_y) + p_pi_ep
temp_pi <- rescale(temp_pi)
p_pi <- temp_pi * (n / sum(temp_pi))

p_df <- cbind(p_1, p_2, p_3, p_y, p_pi)
p_tbl <- as_tibble(p_df)

statistic_tracker <- data.frame(true_mean = numeric(it), 
                                oracle_mean = numeric(it),
                                naive_mean  = numeric(it),
                                pi_naive_mean = numeric(it),
                                lin_imp_mean = numeric(it),
                                nn_imp_mean = numeric(it))
# Want to estimate:
mu_y <- (1 / N) * sum(p_tbl$p_y)

# monte carlo simulation of draws from population with comparison methods
for (i in 1:it) {
  
  sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, 
                                      weight = p_pi)
  
  df <- sample_population_by_pi %>% rename(x_1 = p_1, #Since we are not dealing with p_ anymore
                                           x_2 = p_2,
                                           x_3 = p_3,
                                           pi = p_pi,
                                           y = p_y)
  # verify sum of 1/pi over sample df = N
  sum(1/df$pi) #= 997824.5, approx 10^6
  
  # Verify sum of pi over df approx n
  sum(df$pi) #= 104, approx 100. not correct.
  
  # Can compute some of the statistics before dropping for ease
  mean(df$y) #= 26.258
  
  # Drop some labels - weighted to high y

  indices <- sample(1:nrow(df), .175*nrow(df), prob = df$y) #weights are stdized to 1 by sample()
  dropped_obs <- df[indices,]
  reduced_df <- df[-indices,]
  
  #mean of complete cases
  statistic_tracker$naive_mean[i] <- mean(reduced_df$y)
  
  # pi-corrected naive mean
  hat_N <- sum(1 / (reduced_df$pi))
  statistic_tracker$pi_naive_mean[i] <- (1 / hat_N)*sum((1 / reduced_df$pi) * 
                                                       reduced_df$y)
  
  # Oracle mean, \bar y ^o
  statistic_tracker$oracle_mean[i] <- (1 / N) * sum((1 / reduced_df$pi) * 
                                                      reduced_df$y)
  
  # mean according to imputed data via linear regression
  linear_model <- lm(y ~ x_1 + x_2 + x_3, data = reduced_df)
  lm_y_hat <- predict(linear_model, dropped_obs)
  statistic_tracker$lin_imp_mean[i] <- (1 / N)*(sum(reduced_df$y / reduced_df$pi) 
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
    epochs = 15,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)

  mean(nn_y_hat)
  
  statistic_tracker$nn_imp_mean[i] <- (1 / N)*(sum(reduced_df$y / reduced_df$pi) 
                                               + sum(nn_y_hat / dropped_obs$pi))
  
  print(i)
}

# MSE of these distributions of means compared to true

(1 / it) * sum((bar_y - mu_y)^2) # where bar_y is the vector of predicted mean

# MAE of these distributions of means compared to true

(1 / it) * sum(bar_y - mu_y) # where bar_y is the vector of predicted mean

# Plot distributions of means  