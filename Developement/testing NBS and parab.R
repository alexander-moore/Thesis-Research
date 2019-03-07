## testing new generative function and extrapolation to N BS


library(keras)
library(dplyr)
library(ggplot2)

N = 10^6
n = 10^4
it <- 2

p_y_ep_control <- 0

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

create_validation_split <- function(x_train, y_train) {
  val_indices <- 1:1000
  
  x_val <<- x_train[val_indices,]
  partial_x_train <<- x_train[-val_indices,]
  
  y_val <<- y_train[val_indices]
  partial_y_train <<- y_train[-val_indices]
}

p_1 <- runif(N, min = 1, max = 3)
p_2 <- runif(N, min = 1, max = 3)
p_3 <- runif(N, min = 1, max = 3)

#p_y_ep <- rnorm(N, mean = 0, sd = p_y_ep_control)

p_y <- 6*(p_1-2)^2 + 6*(p_2-2)^2 + 6*(p_3-2)^2 #+ p_y_ep

#p_pi_ep <- rnorm(N, mean = 0, sd = .5)
temp_pi <- sqrt(p_y) #+ p_pi_ep
temp_pi <- rescale(temp_pi)
p_pi <- temp_pi * (n / sum(temp_pi))

p_df <- cbind(p_1, p_2, p_3, p_y, p_pi)
p_tbl <- as_tibble(p_df)

# Verify sum of pi over population approx n
sum(p_tbl$p_pi) #= 10000 = n

statistic_tracker <- data.frame(true_mean = numeric(it), 
                                oracle_mean = numeric(it),
                                #naive_mean  = numeric(it),
                                #pi_naive_mean = numeric(it),
                                #median_imp_mean = numeric(it),
                                lin_imp_mean = numeric(it),
                                nn_imp_mean = numeric(it),
                                nn_pi_imp_mean = numeric(it),
                                #nn_resamp_imp_mean = numeric(it),
                                nn_wmse_imp_mean = numeric(it))
# Want to estimate:
mu_y <- (1 / N) * sum(p_tbl$p_y)
mu_y
statistic_tracker$true_mean <- mu_y

plot(sample_population_by_pi$p_1, sample_population_by_pi$p_y)

for (i in 1:it) {
  
  # This is the "survey dataset", which we will see many of during MC
  # Used to compare methods under many versions of imputation
  sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, 
                                      weight = p_pi)
  
  df <- sample_population_by_pi %>% rename(x_1 = p_1, #Since we are not dealing with p_ anymore
                                           x_2 = p_2,
                                           x_3 = p_3,
                                           pi = p_pi,
                                           y = p_y)
  
  # Oracle mean, \bar y ^o
  # this is a standard estimator which is done on the full (nonmissing) data to compare
  hat_N_sample <- sum(1 / df$pi)
  statistic_tracker$oracle_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * df$y)
  
  # Drop some labels - weighted to high y
  indices <- sample(1:nrow(df), .175*nrow(df), prob = df$y) #weights are stdized to 1 by sample()
  dropped_obs <- df[indices,]
  reduced_df <- df[-indices,] # These have a large differnce: 296 for dropped and 276 for reduced df
  
  
  # mean according to imputed data via linear regression
  
  # adding weight = 1/pi . doing a weighted MSE. this is better than naive Lin Regress
  linear_model <- lm(y ~ x_1 + x_2 + x_3, data = reduced_df, weights = reduced_df$pi)
  lm_y_hat <- predict(linear_model, dropped_obs)
  ## FIX HERE 
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$lin_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                           + sum(lm_y_hat / dropped_obs$pi))
  
  
  ## Mean according to dataset imputed via neural network with custom weighted MSE loss
  # CAUTION: had to do some tricks to extract obs_weights for training (and not validition). uses column 4 for pi, 
  # which might change if features change
  
  # need to pull weights off of training data: this is getting split into validation.
  
  ### I messed this up at some point. need to somehow preserve PI while scaling
  obs_weights <- reduced_df$pi
  
  y_train <- reduced_df$y
  reduced_df_nolab <- select(reduced_df, -c(y))
  
  y_test <- dropped_obs$y
  dropped_obs_nolab <- select(dropped_obs, -c(y))
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  mean <- apply(x_train, 2, mean)
  std <- apply(x_train, 2, sd)
  
  x_train <- scale(x_train, center = mean, scale = std)
  x_test <- scale(x_train, center = mean, scale = std)
  
  #reduced_df_nolab <- as.matrix(reduced_df_nolab)
  #dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  create_validation_split(x_train, y_train)
  
  obs_weights <- partial_x_train[,4]  # this is non-generalizable and will be a problem if we change the number of features. 
  partial_x_train <- partial_x_train[,-4]
  x_train <- x_train[,-4]
  x_val <- x_val[,-4]
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 3, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 3, activation = "relu") %>%
    layer_dense(units = 3, activation = "relu") %>%
    layer_dense(units = 3, activation = "relu") %>%
    layer_dense(units = 3, activation = "relu") %>%
    layer_dense(units = 3, activation = "relu") %>%
    layer_dense(units = 3, activation = "relu") %>%
    layer_dense(units = 3, activation = "relu") %>%
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
    epochs = 100,
    #verbose = 0,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  mean(nn_y_hat)
  
  hat_N_sample <- sum(1/df$pi)
  statistic_tracker$nn_wmse_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                               + sum(nn_y_hat / dropped_obs$pi))
                                                                 
  i
}

mse_table <- statistic_tracker[1,]
for (i in 1:dim(mse_table)[2]) {
  
  matdat <- as.matrix(statistic_tracker)
  
  mse_table[i] <- mean( (statistic_tracker[,i] - statistic_tracker[,1])^2 )
}

mse_table

oracle_ratio_table <- statistic_tracker[1,]

for (i in 1:dim(oracle_ratio_table)[2]) {
  
  oracle_ratio_table[i] <- mse_table[i] / mse_table[2]
}

oracle_ratio_table

glimpse(statistic_tracker)