## Testing network depth for more flexibility: let's overtrain!
library(keras)
library(dplyr)
library(ggplot2)

N <- 10^5
n <- 10^3
it <- 10

create_validation_split <- function(x_train, y_train) {
  # Validation Set
  val_indices <- sample(1:nrow(x_train), .20*nrow(df))
  
  x_val <<- x_train[val_indices,]
  partial_x_train <<- x_train[-val_indices,]
  
  y_val <<- y_train[val_indices]
  partial_y_train <<- y_train[-val_indices]
}

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

p_x <- runif(N, 2, 10)
p_y <- (p_x-6)^2

temp_pi <- sqrt(p_y)
temp_pi <- rescale(temp_pi)
p_pi <- temp_pi * (n / sum(temp_pi))


stat_track <- data.frame(true_mean = numeric(it), 
                         oracle_mean = numeric(it),
                         lin_imp_mean = numeric(it),
                         nn_imp_mean = numeric(it),
                         nn_pi_imp_mean = numeric(it),
                         nn_wmse_imp_mean = numeric(it))

p_df <- cbind(p_x, p_pi, p_y)
p_tbl <- as_tibble(p_df)

sum(p_tbl$p_pi) == n

mu_y <- (1 / N) * sum(p_tbl$p_y)
mu_y

for (i in 1:it) {
  stat_track$true_mean[i] <- mu_y
  
  # Create Sample
  sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, 
                 weight = p_tbl$p_pi)
  
  df <- sample_population_by_pi %>% rename(x = p_x, #Since we are not dealing with p_ anymore
                                           y = p_y,
                                           pi = p_pi)
  
  # Oracle mean (uses missing information to calculate a target estimate)
  hat_N_sample <- sum(1 / df$pi)
  stat_track$oracle_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * df$y)
  
  # Drop labels weighted to large labels
  indices <- sample(1:nrow(df), .25*nrow(df), prob = df$y) #weights are stdized to 1 by sample()
  
  dropped_obs <- df[indices,]
  reduced_df <- df[-indices,] # These have a large differnce: 296 for dropped and 276 for reduced df
  
  dropped_obs$y <- NA
  
  # Wieghted linear mse
  #train <- select(reduced_df, c(x, y))
  weights <- reduced_df$pi
  linear_model <- lm(y ~ x, data = reduced_df, weights = weights)
  
  lm_y_hat <- predict(linear_model, dropped_obs)
  
  hat_N_sample <- sum(1/df$pi)
  stat_track$lin_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                           + sum(lm_y_hat / dropped_obs$pi))
  i
  # Mean accoridng to imputed data via neural network
  # Normal NN, no pi feature
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
    layer_dense(units = 64, activation = "sigmoid", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 32, activation = "sigmoid") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 350, #high variance; study further
    verbose = 0,
    batch_size = 32,  #what should this be
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  viewx <- seq(from = 2, to = 10, by = .05)
  viewy <- predict(model, viewx)
  plot(viewx, viewy)
  
  mean(nn_y_hat)
  
  hat_N_sample <- sum(1/df$pi)
  stat_track$nn_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                          + sum(nn_y_hat / dropped_obs$pi))
  
  i
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
    layer_dense(units = 64, activation = "sigmoid", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 32, activation = "sigmoid") %>%
    layer_dense(units = 1)
  
  
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
  
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 350,
    verbose = 0,
    batch_size = 32,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_pi_y_hat <- predict(model, x_test)
  
  #viewx <- seq(from = 2, to = 10, by = .05)
  #viewy <- predict(model, viewx)
  #plot(viewx, viewy)
  
  hat_N_sample <- sum(1/df$pi)
  stat_track$nn_pi_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                             + sum(nn_pi_y_hat / dropped_obs$pi))
  
  ## Mean according to dataset imputed via neural network with custom weighted MSE loss
  obs_weights <- 1 / reduced_df$pi 
  
  y_train <- reduced_df$y
  reduced_df_nolab <- select(reduced_df, -c(y))  # NEED TO DROP PI LATER. CANT DROP NOW SINCE NEED TO EXTRACT PI FROM TRAINING DATA
  
  y_test <- dropped_obs$y
  dropped_obs_nolab <- select(dropped_obs, -c(pi,y))
  
  reduced_df_nolab <- as.matrix(reduced_df_nolab)
  dropped_obs_nolab <- as.matrix(dropped_obs_nolab)
  
  x_train <- reduced_df_nolab
  x_test <- dropped_obs_nolab
  
  create_validation_split(x_train, y_train)
  
  obs_weights <- partial_x_train[,2]  # this is non-generalizable and will be a problem if we change the number of features. 
  partial_x_train <- partial_x_train[,-2] #drop pi
  x_train <- x_train[,-2]
  x_val <- x_val[,-2]
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "sigmoid", 
                input_shape = 1) %>%
    layer_dense(units = 32, activation = "sigmoid") %>%
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
    epochs = 350,
    verbose = 0,
    batch_size = 32,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  nn_y_hat <- predict(model, x_test)
  
  #viewx <- seq(from = 2, to = 10, by = .05)
  #viewy <- predict(model, viewx)
  #plot(viewx, viewy)
  
  hat_N_sample <- sum(1/df$pi)
  stat_track$nn_wmse_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                               + sum(nn_y_hat / dropped_obs$pi))
  
  i
}

write.csv(stat_track, file = "C:\\Users\\Alexander\\Documents\\thesis stat tracker\\new_trainer.csv")


stat_track

dat <- read.csv("C:\\Users\\Alexander\\Documents\\thesis stat tracker\\new_trainer.csv")
dat <- dat[,-1]

#### COMPUTING MSE compared to true mean
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

library(kableExtra)
library(knitr)
library(purrr)

mse_t <- unlist(transpose(mse_table))
oracle_t <- unlist(transpose(oracle_ratio_table))
prb_t <- unlist(transpose(prb_table))

binded <- cbind(mse_t, oracle_t, prb_t)
colnames(binded) <- c("MSE", "Oracle Ratio", "Relative Bias")
#rownames(binded) <- colnames(dat)

test <- binded
test <- signif(test, digits = 3)

test <- test[-6,]

test %>%
  kable() %>%
  kable_styling()


