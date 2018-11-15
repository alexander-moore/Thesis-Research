library(keras)
library(survey)
library(dplyr)

set.seed(1)

## Creating a population to sample from 
N <- 10^6
n <- 10000
ep_sig_control <- 2


p_1 <- rnorm(N, mean = 30, sd = 2.5)
p_2 <- rnorm(N, mean = 15, sd = 2)
p_3 <- rnorm(N, mean = 5, sd = 1)

# create y as a piece-wise function with discontinuity
ty <- rep(0, N)

mp3 <- mean(p_3)
epsilon <- rnorm(N, mean = 0, sd = ep_sig_control)


for (i in 1:N) { #This should probably be done w [] subsetting
  if (p_3[i] < mp3) {
    ty[i] <- sqrt(p_1[i]*p_2[i]) + p_3[i] + epsilon[i]
  } else {
    ty[i] <- sqrt(p_1[i]*p_2[i]) + p_3[i] + 5 + epsilon[i]
  }
}
p_y <- ty

# old way, a polynomial could approximate probably?
#p_y <- (sqrt(p_1 * p_2) + ty) + epsilon

pi_noise <- rnorm(N, mean = 0, sd = 1)
temp_pi <- sqrt(p_y) + pi_noise 

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

temp_pi <- rescale(temp_pi)

p_pi <- temp_pi * (n / sum(temp_pi)) # scale so that sum is n
sum(p_pi)

# Report correlations
cor(p_1, p_y) 
cor(p_2, p_y) 
cor(p_3, p_y) 
cor(p_pi, p_y)

p_df <- cbind(p_1, p_2, p_3, p_pi, p_y)
p_tbl <- as_tibble(p_df)


sample_population_by_pi <- sample_n(tbl = p_tbl, size = n, replace = FALSE, weight = p_pi)

df <- sample_population_by_pi %>% rename(x_1 = p_1, #Since we are not dealing with p_ anymore
                                         x_2 = p_2,
                                         x_3 = p_3,
                                         pi = p_pi,
                                         y = p_y)

sum( (1 / df$pi) ) # approx = N

##############################################################
# Make better fake data. The x's will be independent for now.

# This isn't the data for now, this is just a stomping ground
# to test ideas to make better fake data in the future

x_1 <- rnorm(10000, mean = 30, sd = 2.5)
x_2 <- rnorm(10000, mean = 15, sd = 2)
x_3 <- rnorm(10000, mean = 5, sd = 1.5)


middle_no_correlation <- function(vec) {
  k <- vec
  for (i in 1:length(vec)) {
    if (vec[i] > .95*mean(vec)  &&  vec[i] < 1.05*mean(vec)) {
      k[i] = mean(vec)
    }
  }
  return(k)
}

t_2 <- middle_no_correlation(x_2)


make_discontinuity <- function(vec) {
  k <- vec
  for (i in 1:length(vec)) {
    
    if (vec[i] < mean(vec)) {
      k[i] <- vec[i] - 1
    } else {
      k[i] <- vec[i] + 1
    }
    
  }
  return(k)
}

t_1 <- make_discontinuity(x_1)

spice_up <- function(vec) {
  k <- log(vec+2)
  
  return(k)
}

t_3 <- spice_up(x_3)

y <- (sqrt(t_1 * t_2) + exp(t_3)) + epsilon
# - 2*log(x_1+x_3)   # more complexity


noise <- rnorm(10000, mean = 0, sd = 1.5)
pi <- sqrt(y) + noise 

rescale <- function(vec) { # rescale to 0,1
  (vec - min(vec)) / (max(vec) - min(vec))
}

pi <- rescale(pi)

df_pi <- cbind(x_1, x_2, x_3, pi)
df_npi <- cbind(x_1, x_2, x_3)

df <- df_pi
df <- df_npi

# Let's just functionalize normalize and validation right here right now

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



############################################################
############################################################

# Making a survey-weighted resample using Sample_n
# need to have Big df with y so that the scramble keeps them


weight_vec <- 1 / as.numeric(df$pi)

df <- as_tibble(df)
new_data <- sample_n(tbl = df, size = nrow(df), replace = TRUE, weight = weight_vec)

y <- new_data$y

df <- select(new_data, -c(y))

# Validating my bootstrapped data
alpha <- sum(1/weight_vec)

sum_ratio <- sum(y / new_data[,4])

normal_mean <- (1/length(y)) * sum(y)

# Using the survey-weighted dataset to train the model

smp_size <- floor(.75*nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

x_train <- df[train_ind, ]
x_test <- df[-train_ind, ]

y_train <- y[train_ind]
y_test <- y[-train_ind]

# Normalization
mean <- apply(x_train, 2, mean)
std <- apply(x_train, 2, sd)
x_train <- scale(x_train, center = mean, scale = std)
x_test <- scale(x_test, center = mean, scale = std)

# Model Definition
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(x_train)[[2]]) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

# Validation Set
val_indices <- 1:1000

x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

# Training the Model
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 100,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

BS_pi_results <- model %>% evaluate(x_test, y_test)
BS_pi_results

####################################

# Can also do BS_npi_results

####################################
# Multiple Set Bootstrap

df <- cbind(x_1, x_2, x_3, pi, y)
#df <- as.data.frame(df)

weight_vec <- as.numeric(unlist(df[,4]))

df <- as_tibble(df)

iterations <- 10

loss_results_vec <- vector("double", iterations)
mae_results_vec <- vector("double", iterations)


y <- new_data[,5]

# Do not edit df once we're in. First thing to 
for (i in 1:iterations) {
  new_data <- sample_n(tbl = df, size = nrow(df), 
                       replace = TRUE, weight = weight_vec)
  y <- new_data[,5]

  y <- as.numeric(unlist(y))
  
  temp_df <- select(new_data, -c(y))
  temp_df <- as.matrix(temp_df)
  
  create_split(temp_df)
  
  # Model Definition
  model <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
  
  # Create validation data, called x_val y_val, partial_x_train partial_y_train
  create_validation_split(x_train, y_train)
  
  partial_x_train <- as.matrix(partial_x_train)
  x_val <- as.matrix(x_val)
  
  # Training the Model
  history <- model %>% fit(
    partial_x_train,
    partial_y_train,
    epochs = 25,
    batch_size = 512,
    validation_data = list(x_val, y_val)
  )
  
  x_test <- as.matrix(x_test)
  
  BS_pi_results <- model %>% evaluate(x_test, y_test)

  loss_results_vec[i] <- BS_pi_results[1]
  mae_results_vec[i] <- BS_pi_results[2]
}













#############################################################
#############################################################

# Making a New Loss Function

#
#
# ARGGRHJGRHRGHHG THERE IS A SAMPLE WEIGHT PARAMETER IN KERAS _FIT_
#https://keras.rstudio.com/reference/fit.html
# yOU FOOL!
# https://stackoverflow.com/questions/51316307/custom-loss-function-in-r-keras
# for the future:  https://stackoverflow.com/questions/49862080/writing-a-loss-function-in-r-studio-using-keras

new_df <- df
y <- new_df$y
#pi <- new_df$pi
new_df <- select(new_df, -c(y))

create_split(new_df)
create_validation_split(x_train, y_train)
normalize_data(x_train, x_test)

weights <- partial_x_train$pi
partial_x_train <- select(partial_x_train, -c(pi))
x_val <- select(x_val, -c(pi))

partial_x_train <- as.matrix(partial_x_train)
x_val <- as.matrix(x_val)

model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(partial_x_train)[[2]]) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)
# Training the Model
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  sample_weight = weights,
  epochs = 25,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)










###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
##############    Graveyard ###################################







###############################################################
###############################################################
# First dataset: Fake data With inclusion Probability predictor
df <- df_pi

# Transpose to make fit keras network
df <- t(df)

smp_size <- floor(.75*nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

x_train <- df[train_ind, ]
x_test <- df[-train_ind, ]

y_train <- y[train_ind]
y_test <- y[-train_ind]

# Normalization
mean <- apply(x_train, 2, mean)
std <- apply(x_train, 2, sd)
x_train <- scale(x_train, center = mean, scale = std)
x_test <- scale(x_test, center = mean, scale = std)

# input shape should be number of predictors
# Model Definition
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(x_train)[[2]]) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

# Validation Set
val_indices <- 1:1000

x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

# Training the Model
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 100,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

pi_results <- model %>% evaluate(x_test, y_test)
pi_results



###############################################################
###############################################################
# Second dataset: Fake data Without inclusion Probability predictor
df <- df_npi

# Transpose to make fit keras network
df <- t(df)

smp_size <- floor(.75*nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

x_train <- df[train_ind, ]
x_test <- df[-train_ind, ]

y_train <- y[train_ind]
y_test <- y[-train_ind]

# Normalization
mean <- apply(x_train, 2, mean)
std <- apply(x_train, 2, sd)
x_train <- scale(x_train, center = mean, scale = std)
x_test <- scale(x_test, center = mean, scale = std)

# Model Definition
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(x_train)[[2]]) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

# Validation Set
val_indices <- 1:1000

x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

# Training the Model
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 100,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

npi_results <- model %>% evaluate(x_test, y_test)
npi_results










alex_mse <- function(y_true, y_pred){
  K <- backend()
  K$mean(obs_weight * (y_pred - y_true)^2, axis = -1)
}



customLoss <- function(obs_weights) {
  
  loss <- function(y_true, y_pred) {
    
    K <- backend()
    K$mean(obs_weights * K$square(y_pred - y_true), axis = -1)
    #K$mean(obs_weights * (y_pred - y_true)^2, axis = -1)
    
    #axis = 1, axis = 2?
    
  }
  
  return(loss)
  
}

my_loss <- customLoss(obs_weights = weights)


lossFunction <- function(y_true, y_pred) {
  
  K <- backend()
  K$mean((y_pred - y_true)^2)
  
  #K$mean(K$square(y_pred - y_true), axis = -1)
  #K$mean(obs_weights * (y_pred - y_true)^2, axis = -1)
  
  #axis = 1, axis = 2?
  
}





# now compile keras model with loss = customLoss(obs_weights = df$weights)
model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(x_train)[[2]]) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "rmsprop",
  loss = lossFunction,
  metrics = c("mae")
)








# Potential Solution: "Sneaking" the weights in
new_custom_loss <- function(y_true, y_pred) {
  weights <- y_true[,1]
  y_true <- y_true[,1]
  
  K <- backend()
  K$mean(obs_weight * (y_pred - y_true)^2, axis = -1)
}
# Note that the metric functions will need to be customized as well
# by adding y_true = y_true[:,0] at the top.


weighted_mse <- function(y_true, y_pred,weights){
  # convert tensors to R objects
  K        <- backend()
  y_true   <- K$eval(y_true)
  y_pred   <- K$eval(y_pred)
  weights  <- K$eval(weights)
  
  # calculate the metric
  loss <- sum(weights*((y_true - y_pred)^2)) 
  
  # convert to tensor
  return(K$constant(loss))
}


weights <- df_pi[4,]
weights <- as.array(weights)
weights <- as.vector(weights)
#weights <- as.double(weights)
weights <- vector("double", 10000)
weights <- rep(1, 10000)

model %>% compile(
  optimizer = "rmsprop",
  loss = customLoss(obs_weights = weights),
  metrics = c("mae")
)

model %>% compile(
  optimizer = "rmsprop",
  loss = my_loss,
  metrics = c("mae")
)














counter <- 0

for (i in 1:length(weights)) {
  if (typeof(weights[i]) == "double") {
    counter <- counter + 1
  }
}
counter

