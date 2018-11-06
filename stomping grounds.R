library(NHANES)
library(keras)
library(dplyr)
library(tidyr)

# Exploratory data analysis using NHANESraw data, which has
# inclusion probabilities of observations. 

# First we will do book example of regression
# Load data
dataset <- dataset_boston_housing()

c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

# Normalizing the data
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

# Model Definition
build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_data)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse", #this is where we could potential include weight
    metrics = c("mae")
  )  
}

# K-Fold Validation

k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)

num_epochs <- 500
all_scores <- c()
all_mae_histories <- NULL

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets, 
    validation_data = list(val_data, val_targets), 
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  
  mae_history <- history$metrics$val_mean_absolute_error
  all_mae_histories <- rbind(all_mae_histories, mae_history)
  
}

average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

library(ggplot2)
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + 
  geom_smooth()

# Training the Final Model
model <- build_model()
model %>% fit(train_data, train_targets,
              epochs = 80, batch_size = 16, verbose = 0)
result <- model %>% evaluate(test_data, test_targets)

result


# NHANES version of DL regression

set.seed(1)
dataset <- NHANESraw

dataset$Diabetes <- as.integer(dataset$Diabetes)-1
#drop_na(dataset$Diabetes)

# Remove nonNumeric columns
nums <- unlist(lapply(dataset, is.numeric))
dataset <- dataset[ , nums]

smp_size <- floor(.8*nrow(dataset))
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train_data <- dataset[train_ind, ]
test_data <- dataset[-train_ind, ]

train_targets <- train_data$Diabetes
test_targets <- test_data$Diabetes

train_data <- subset(train_data, select = -c(Diabetes))
test_data <- subset(test_data, select = -c(Diabetes))

# ignore, this is just how the amazon data is packed?
#c(c(train_data, train_targets), c(test_data, test_targets)) %<-% dataset

# Normalizing the data
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)
test_data <- scale(test_data, center = mean, scale = std)

# Model Definition
build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", #relu is nondifferntiable, but sigmoid is
                input_shape = dim(train_data)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse", #this is where we could potential include weight
    metrics = c("mae")
  )  
}

# K-Fold Validation

k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)

num_epochs <- 500
all_scores <- c()
all_mae_histories <- NULL

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices]
  
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets, 
    validation_data = list(val_data, val_targets), 
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  
  mae_history <- history$metrics$val_mean_absolute_error
  all_mae_histories <- rbind(all_mae_histories, mae_history)
  
}

average_mae_history <- data.frame(
  epoch = seq(1:ncol(all_mae_histories)),
  validation_mae = apply(all_mae_histories, 2, mean)
)

library(ggplot2)
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + 
  geom_smooth()

# Training the Final Model
model <- build_model()
model %>% fit(train_data, train_targets,
              epochs = 80, batch_size = 16, verbose = 0)
result <- model %>% evaluate(test_data, test_targets)

result
# loss: 19.795
# mean absolute error: 2.96
# this makes no sense given that we are working with binary response: 
# need to correct the output function to be sigmoid or something
# instead of regression

# Using correct model
library(keras)
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", 
              input_shape = dim(train_data)[[2]]) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", # would pass loss = alexLoss for my loss function
  metrics = c("accuracy")
)

val_indices <- 1:10000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)
str(history)