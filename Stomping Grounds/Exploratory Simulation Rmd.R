
library(data.table)
library(dplyr)
library(tidyr)




bigData <- drop_na(bigData)
bigData <- sapply(bigData, as.numeric)
bigData <- as.data.frame(bigData)

# take pairwise correlation of columns w inclusion prob

corStorage <- numeric(20)
# first need to prune down data to numerics before correlations make sense
for (i in 1:20) {
  vecInc <- bigData$FINLWT21 #inclusion probability vector (already inversed)
  vecDat <- as.numeric(bigData[ ,i]) # every other vector
  corStorage[i] <- cor(vecInc, vecDat)
}
corStorage
# implies 1 and 5 col are most correlated to inc prob
# BLS_URBN and POPSIZE. 7 not bad either
# but now, 5,6,10 most prominent (10 very negative?)
#########################################

# I CHANGED FROM CREDFINX PREDICTION TO SOMETHING MORE DEVELOPED?
# Also Dropping NAS on predict column since I think that's what problem is
# actually for now, CREDFINX cast to numeric. lets see

# Just taking big stack data with reduced vars and getting down to
# business
library(keras)
library(dplyr)
library(tidyr)
set.seed(1)

dataset <- bigData

smp_size <- floor(.8*nrow(dataset))
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

# Segregate Train and Test
train_data <- dataset[train_ind, ]
test_data <- dataset[-train_ind, ]

# Remove targets as seperate vector
train_targets <- train_data$CREDFINX
test_targets <- test_data$CREDFINX

train_data <- subset(train_data, select = -c(CREDFINX))
test_data <- subset(test_data, select = -c(CREDFINX))

## Normalizing the data

# normalize only the nonfactor predictors, not output?
#mean <- apply(train_data, 2, mean)
#std <- apply(train_data, 2, sd)
#train_data <- scale(train_data, center = mean, scale = std)
#test_data <- scale(test_data, center = mean, scale = std)

# Test must be first, since we are directly modding data
#test_data$FINCBTAX <- (test_data$FINCBTAX-mean(train_data$FINCBTAX))/sd(train_data$FINCBTAX)
#train_data$FINCBTAX <- (train_data$FINCBTAX-mean(train_data$FINCBTAX))/sd(train_data$FINCBTAX)

#test_data$FSALARYX <- (test_data$FSALARYX-mean(train_data$FSALARYX))/sd(train_data$FSALARYX)
#train_data$FSALARYX <- (train_data$FSALARYX-mean(train_data$FSALARYX))/sd(train_data$FSALARYX)

#test_data$FSSIX <- (test_data$FSSIX-mean(train_data$FSSIX))/sd(train_data$FSSIX)
#train_data$FSSIX <- (train_data$FSSIX-mean(train_data$FSSIX))/sd(train_data$FSSIX)

#test_data$RETSURVM <- (test_data$RETSURVM-mean(train_data$RETSURVM))/sd(train_data$RETSURVM)
#train_data$RETSURVM <- (trian_data$RETSURVM-mean(train_data$RETSURVM))/sd(train_data$RETSURVM)

# Model Definition

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(19)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

val_indices <- 1:2000 #want like 3000 randoms from train?

x_val <- train_data[val_indices,]
partial_x_train <- train_data[-val_indices,]

y_val <- train_targets[val_indices]
partial_y_train <- train_targets[-val_indices]

partial_x_train <- as.matrix(partial_x_train)

# this runs
model %>% fit(partial_x_train,
              partial_y_train,
              batch_size = 128,
              epochs = 10)


# this doesnt
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 128,
  validation_data = list(x_val, y_val)
)
















build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", #sigmoid is differntiable, but sigmoid is
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

#batches and epochs nonoptimized: need to find from prev stuff
history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)
str(history)












################## WORKKKKKKKKKINGGGGGGGGGGGGGGGGGGGGG
# https://stackoverflow.com/questions/42379138/keras-valueerror-no-data-provided-for-input-1-need-data-for-each-key#42381526
library(keras)

#The network should identify the rule that a row sum greater than 1.5 should yield an output of 1

df <- bigData
df <- drop_na(df)

my_y = df$FINCBTAX

#my_x=matrix(data=runif(30000), nrow=10000, ncol=3)
my_x = select(df, -c(FINCBTAX))
my_x = as.matrix(my_x)

#my_y=ifelse(rowSums(my_x)>1.5,1,0)
#my_y=to_categorical(my_y, 2)

model = keras_model_sequential()
layer_dense(model,units = 2000, activation = "relu", input_shape = c(19))
layer_dense(model,units = 50, activation = "relu")
layer_dense(model,units = 1)

compile(model,loss = "mse",optimizer = "rmsprop",metrics = "mae")

history <- fit(model,  my_x, my_y, epochs = 5, batch_size = 128, validation_split = 0.2)

evaluate(model,my_x, my_y,verbose = 0)

predict_classes(model,my_x)

