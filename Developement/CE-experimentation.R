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

# load data
# Read in one quarter (kelly said so: can verify)

fmli171x <- read.csv("data/intrvw17/intrvw17/fmli171x.csv")
fmli172 <- read.csv("data/intrvw17/intrvw17/fmli172.csv")
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

median_impute <- function(df) {
  
  for (i in 1:dim(df)[[2]]) {
    df[[i]] <- as.numeric(df[[i]])
    df[[i]][is.na(df[[i]])] <- median(df[[i]][!is.na(df[[i]])])
  }
  
  df
  
}
# Turn data set into only numerics
#dplyr::select_if(x, is.numeric)

# Impute missing columns w uninformative median imputation: preserve maximal obs
# Could revist rows with lots of missingness. ####
# Could drop missing obs that are missing our Testing Label: 
# we need actual information there for out tests
df <- fmli171x
count_nas(df)

idf <- median_impute(df)
count_nas(idf)

not_all_na <- function(x) {!all(is.na(x))}
didf <- idf %>% select_if(not_all_na)

count_nas(didf)

write.csv(didf, file = "Data/didf.csv")

# Assuming data is in the correct shape, here's how the real testing goes down

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

# Split the CE data into training and testing. Might want some kind of scheme to 
# get the right amount of high-pi and low-pi observations in each
# (testing is unlabelled, but we know the right answer)

reduced_df <- a
dropped_obs <- b

#########
# True mean

#########
# Oracle mean
# this is a standard estimator which is done on the full (nonmissing) data to compare
hat_N_sample <- sum(1 / df$pi)
statistic_tracker$oracle_mean[i] <- (1 / hat_N_sample) * sum((1 / df$pi) * df$y)

#########
# pi-weighted naive mean
# non-imputation of estimate which accounts for complex design
hat_N_respondent <- sum(1 / (reduced_df$pi))
statistic_tracker$pi_naive_mean[i] <- (1 / hat_N_respondent)*sum((1 / reduced_df$pi) * 
                                                                   reduced_df$y)

# Compute median imputation mean estimate

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
# Linear regression imputation: MSE 1/pi weighted
red_weight <- 1 / reduced_df$pi
lin_dat <- select(reduced_df, -c(pi))
lin_dropped <- select(dropped_obs, -c(pi))

linear_model <- lm(y ~ ., data = lin_dat, weights = red_weight)

lm_y_hat <- predict(linear_model, lin_dropped)
hat_N_sample <- sum(1/df$pi)
statistic_tracker$lin_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                         + sum(lm_y_hat / dropped_obs$pi))
#########
# Mean accoridng to imputed data via naive neural network (ignore complex design)
y_train <- reduced_df$y
reduced_df_nolab <- select(reduced_df, -c(pi, y))

y_test <- dropped_obs$y
dropped_obs_nolab <- select(dropped_obs, -c(pi,y))

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
  epochs = 375, 
  verbose = 0,
  batch_size = 32,  #512
  validation_data = list(x_val, y_val)
)
#390
x_test <- as.matrix(x_test)
nn_y_hat <- predict(model, x_test)

hat_N_sample <- sum(1/df$pi)
statistic_tracker$nn_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                        + sum(nn_y_hat / dropped_obs$pi))
#########
# Mean according to imputed data via neural network w pi feature
y_train <- reduced_df$y
reduced_df_nolab <- select(reduced_df, -c(y))
reduced_df_nolab$pi <- 1 / reduced_df_nolab$pi

y_test <- dropped_obs$y
dropped_obs_nolab <- select(dropped_obs, -c(y))
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
  epochs = 250,
  verbose = 0,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

x_test <- as.matrix(x_test)
nn_pi_y_hat <- predict(model, x_test)

hat_N_sample <- sum(1/df$pi)
statistic_tracker$nn_pi_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                           + sum(nn_pi_y_hat / dropped_obs$pi))

## Mean according to dataset imputed via neural network with custom weighted MSE loss
# CAUTION: had to do some tricks to extract obs_weights for training (and not validition). uses column 4 for pi, 
# which might change if features change

# need to pull weights off of training data: this is getting split into validation.

y_train <- reduced_df$y
reduced_df_nolab <- select(reduced_df, -c(y))  # NEED TO DROP PI LATER. CANT DROP NOW SINCE NEED TO EXTRACT PI FROM TRAINING DATA

y_test <- dropped_obs$y
dropped_obs_nolab <- select(dropped_obs, -c(y))

reduced_df_nolab <- as.matrix(reduced_df_nolab)
dropped_obs_nolab <- as.matrix(dropped_obs_nolab)

x_train <- reduced_df_nolab
x_test <- dropped_obs_nolab

normalize_data(x_train, x_test)
create_validation_split(x_train, y_train)

obs_weights <- 1 / partial_x_train[,3]  # this is non-generalizable and will be a problem if we change the number of features. 

partial_x_train <- partial_x_train[,-3]
x_val <- x_val[,-3]
x_test <- as.matrix(x_test)
x_test <- x_test[,-3]

model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(partial_x_train)[[2]]) %>%
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
  epochs = 105,
  verbose = 0,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)


nn_y_hat <- predict(model, x_test)

hat_N_sample <- sum(1/df$pi)
statistic_tracker$nn_wmse_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                             + sum(nn_y_hat / dropped_obs$pi))

##########
# Mean according to imputed dataset via neural network with weighted resample
# on the full sample. the missing values in the resample are then imputed and 
# the imputation mean is taken on the new data set
# (without pi feature)
dropped_obs_NA_lab <- dropped_obs
dropped_obs_NA_lab$y <- NA

orig_df <- rbind(reduced_df, dropped_obs_NA_lab)

# sample by inclusion probability or pi ?????
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
  epochs = 200,
  verbose = 0,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

x_test <- as.matrix(x_test)
nn_resamp_y_hat <- predict(model, x_test)

hat_N_sample <- sum(1/df$pi)
statistic_tracker$nn_resamp_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                               + sum(nn_resamp_y_hat / resamp_dropped_obs$pi))
########
# Derived-parameter NN imputation, where derived parameters are ??

y_train <- reduced_df$y
reduced_df_nolab <- select(reduced_df, -c(y))

y_test <- dropped_obs$y
dropped_obs_nolab <- select(dropped_obs, -c(y))

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
statistic_tracker$nn_deriv_imp_mean[i] <- (1 / hat_N_sample)*(sum(reduced_df$y / reduced_df$pi) 
                                                              + sum(nn_y_hat / dropped_obs$pi))


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