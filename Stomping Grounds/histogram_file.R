## Histogram Practice
library(readr)
library(ggplot2)
library(dplyr)

CE_results <- read_csv("data/LR_epo_running_1.csv")
dat <- CE_results[,-1]

init <- data.frame(matrix(ncol = 2, nrow = 800))

init[,1] <- c(dat$true_mean, dat$oracle_mean, dat$pi_naive_mean, dat$median_imp_mean, 
              dat$lin_imp_mean, dat$nn_imp_mean, dat$nn_pi_imp_mean, dat$nn_resamp_imp_mean,
              dat$nn_wmse_imp_mean, dat$nn_deriv_imp_mean)

# Now 2nd column is method name
for (i in 1:ncol(dat)) {
  df_names <- names(dat)
  
  for (j in 1:80) {
    index <- 80 * (i - 1) + j
    
    init[index,2] <- df_names[i]
  }
  
}

dim(init)
colnames(init) <- c("Score", "Method")

ggplot(init, aes(Score, color = Method)) +
  geom_freqpoly() +
  geom_vline(xintercept = as.numeric(dat[1,1]))




#### Without the icky ones
CE_results <- read_csv("data/LR_epo_running_1.csv")
dat <- CE_results[,-1]

true_mean <- as.numeric(dat[1,1])

dat <- select(dat, -c(true_mean, oracle_mean))

init <- data.frame(matrix(ncol = 2, nrow = (nrow(dat)*ncol(dat))))

init[,1] <- c(dat$pi_naive_mean, dat$median_imp_mean, 
              dat$lin_imp_mean, dat$nn_imp_mean, dat$nn_pi_imp_mean, dat$nn_resamp_imp_mean,
              dat$nn_wmse_imp_mean, dat$nn_deriv_imp_mean)

# Now 2nd column is method name
for (i in 1:ncol(dat)) {
  df_names <- names(dat)
  
  for (j in 1:80) {
    index <- 80 * (i - 1) + j
    
    init[index,2] <- df_names[i]
  }
  
}

dim(init)
colnames(init) <- c("Score", "Method")

ggplot(init, aes(Score, color = Method)) +
  geom_freqpoly() +
  geom_vline(xintercept = true_mean)

### MSE barchart


CE_results <- read_csv("data/LR_epo_running_1.csv")
dat <- CE_results[,-1]
dat <- dat[,-1]

mse_table <- dat[1,]
for (i in 1:dim(dat)[2]) {
  
  matdat <- as.matrix(dat)
  
  mse_table[i] <- mean( (matdat[,i] - matdat[,1])^2 )
}

mse_table

oracle_ratio_table <- dat[1,]

for (i in 1:dim(dat)[2]) {
  
  oracle_ratio_table[i] <- mse_table[i] / mse_table[2]
}

oracle_ratio_table
oracle_ratio_table <- oracle_ratio_table[,-1]

barplot(as.matrix(oracle_ratio_table), las=2, cex.names=.7)

library(grid)
library(gridBase)

mid <- barplot(as.matrix((oracle_ratio_table)))
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(names(names(oracle_ratio_table)),
          x = unit(mid, "native"), y=unit(-1, "lines"),
          just="right", rot=50)

popViewport(3)
