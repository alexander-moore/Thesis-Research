# oracle only
df <- read.csv("Data/AWS_running_short.csv")
df <- df[,-1]

means <- apply(df, FUN = mean, MARGIN = 2)
means

sds <- apply(df, FUN = sd, MARGIN = 2)
sds

mean_ratio <- means / means[2]
mean_ratio

sd_ratio <- sds / sds[2]
sd_ratio

# fULL
df <- read.csv("Data/AWS_running_full.csv")
df <- df[,-1]

means <- apply(df, FUN = mean, MARGIN = 2)
means

sds <- apply(df, FUN = sd, MARGIN = 2)
sds

mean_ratio <- means / means[2]
mean_ratio

sd_ratio <- sds / sds[2]
sd_ratio

####
dat <- read.csv("Data/AWS_running_full.csv")
dat <- dat[,-c(1, 12, 13, 14, 15, 16)]
dat <- dat[,-11]

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
library(data.table)

mse_t <- transpose(mse_table)
oracle_t <- transpose(oracle_ratio_table)
prb_t <- transpose(prb_table)

binded <- cbind(mse_t, oracle_t, prb_t)
colnames(binded) <- c("MSE", "Oracle Ratio", "Relative Bias")
rownames(binded) <- colnames(dat)

test <- binded
test <- signif(test, digits = 3)

test %>%
  kable() %>%
  kable_styling()

