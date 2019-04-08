## Real Experiments in CE Data
# Documentation:
# https://www.bls.gov/cex/2017/csxintvwdata.pdf
library(readr)
library(dplyr)

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

# Turn data set into only numerics
#dplyr::select_if(x, is.numeric)

# Impute missing columns w uninformative median imputation: preserve maximal obs
# Could revist rows with lots of missingness. ####
# Could drop missing obs that are missing our Testing Label: 
# we need actual information there for out tests
df <- fmli171x
count_nas(df)

median_impute <- function(df) {
  
  for (i in 1:dim(df)[[2]]) {
    df[[i]] <- as.numeric(df[[i]])
    df[[i]][is.na(df[[i]])] <- median(df[[i]][!is.na(df[[i]])])
  }
  
  df
  
}

idf <- median_impute(df)
count_nas(idf)

not_all_na <- function(x) {!all(is.na(x))}
didf <- idf %>% select_if(not_all_na)

count_nas(didf)

write.csv(didf, file = "Data/didf.csv")

# Assuming data is in the correct shape, here's how the real testing goes down

# Split the CE data into training and testing. Might want some kind of scheme to 
# get the right amount of high-pi and low-pi observations in each
# (testing is unlabelled, but we know the right answer)

# Compute oracle mean estimate

# Compute median imputation mean estimate

# Run pi-weighted linear regression to impute, use formula from sim to compute mean estimate

# Run naive NN, compute mean estimate

# Run pi-feature NN, compute mean estimate

# Run weighted-MSE NN, compute mean estimate

# Run resample NN, compute mean estimate

# Run derived parameter NN, compute mean estimate


# Impute the truly missing labels for fun?