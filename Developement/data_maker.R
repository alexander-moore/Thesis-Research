# Make CE-data ready to load into model code

# load data
# Read in one quarter (kelly said so: can verify)

fmli171x <- read.csv("data/intrvw17/intrvw17/fmli171x.csv")

count_nas <- function(df) {
  isna <- apply(df, 2, is.na)
  tots <- apply(isna, 2, sum)
  tots
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

df <- cbind(df, pi = 1 / df$FINLWT21)
which( colnames(df)=="FINLWT21")
which( colnames(df)=="pi" ) 

# Potential Labels
which( colnames(df)=="FINCBTAX" ) # (no missingness, but is BLS derived)
#AGE_REF_


count_nas(df)

## Remove columns with more than 33% NA
df <- df[, -which(colMeans(is.na(df)) > 0.33)]
count_nas(df)


# Median impute missing values, but NOT our chosen label
idf <- median_impute(df)
count_nas(idf)

# not sure what this is for, selects "not all NA" cols
#not_all_na <- function(x) {!all(is.na(x))}
#didf <- idf %>% select_if(not_all_na)
#count_nas(didf)

write.csv(idf, file = "Data/imputed_CE.csv")