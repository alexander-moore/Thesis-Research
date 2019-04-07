# Read in all FMLI files and stack them (these are quarterly files)

#First get column names

fmli151x <- read.csv("data/intrvw15/intrvw15/fmli151x.csv")
fmli152  <- read.csv("data/intrvw15/intrvw15/fmli152.csv")
fmli153  <- read.csv("data/intrvw15/intrvw15/fmli153.csv")
fmli154  <- read.csv("data/intrvw15/intrvw15/fmli154.csv")
fmli161  <- read.csv("data/intrvw15/intrvw15/fmli161.csv")


#memi151x <- read.csv("C:/Users/Alexander/Documents/Data File Downloads/memi151x.csv")

pruning <- function(df = data.frame) {
  #variables to keep
  sub1 <- subset(df, select = c(BLS_URBN, CUTENURE, EARNCOMP,
                                FAM_TYPE, POPSIZE, REGION, 
                                SMSASTAT, CHILDAGE, INCLASS, 
                                STATE, INCLASS2, OCCUCOD1, 
                                FINCBTAX, FSALARYX, 
                                FSSIX, RETSURVM, BUILDING,
                                CUTENURE, FINLWT21, CREDFINX))
  #BLS_URBN to factor
  #sub1$BLS_URBN <- as.factor(sub1$BLS_URBN)
  #CUTENURE to factor
  #sub1$CUTENURE <- as.factor(sub1$CUTENURE)
  #EARNCOMP to factor, potentially drop
  #sub1$EARNCOMP <- as.factor(sub1$EARNCOMP)
  #FAM_TYPE to factor
  #sub1$FAM_TYPE <- as.factor(sub1$FAM_TYPE)
  #POPSIZE to factor
  #sub1$POPSIZE <- as.factor(sub1$POPSIZE)
  #REGION to factor
  #sub1$REGION <- as.factor(sub1$REGION)
  #SMSASTAT to logi / factor?
  #sub1$SMSASTAT <- as.factor(sub1$SMSASTAT)
  #CHILDAGE to factor
  #sub1$CHILDAGE <- as.factor(sub1$CHILDAGE)
  #INCLASS to factor
  #sub1$INCLASS <- as.factor(sub1$INCLASS)
  #STATE to factor
  #sub1$STATE <- as.factor(sub1$STATE)
  #INCLASS2 to factor
  #sub1$INCLASS2 <- as.factor(sub1$INCLASS2)
  #BUILDING to factor
  #sub1$BUILDING <- as.factor(sub1$BUILDING)
  #OCCUCOD1 to factor
  #sub1$OCCUCOD1 <- as.factor(sub1$OCCUCOD1)
  #CUTENURE
  #sub1$CUTENURE <- as.factor(sub1$CUTENURE)
  #EDUCA to factor
  #sub1$EDUCA <- as.factor(sub1$EDUCA)
  sub1$FSSIX <- as.numeric(sub1$FSSIX)
  sub1$CREDFINX <- as.numeric(sub1$CREDFINX)
  sub1$RETSURVM <- as.numeric(sub1$RETSURVM)
  # Inverse to get inclusion probs
  sub1$FINLWT21 <- 1/sub1$FINLWT21
  
  return(sub1)
}

clean_fmli151x<- pruning(fmli151x)
clean_fmli152 <- pruning(fmli152)
clean_fmli153 <- pruning(fmli153)
clean_fmli154 <- pruning(fmli154)
clean_fmli161 <- pruning(fmli161)

# big 30,000 obs dataset :)
bigData <- rbind(clean_fmli151x, clean_fmli152, 
                 clean_fmli153, clean_fmli154, 
                 clean_fmli161)
