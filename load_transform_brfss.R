# code snippet for converting SAS to R file -------------------------------
# library(foreign)
# 
# # read .XPT file and save
# brfss11 <- read.xport("./data/LLCP2011.XPT")
# brfss13 <- read.xport("./data/LLCP2013.XPT")
# brfss15 <- read.xport("./data/LLCP2015.XPT")
# save(brfss11, file="../data/brfss11.rda")
# save(brfss13, file="../data/brfss13.rda")
# save(brfss15, file="../data/brfss15.rda")
# 
# # clean up workspace
# rm(brfss11, brfss13, brfss15)

# load and transform relevant data ----------------------------------------
# preliminaries
library(car)
library(dplyr)

# load relevant datasets from 2011-2015 (yrs containing blood pressure/cholesterol q's)
load("../data/brfss11.rda")
load("../data/brfss13.rda")
load("../data/brfss15.rda")

# list of variables (state, diagnosis, 4 known diabetes predictors, 12 lifestyle variables)
vlist <- c("X_STATE", "DIABETE3", "BPHIGH4", "TOLDHI2", "CVDINFR4",
           "CVDSTRK3", "GENHLTH", "EDUCA", "X_INCOMG", "SEX", 
           "EXERANY2", "SMOKE100", "DRNKANY5", "SEATBELT", 
           "FRUTDA1_", "GRENDAY_", "ORNGDAY_", "BEANDAY_")

# extract relevant variables for each year and combine into super dataset
brfssdata <- rbind(subset(brfss11,,vlist), subset(brfss13,,vlist), subset(brfss15,,vlist))
rm(brfss11,brfss13,brfss15)
#dim(brfssdata)

# recode individual columns appropriately (see report/BRFSS website for individual question info.)
brfssdata <- brfssdata %>%
  mutate(DIABETE3 = car::recode(DIABETE3, "c(1)=1; else=0")) %>% 
  mutate(BPHIGH4  = car::recode(BPHIGH4,  "c(1)=1; else=0")) %>%
  mutate(TOLDHI2  = car::recode(TOLDHI2,  "c(1)=1; else=0")) %>%
  mutate(CVDINFR4 = car::recode(CVDINFR4, "c(1)=1; else=0")) %>%
  mutate(CVDSTRK3 = car::recode(CVDSTRK3, "c(1)=1; else=0")) %>%
  mutate(SEX      = car::recode(SEX,      "c(1)=1; else=0")) %>%
  mutate(GENHLTH  = car::recode(GENHLTH,  "c(7,9,NA)=0"))    %>%
  mutate(EDUCA    = car::recode(EDUCA,    "c(9,NA)=0"))      %>%
  mutate(X_INCOMG = car::recode(X_INCOMG, "c(9,NA)=0"))      %>%
  mutate(EXERANY2 = car::recode(EXERANY2, "c(1)=1; else=0")) %>%
  mutate(SMOKE100 = car::recode(SMOKE100, "c(1)=1; else=0")) %>%
  mutate(DRNKANY5 = car::recode(DRNKANY5, "c(1)=1; else=0")) %>%
  mutate(SEATBELT = car::recode(SEATBELT, "c(7,8,9,NA)=0"))  %>%
  mutate(FRUTDA1_ = car::recode(FRUTDA1_, "c(NA)=0"))        %>%
  mutate(GRENDAY_ = car::recode(GRENDAY_, "c(NA)=0"))        %>%
  mutate(ORNGDAY_ = car::recode(ORNGDAY_, "c(NA)=0"))        %>%
  mutate(BEANDAY_ = car::recode(BEANDAY_, "c(NA)=0"))
  #note: NA's for four healthy eating variables are set to 1 so that log(value) = 0
  #also: these four variables are given in (servings/day)*100 i.e., 50 = 0.5 serving/day
  
# examine distribution of each variable
x11(10,10) #larger display window
par(mfrow = c(4, 4), mar = c(4, 5, 2, 2), xpd = TRUE);
for (i in 1:16){
  hist(brfssdata[,i+2],main=names(brfssdata[i+2]))
  title(main=names(brfssdata[i+2]))
}
#note: binary/asymmetric binary and categorical variables are not transformed in any way 
#(except for dummy encoding of categorical variables)

# explore a log-transform for the four magnitude variables (healthy eating behaviors)
#thresh <- 1000 #assume (servings/day)*100 > threshold due coding error (e.g., 7777/9999 code)
# for (i in 15:18) {
#   brfssdata[brfssdata[,i] > thresh, i] = runif(sum(brfssdata[,i] > thresh))*100 # minumum is 1 (i.e. log(minumum) = 0)
#   if (i == 16) {
#     greens_pretransform <- brfssdata[,i]
#   }
#   brfssdata[brfssdata[,i] == 0, i] <- log(0.1)
#   brfssdata[brfssdata[,i] > 0,i] <- log(brfssdata[brfssdata[,i] > 0,i]) # take log 
#   brfssdata[,i] <- (brfssdata[,i])#/max(brfssdata[,i]) # normalize
#   #brfssdata[brfssdata[,i] < 0, i] <- 0 # bound a v. small handful of negative values, likely caused by 300/777 response categories
# }
# greens_posttransform <- brfssdata$ORNGDAY_
# # illustrate an example transformation
# par(mfrow = c(1, 2));
# green_pre <- hist(greens_pretransform, plot=FALSE)
# green_post <- hist(greens_posttransform, plot=FALSE)
# hist(green_pre$counts, green_pre$breaks, col = "green4", main="Servings Greens/Day (raw)")
# hist(green_post$counts, green_post$breaks, col = "green4", main="Servings Greens/Day (log-transformed)")

# log-transforming the four magnitude variables does not handle the 'Never' (0) responses well; 
# instead, split into two groups and binarize by rule-of-thumb threshold (low/high freq. healthy eating behaviors)
thresh <- 33 #e.g., one-third unit of fruit/day 
for (i in 15:18) {
  ind_healthy_eaters <- brfssdata[,i] > 33
  brfssdata[ind_healthy_eaters, i] <- 0
  brfssdata[!ind_healthy_eaters, i] <- 1 
}

# examine final distributions for use in model-fitting
x11(10,10) #larger display window
par(mfrow = c(4, 4), mar = c(4, 5, 2, 2), xpd = TRUE);
for (i in 1:16){
  hist(brfssdata[,i+2], main=names(brfssdata[i+2]))
  title(main=names(brfssdata[i+2]))
}

# convert variables to appropriate type for model fitting (factor or dbl)
for (i in 3:18) {
  brfssdata[,i] <- factor(brfssdata[,i])
}

# take a quick final look at data
glimpse(brfssdata)

# save to file and clean-up
save(brfssdata, vlist, file = "../data/brfssdata.rda")
