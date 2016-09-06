# preliminaries
library(car)
library(dplyr)
library(survey)
library(srvyr)
library(choroplethr)
library(choroplethrMaps)

# visualization of diabetes prevalence -----------------------------

# load BRFSS data from 2015 and list of state ID codes
load("../data/brfss15.rda")
state.id <- read.csv("../data/usa_state_codes_brfss.csv", stringsAsFactors=F)

# exploratory analysis of diabetes prevalence (use survey functions)
brfss15 <- brfss15 %>%
  mutate(DIABETE3 = car::recode(DIABETE3,  "c(1)=1; else=0"), 
         DIABETE3=as.factor(DIABETE3))
brfss15 <- brfss15 %>%
  as_survey_design(ids=X_PSU, variables= c(DIABETE3, X_STATE))

# compute prevalance by state
diabetes_prevalence <- brfss15 %>%
  group_by(X_STATE, DIABETE3) %>%
  dplyr::summarize(prevalence = survey_mean(na.rm=T), N = survey_total(na.rm=T)) %>%
  left_join(state.id, by=c("X_STATE"="id"))

diabetes_prevalence <- cbind(diabetes_prevalence[,7],diabetes_prevalence[,3])
names(diabetes_prevalence) <- c("region","value")

# plot and save to .png
png("../figures/figure1_diabetes_prevalence.png", height=300, width=650)
choroplethr::state_choropleth(diabetes_prevalence, title="Diabetes prevalence 2015", num_colors=7)
dev.off()


# healthy eating (farmer’s market prevalence in USA) ----------------------

# load in data from USDA on farmer's market locations and census population data
farmers_markets <- read.csv("../data/usa_farmers_markets_usda.csv", stringsAsFactors=F)
state_populations <- read.csv("../data/usa_state_population_estimates_census.csv", stringsAsFactors=F)

# calculate no. of farmer's markets per 100000 population per state
fmarket_counts <- count(farmers_markets, "State")
fmarket_counts <- fmarket_counts[-1,]
fmarket_counts <- fmarket_counts[!fmarket_counts$State=="Virgin Islands",]
fmarket_prevalence <- left_join(fmarket_counts, state_populations, by=c("State"="region"))
fmarket_prevalence <- data.frame("region" = tolower(fmarket_prevalence$State), 
                            "value"=(fmarket_prevalence$freq/fmarket_prevalence$value)*100000)

# plot and save to .png
png("../figures/figure1_fmarkets_prevalence.png", height=300, width=650)
choroplethr::state_choropleth(fmarket_prevalence, title="No. of farmer's markets (per/100000 people)", num_colors=7)
dev.off()
