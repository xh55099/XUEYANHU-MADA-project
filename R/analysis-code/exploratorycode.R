## ---- packages --------
#load needed packages. make sure they are installed.
library(here) #for data loading/saving
library(dplyr)
library(skimr)
library(ggplot2)

## ---- loaddata --------
#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("starter-analysis-exercise","data","processed-data","processeddata.rds")
#load data
mydata <- readRDS(data_location)

## ---- logistic --------
# Fit multinomial logistic regression model
#library(nnet)
#multinom_model <- multinom(FAF ~ MTRANS, data = mydata)

# Summary of the model
#summary(multinom_model)


## ---- lm1 --------
# linear model of BMI by water and FAF
lm1 <- lm(BMI ~ Water + FAF + Water * FAF, data = mydata)
lm1_result <- summary(lm1)

# save the result
lm_file = here("results", "tables", "lm1table.rds")
saveRDS(lm1_result, file = lm_file)


## ---- lm2 --------
mydata$Alcohol <- as.factor(mydata$Alcohol)
lm2 <- lm(BMI ~ Alcohol, data = mydata)
lm2_result <- summary(lm2)

# save the result
lm_file = here("results", "tables", "lm2table.rds")
saveRDS(lm2_result, file = lm_file)

## ---- lm3 --------
mydata1 <- mydata %>%
  select(History, Water, Alcohol, FAF, MTRANS, BMI)
lm3 <- lm(BMI ~ ., data = mydata1)
lm3_result <- summary(lm3)

# save the result
lm_file = here("results", "tables", "lm3table.rds")
saveRDS(lm3_result, file = lm_file)
 