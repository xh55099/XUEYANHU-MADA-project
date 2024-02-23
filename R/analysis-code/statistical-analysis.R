###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

############################
#### summary table
summary_df = skimr::skim(mydata)
print(summary_df)
# save to file
summarytable_file = here("results", "tables", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

############################
#### model fit
# Fit the ANOVA model
model <- aov(Selective ~ Seed + Strain + Treatment + Day + Rep, data = mydata)

# Summarize the ANOVA results
linear_model <- summary(model)

# Save this table

anovatable_file = here("results", "tables", "anovatable.rds")
saveRDS(linear_model, file = anovatable_file)

  