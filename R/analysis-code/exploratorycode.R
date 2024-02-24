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

## ---- table1 --------
summary_df = skimr::skim(mydata)
print(summary_df)
# save to file
summarytable_file = here("starter-analysis-exercise","results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)


## ---- ANOVA table --------
p2 <- mydata %>% ggplot(aes(x=Weight)) + geom_histogram() 
plot(p2)
figure_file = here("starter-analysis-exercise","results","weight_distribution.png")
ggsave(filename = figure_file, plot=p2)


## ---- LSD test --------
# Perform LSD test
lsd_result <- LSD.test(model1, "Treatment", alpha = 0.05)

# Display the results
print(lsd_result)

# save LSD result

lsdtable_file = here("results", "tables", "lsdtable.rds")
saveRDS(lsd_result, file = lsdtable_file)

 