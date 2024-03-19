###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder
#
# Note the ## ---- name ---- notation
# This is done so one can pull in the chunks of code into the Quarto document
# see here: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html


## ---- packages --------
#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing/cleaning
library(tidyr) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here) #to set paths

## ---- loaddata --------

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
data_location <- here::here("data","raw-data","500_Cities__Cancer__excluding_skin_cancer__among_adults_aged___18_years_20240314.csv")
rawdata <- read.csv(data_location)


## ---- exploredata --------
#take a look at the data
dplyr::glimpse(rawdata)

#another way to look at the data
summary(rawdata)

#yet another way to get an idea of the data
head(rawdata)

#this is a nice way to look at data
skimr::skim(rawdata)




## ---- cleandata --------
# The data looks generally clear before cleaning. 
#So I just want to make sure that "Day" could be 
#defined as a categorical variable but not a numeric variable.


# choose needed categories
d1 <- rawdata %>%
  filter(GeographicLevel == "City", DataValueTypeID == "CrdPrv", StateAbbr != "US")

# drop uninterested variables for a smaller dataset
d2 <- d1 %>%
  select(Year, StateAbbr, StateDesc, CityName, DataValueTypeID, Data_Value,PopulationCount)

# generating a new variable
d3 <- d2 %>%
  mutate(Cancer_Case = round((Data_Value / 100) * PopulationCount))

# re-check with the data
dplyr::glimpse(d3)
summary(d3)
head(d3)
skimr::skim(d3)

## ---- savedata --------
# all done, data is clean now. 
# Let's assign at the end to some final variable
# makes it easier to add steps above
# name the file
processeddata <- d3
# location to save the file
save_data_location <- here::here("data","processed-data","processeddata_cancer.rds")
saveRDS(processeddata, file = save_data_location)



## ---- notes --------
# anything you don't want loaded into the Quarto file but 
# keep in the R file, just give it its own label and then don't include that label
# in the Quarto file

# Dealing with NA or "bad" data:
# removing anyone who had "faulty" or missing data is one approach.
# it's often not the best. based on your question and your analysis approach,
# you might want to do cleaning differently (e.g. keep individuals with some missing information)

# Saving data as RDS:
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# However, CSV is better for sharing with others since it's plain text. 
# If you do CSV, you might want to write down somewhere what each variable is.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata



