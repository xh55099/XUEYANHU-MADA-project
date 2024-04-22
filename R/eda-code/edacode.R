## ---- packages --------
#load needed packages. make sure they are installed.
library(here) #for data loading/saving
library(dplyr)
library(skimr)
library(ggplot2)

## ---- loaddata --------
#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","processeddata.rds")
#load data
mydata <- readRDS(data_location)

## ---- p1 to p3 --------
# Convert numeric values to labels
mydata$Gender <- ifelse(mydata$Gender == 1, "Male", "Female")

# bar chart of gender distribution
bc1 <- ggplot(mydata, aes(x = Gender)) +
  geom_bar(fill = c("pink", "skyblue"), color = "black", width = 0.5) +
  geom_text(stat = 'count', aes(label=..count..), vjust = -0.5, size=3, color="black") +
  labs(x = "Gender", y = "Count", title = "Bar Chart of Gender Distribution") +
  theme_minimal()
plot(bc1)

# histogram of population by age
# histogram of population by age
# Calculate the bin edges and labels
bin_data <- mydata %>%
  mutate(bin = cut(Age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, Inf), 
                   labels = FALSE, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(bin_min = min(Age), bin_max = max(Age)) %>%
  mutate(bin_min = as.integer(bin_min),  # Convert bin_min to integer
         bin_max = as.integer(bin_max),  # Convert bin_max to integer
         bin_label = sprintf("%d-%d", bin_min, bin_max))  # Format bin labels

hp2 <- ggplot(mydata, aes(x = Age)) +
  geom_histogram(binwidth = 10, fill = "grey", color = "black") +
  labs(x = "Age", y = "Count", title = "Histogram of Age distribution") +
  stat_bin(binwidth = 10, geom = "text", aes(label = ..count..),
           vjust = -0.5, color = "black", size = 3)  +
  geom_text(data = bin_data, aes(x = bin_min + (bin_max - bin_min)/2 - 5, y = -5, label = bin_label),
            vjust = +1.2, size = 3, color = "black") +  # Add text labels for bins
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))  # Adjust the size of x-axis text
plot(hp2)
# bar chart of obesity level distribution
bc3 <- ggplot(mydata, aes(x = Obesity)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label=..count..), vjust = -0.5, size=3, color="black") +
  scale_x_discrete(labels = c("Under", "Normal", "OW1", "OW2", "Obese1", "Obese2", "Obese3")) +
  labs(x = "Obesity level", y = "Count", title = "Bar Chart of obesity Distribution") +
  theme_minimal()
plot(bc3)

## ---- p4 and p5 --------
unique(mydata$Obesity)
# obesity level by gender
sbp4 <- ggplot(mydata, aes(x = Obesity, fill = Gender)) +
  geom_bar(position = "stack") +
  labs(x = "Obesity Level", y = "Count", title = "Gender Distribution by Obesity Level") +
  scale_fill_manual(values = c("skyblue", "pink"), labels = c("Male", "Female")) +
  scale_x_discrete(labels = c("Under", "Normal", "OW1", "OW2", "Obese1", "Obese2", "Obese3")) +
  theme_minimal()
plot(sbp4)

# Obesity level by age
# Categorize age into groups
mydata <- mydata %>%
  mutate(Age_Group = cut(Age, breaks = c(0, 20, 30, 40, 50, 60, 70, Inf),
                         labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71+")))

sbp5 <- ggplot(mydata, aes(x = Obesity, fill = Age_Group)) +
  geom_bar(position = "stack",stat = "count") +
  coord_flip() + # Rotate the plot to make it horizontal
  labs(x = "Obesity level", y = "Count", title = "Stacked Bar Chart of Obesity Level by Age") +
  theme_minimal()
plot(sbp5)

## ---- p6 --------
vp6 <-ggplot(mydata, aes(x = History, y = BMI, fill = History)) +
  geom_violin() +  # Add violin geometry
  geom_boxplot(width = 0.1, fill = "white", color = "black") +  # Add boxplot for better visualization
  geom_jitter(aes(color = History), width = 0.2, alpha = 0.5, size = 1) +   # Add jittered scatterplot
  labs(x = "Family History of Obesity", y = "BMI", title = "Violin Plot of BMI by History of Obesity") +
  scale_fill_manual(values = c("yes" = "lightyellow", "no" = "lightgreen")) +  # Define colors
  theme_minimal()
plot(vp6)

## ---- table --------
#summary1 <- summary(mydata)
summary2 <-skim(mydata)
summarytable_file = here("results", "tables", "summarytable1.rds")
saveRDS(summary1, file = summarytable_file)
summarytable_file = here("results", "tables", "summarytable2.rds")
saveRDS(summary2, file = summarytable_file)
