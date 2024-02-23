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

## ---- table1 --------
summary_df = skimr::skim(mydata)
print(summary_df)
# save to file
summarytable_file = here("results", "tables", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

## ---- seed --------
# Calculate average population by seed type
mydata_seed <- mydata %>%
  group_by(Seed) %>%
  summarize(mean_Selective = mean(Selective),
            sd_Selective = sd(Selective))

# Create the bar chart with standard deviation lines
bp1 <- ggplot(mydata_seed, aes(x = Seed, y = mean_Selective, fill = Seed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = 0.4)  +
  geom_errorbar(data = mydata_seed, aes(x = Seed, ymin = mean_Selective - sd_Selective, ymax = mean_Selective + sd_Selective, color = "black"),
                position = position_dodge(width = 0.1), width = 0.05) +
  labs(title = "The population difference by seed type",
       x = "Seed Type",
       y = "Average Population") +
  scale_color_manual(values = c("Alfalfa" = "lightblue", "Fenugreek" = "pink")) +
  theme_minimal()
plot(bp1)
figure_file = here("results","figures", "seed_type.png")
ggsave(filename = figure_file, plot=bp1) 

## ---- strain --------
# Create boxplot for bacterial population by 
bp2 <- ggplot(mydata, aes(x = Strain, y = Selective)) +
  geom_boxplot() +
  labs(title = "The population difference by treatment",
       x = "Strain type",
       y = "Average Population") +
  theme_minimal()
plot(bp2)
figure_file = here("results","figures", "strain_type.png")
ggsave(filename = figure_file, plot=bp2) 

## ---- treatment --------
# Calculate average population by seed type
mydata_treatment <- mydata %>%
  group_by(Treatment) %>%
  summarize(mean_Selective = mean(Selective),
            sd_Selective = sd(Selective))

# Create a continuous variable for fill
mydata_treatment$fill_color <- scales::rescale(mydata_treatment$mean_Selective)

# Create the bar chart with standard deviation lines
bp3 <- ggplot(mydata_treatment, aes(x = Treatment, y = mean_Selective, fill = fill_color)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = 0.4)  +
  geom_errorbar(data = mydata_treatment, aes(x = Treatment, ymin = mean_Selective - sd_Selective, ymax = mean_Selective + sd_Selective, color = "grey"),
                position = position_dodge(width = 0.1), width = 0.05) +
  labs(title = "The population difference by treatment",
       x = "Treatment",
       y = "Average Population") +
  scale_fill_gradient(low = "yellow", high = "red") +  # Adjust the colors as needed
  scale_color_manual(values = c("grey")) +  # Use grey color for error bar color
  guides(color = "none") +  # Turn off both fill and color legends
  theme_minimal()
plot(bp3)
figure_file = here("results","figures", "treatment.png")
ggsave(filename = figure_file, plot=bp3) 

## ---- day --------
# Calculate the average population for each day
mydata_day <- aggregate(Selective ~ Day, data = mydata, FUN = mean)

# Draw a point plot
sp4 <- ggplot(mydata_day, aes(x = Day, y = Selective)) +
  geom_point(size = 2, color = "blue") +
  geom_line(aes(group = 1), color = "red") +
  labs(title = "The growth trend of the bacterial population over time",
       x = "Day",
       y = "Average Bacterial Population") +
  theme_minimal()
plot(sp4)
figure_file = here("results","figures", "timepoint.png")
ggsave(filename = figure_file, plot=sp4)  

## ---- modelfit --------
# Fit the ANOVA model
model <- aov(Selective ~ Seed + Strain + Treatment + Day + Rep, data = mydata)

# Summarize the ANOVA results
linear_model <- summary(model)

# Save this table

anovatable_file = here("results", "tables", "anovatable.rds")
saveRDS(linear_model, file = anovatable_file)
