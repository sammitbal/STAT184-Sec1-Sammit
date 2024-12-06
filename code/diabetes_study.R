#load packages
library(ggplot2)
library(dplyr)
library(tidyr)


# Prompt:
# You're working as a medical researcher at the NIH. Using the dataset provided,
# Which biomarker a most likely driver of diabetes (e.g. *causes* the greatest 
# increase in diabetes risk) -- insulin, glucose, or both equally?
# Note: For "outcome", 1=diabetes, 0= no diabetes

#load the data
setwd("~/STAT184")
data <- read.csv("diabetes_dataset.csv")

head(data)
View(data)

#clean the data
data = data %>% 
  mutate(Glucose_Quartile = ntile(Glucose, 3)) %>%
  mutate(Insulin_Quartile = ntile(Insulin, 3))

#EDA
head(data)
summary(data)

# Show the relationship between Insulin and Diabetes, and Glucose and Diabetes
ggplot(data, aes(x = factor(Outcome), y = log(Insulin))) +
  geom_boxplot() +
  labs(x = "Outcome", y = "Insulin", title = "Boxplot of Insulin by Diabetes Outcome") +
  theme_minimal()

ggplot(data, aes(x = factor(Outcome), y = Glucose)) +
  geom_boxplot() +
  labs(x = "Outcome", y = "Glucose", title = "Boxplot of Glucose by Diabetes Outcome") +
  theme_minimal()

ggplot(data, aes(x = log(Insulin), fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  labs(x = "Insulin", y = "Density", fill = "Outcome",
       title = "Density Plot of Insulin by Diabetes Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

ggplot(data, aes(x = Glucose, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  labs(x = "Glucose", y = "Density", fill = "Outcome",
       title = "Density Plot of Glucose by Diabetes Outcome") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

#Show the relationship between glucose and insulin
ggplot(data, aes(x = Glucose, y = log(Insulin))) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(x = "Glucose", y = "log(Insulin)",
       title = "Scatter Plot of Glucose vs. Insulin") +
  theme_minimal()

summary_data <- data %>%
  group_by(Glucose_Quartile, Insulin_Quartile) %>%
  summarise(Outcome_Percent = mean(Outcome == 1) * 100) %>%
  ungroup()

# Plotting hierarchical effects of glucose & insulin on diabetes 
ggplot(summary_data, aes(x = Glucose_Quartile, y = Outcome_Percent, color = factor(Insulin_Quartile))) +
  geom_line() +
  geom_point() +
  labs(x = "Glucose Quantile", y = "Diabetes Rate (%)", color = "Insulin Quantile",
       title = "Diabetes Rate by Glucose and Insulin Quantiles") +
  theme_minimal()

# Plotting hierarchical effects of insulin & glucose on diabetes 
ggplot(summary_data, aes(x = Insulin_Quartile, y = Outcome_Percent, color = factor(Glucose_Quartile))) +
  geom_line() +
  geom_point() +
  labs(x = "Insulin Quartile", y = "Diabetes Rate (%)", color = "Glucose Quartile",
       title = "Diabetes Rate by Glucose and Insulin Quantiles") +
  theme_minimal()

table(data$Glucose_Quartile, data$Insulin_Quartile)

