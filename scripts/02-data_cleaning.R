#### Preamble ####
# Purpose: Cleans the raw data, select instrest columns and rows, convert the
# data to quarterly mean mode instead of monthly, mutate the age group to better
# reflects working ability
# Author: Junbo Li
# Date: 13 SEP 2024
# Contact: junb.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: raw data downloaded
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/raw_data/raw_data.csv")
c
cleaned_data <-
  raw_data |>
  janitor::clean_names() 

cleaned_data1 <- cleaned_data |> 
  mutate(
    Early_Career_Stage = ageunder16 + age16_24) |>
  mutate(Working_Age = age25_34 + age35_44) |>
  mutate(Pre_Retirement_and_Retirement_Age = age45_54 + age55_64 + age65over) |>
  select(date_mmm_yy, population_group, actively_homeless,
         Early_Career_Stage, Working_Age, Pre_Retirement_and_Retirement_Age) |>
  filter(population_group == "All Population" | population_group == "Chronic" |
           population_group == "Refugees") |>
  tidyr::drop_na()

data <- cleaned_data1

data$date_mmm_yy <- as.Date(paste0("01-", data$date_mmm_yy), format="%d-%b-%y")

# Create a new column for Quarters
data$Quarter <- paste0(year(data$date_mmm_yy), "Q", quarter(data$date_mmm_yy))

# Group by Quarter and population_group, then calculate the average for all other columns
quarterly_data <- data %>%
  group_by(Quarter, population_group) %>%
  summarise(across(actively_homeless:Pre_Retirement_and_Retirement_Age, mean, 
                   na.rm = TRUE))

quarterly_data <- quarterly_data |> 
  filter(Quarter != '2024Q3')
#### Save data ####
write_csv(quarterly_data, "data/analysis_data/analysis_data.csv")

# Test 1: Ensure all entries are greater than 0
test_greater_than_zero <- all(quarterly_data$Total_Population > 0) &
  all(quarterly_data$Pre_Working >= 0) &
  all(quarterly_data$Working >= 0) &
  all(quarterly_data$Retirement >= 0) &
  all(quarterly_data$Chronic >= 0) &
  all(quarterly_data$Refugee >= 0)

if(test_greater_than_zero) {
  print("Test 1 Passed: All entries are greater than or equal to 0.")
} else {
  print("Test 1 Failed: Some entries are less than 0.")
}

# Test 2: Ensure the sum of Pre-working, Working, and Retirement equals the Total Population
test_sum_equals_total <- all(quarterly_data$Pre_Retirement_and_Retirement_Age + 
                               quarterly_data$Working_Age + 
                               quarterly_data$Early_Career_Stage == quarterly_data$actively_homeless)

if(test_sum_equals_total) {
  print("Test 2 Passed: The sum of Pre-working, Working, and Retirement equals the Total Population.")
} else {
  print("Test 2 Failed: The sum of the working categories does not equal the total population.")
}
