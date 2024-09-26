#### Preamble ####
# Purpose: Simulates homeless population data
# Author: Junbo Li
# Date: SEP-20-2024
# Contact: junb.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(dplyr)


#### Simulate data ####
# Load necessary libraries

# Set the seed for reproducibility
set.seed(123)

# Create a date sequence from 2018-Jan to 2024-Jan (monthly data)
dates <- seq(as.Date("2018-01-01"), as.Date("2024-01-01"), by = "month")

# Repeat each date 3 times
repeated_dates <- rep(dates, each = 3)

# Format the dates in "MMM-YY" format
formatted_dates <- format(repeated_dates, "%b-%y")

# Simulate the total population as a normally distributed random variable
mean_population <- 10000
sd_population <- 1000
total_population <- round(rnorm(length(dates), mean = mean_population, sd = sd_population))

# Ensure total population is positive
total_population[total_population < 0] <- 0

# Simulate chronic cases (chronic number is below total population)
chronic <- round(runif(length(dates), min = 0.01, max = 0.99) * total_population)

# Simulate refugee cases (refugee number is below total population)
refugee <- round(runif(length(dates), min = 0.01, max = 0.99) * total_population)

# Combine the three columns into one long column alternately
df <- data.frame(total_population, chronic, refugee)
combined_population <- c(t(df))

# Simulate working status components
# These three categories should sum up to the total population
pre_working <- round(combined_population * runif(length(dates)*3, min = 0.2, max = 0.4))
working <- round(combined_population * runif(length(dates)*3, min = 0.4, max = 0.6))
retirement <- combined_population - pre_working - working

# Create a dataframe with all the data
population_data <- data.frame(
  "Date" = formatted_dates,
  "Population_group" = rep(c("All Population", "Chronic", "Refugees"), times = length(dates)),
  "actively_homeless" = combined_population,
  "Pre_Working" = pre_working,
  "Working" = working,
  "Retirement" = retirement
)

# View the first few rows of the dataframe
head(population_data)

# Test 1: Ensure all entries are greater than 0
test_greater_than_zero <- all(population_data$Total_Population > 0) &
  all(population_data$Pre_Working >= 0) &
  all(population_data$Working >= 0) &
  all(population_data$Retirement >= 0) &
  all(population_data$Chronic >= 0) &
  all(population_data$Refugee >= 0)

if(test_greater_than_zero) {
  print("Test 1 Passed: All entries are greater than or equal to 0.")
} else {
  print("Test 1 Failed: Some entries are less than 0.")
}

# Test 2: Ensure the sum of Pre-working, Working, and Retirement equals the Total Population
test_sum_equals_total <- all(population_data$Pre_Working + 
                               population_data$Working + 
                               population_data$Retirement == population_data$Total_Population)

if(test_sum_equals_total) {
  print("Test 2 Passed: The sum of Pre-working, Working, and Retirement equals the Total Population.")
} else {
  print("Test 2 Failed: The sum of the working categories does not equal the total population.")
}

