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

# Extract the year and quarter from the dates
quarters <- paste0(year(dates), "Q", quarter(dates))

# Keep only unique quarters (quarterly data)
quarterly_data <- quarters[!duplicated(quarters)]

# Repeat each date 3 times
repeated_dates <- rep(quarterly_data, each = 3)

# Simulate the total population as a normally distributed random variable
mean_population <- 10000
sd_population <- 1000
total_population <- round(rnorm(length(quarterly_data), mean = mean_population, sd = sd_population))

# Ensure total population is positive
total_population[total_population < 0] <- 0

# Simulate chronic cases (chronic number is below total population)
chronic <- round(runif(length(quarterly_data), min = 0.01, max = 0.99) * total_population)

# Simulate refugee cases (refugee number is below total population)
refugee <- round(runif(length(quarterly_data), min = 0.01, max = 0.99) * total_population)

# Combine the three columns into one long column alternately
df <- data.frame(total_population, chronic, refugee)
combined_population <- c(t(df))

# Simulate working status components
# These three categories should sum up to the total population
pre_working <- round(combined_population * runif(length(quarterly_data) * 3, min = 0.2, max = 0.4))
working <- round(combined_population * runif(length(quarterly_data) * 3, min = 0.4, max = 0.6))
retirement <- combined_population - pre_working - working

# Create a dataframe with all the data
population_data <- data.frame(
  "Date" = repeated_dates,
  "Population_group" = rep(c("All Population", "Chronic", "Refugees"), times = length(quarterly_data)),
  "actively_homeless" = combined_population,
  "Pre_Working" = pre_working,
  "Working" = working,
  "Retirement" = retirement
)

write_csv(population_data, "data/simulated_data/simulated_data.csv")

