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


# Tests

# Test 1: Ensure all cells are non-empty
# Function to check if any cell in the data frame is empty (NA or "")
check_empty_cells <- function(df) {
  # Check for NA values
  has_na <- any(is.na(df))

  # Check for empty strings in character columns
  has_empty_strings <- any(sapply(df, function(x) {
    if (is.character(x)) {
      any(x == "")
    } else {
      FALSE
    }
  }))

  # Return TRUE if either NA or empty strings are found
  return(has_na || has_empty_strings)
}

# Apply the function
if (check_empty_cells(population_data)) {
  print("The data frame contains empty cells (NA or empty strings).")
} else {
  print("The data frame has no empty cells.")
}



# Test 2: Ensure the sum of Pre-working, Working, and Retirement equals the Total Population
test_sum_equals_total <- all(population_data$Pre_Working +
  population_data$Working +
  population_data$Retirement == population_data$actively_homeless)

if (test_sum_equals_total) {
  print("Test 2 Passed: The sum of Pre-working, Working, and Retirement equals the Total Population.")
} else {
  print("Test 2 Failed: The sum of the working categories does not equal the total population.")
}


# Test 3: Ensure all numerical cells are greater than 0
# Function to check if all numerical columns have values greater than 0
test_numerical_gt_zero <- function(df) {
  # Apply the condition only to numeric columns
  numeric_columns <- sapply(df, is.numeric)
  all(sapply(df[, numeric_columns], function(x) all(x > 0)))
}

# Apply the test
result <- test_numerical_gt_zero(population_data)

# Display the result
if (result) {
  print("All numerical inputs are greater than 0")
} else {
  print("Some numerical inputs are less than or equal to 0")
}
