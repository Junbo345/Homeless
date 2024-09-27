#### Preamble ####
# Purpose: Tests the cleaned date is neat and correct
# Author: Junbo Li
# Date: 26 SEP 2024
# Contact: junb.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned analysis data
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)


#### Test data ####
data <- read.csv("data/analysis_data/analysis_data.csv")

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
if (check_empty_cells(data)) {
  print("The data frame contains empty cells (NA or empty strings).")
} else {
  print("The data frame has no empty cells.")
}



# Test 2: Ensure the sum of Pre-working, Working, and Retirement equals the Total Population
test_sum_equals_total <- all(round(data$Early_Career_Stage +
  data$Working_Age +
  data$Pre_Retirement_and_Retirement_Age) == round(data$actively_homeless))

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
result <- test_numerical_gt_zero(data)

# Display the result
if (result) {
  print("All numerical inputs are greater than 0")
} else {
  print("Some numerical inputs are less than or equal to 0")
}
