#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("inputs/data/raw_data.csv")

cleaned_data <-
  raw_data |>
  janitor::clean_names() 

cleaned_data1 <- cleaned_data |> 
  mutate(
    Early_Career_Stage = ageunder16 + age16_24) |>
  mutate(Working_Age = age25_34 + age35_44) |>
  mutate(Pre_Retirement_and_Retirement_Age = age45_54 + age55_64 + age65over) |>
  select(date_mmm_yy, population_group, actively_homeless, 
         gender_female, gender_male, gender_transgender_non_binary_or_two_spirit,
         Early_Career_Stage, Working_Age, Pre-Retirement_and_Retirement_Age) |>
  filter(population_group == "All Population" | population_group == "Chronic" |
           population_group == "Refugees") |>
  tidyr::drop_na()

#### Save data ####
write_csv(cleaned_data1, "outputs/data/analysis_data.csv")
