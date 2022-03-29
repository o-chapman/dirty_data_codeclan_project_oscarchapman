# Loading libraries and reading in data

library(tidyverse)
library(janitor)

cake_raw <- read_csv("task_2/raw_data/cake-ingredients-1961.csv")

cake_codes <- read_csv("task_2/raw_data/cake_ingredient_code.csv")

glimpse(cake_raw)

glimpse(cake_codes)

# First replacing all NAs with 0s

cake_no_nas <- cake_raw %>% 
  mutate_all(~replace(., is.na(.), 0))

# Pivoting the data set longer, creating a column called ingredient which lists
# the ingredient used
# This is in preparation for joining with cake_codes and for easier analysis

cake_pivoted_no_nas <- cake_no_nas %>% 
  pivot_longer(-Cake,
               names_to = "ingredient_code",
               values_to = "amount")
  
# Joining the tables on the ingredient code
# Then o on to clean up names and convert everything to lower case

cake_full <- cake_pivoted_no_nas %>% 
  full_join(
    cake_codes,
    by = c("ingredient_code" = "code")
  ) %>% 
  clean_names() %>% 
  transmute(cake = tolower(cake),
            ingredient = tolower(ingredient),
            amount,
            measure)

# There are NAs in the column measure for the sour cream cup ingredient
# Replacing these with "one"

cake_full <- cake_full %>% 
  mutate(measure = if_else(
    is.na(measure) == TRUE, "one",
    measure
  ))

# Writing to csv

write_csv(cake_full, "task_2/clean_data/cake_clean.csv")

