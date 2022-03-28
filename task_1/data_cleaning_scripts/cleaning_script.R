# Loading Packages

library(tidyverse)

# Reading and Viewing Data

decathlon_raw <- read_rds("task_1/raw_data/decathlon.rds")

glimpse(decathlon)

# Cleaning names

decathlon <- janitor::clean_names(decathlon_raw)

# The row names need to be converted into a names column
# We also need to clean this names column as some of the athletes have their
# names occurring more than once, but in different cases
  
decathlon_names <- decathlon %>% 
  rownames_to_column("name") %>% 
  mutate(name = tolower(name),
         competition = as.character(competition))

decathlon_clean <- decathlon_names

write_csv(decathlon_clean, file = "task_1/clean_data/decathlon_clean.csv")
