# Loading Packages

library(janitor)
library(here)
library(tidyverse)

# Reading and Viewing Data

decathlon_raw <- read_rds("task_1/raw_data/decathlon.rds")

glimpse(decathlon)

# cleaning names

decathlon <- clean_names(decathlon_raw)

# The row names need to be converted into a names column
# We also need to clean this names column as some of the athletes have their
# names occurring more than once, but in different cases
  
decathlon_names <- decathlon %>% 
  rownames_to_column("name") %>% 
  mutate(name = tolower(name),
         competition = as.character(competition))

decathlon_clean <- decathlon_names

# 1. Who had the longest long jump seen in the data?

decathlon_clean %>% 
  slice_max(long_jump)

# 2. What was the average 100m time in each competition?

decathlon_clean %>% 
  group_by(competition) %>% 
  summarise(avg_100m_time = mean(x100m))

# 3. Who had the highest total points across both competitions?

decathlon_clean %>% 
  group_by(name) %>% 
  summarise(avg_points = mean(points)) %>% 
  slice_max(avg_points)

# 4. What was the shot-put scores for the top three competitors in each competition?

decathlon_clean %>%
  group_by(competition) %>% 
  slice_max(shot_put, n = 3) %>% 
  select(competition, name, shot_put)

# 5. What was the average points for competitors who ran the 400m in less than 50 seconds vs. those than ran 400m in more than 50 seconds?

# We have some competitors who have run both under 50s and over 50s but we can just include them in both under 50s and over

# Less than 50s
decathlon_clean %>% 
  filter(x400m < 50) %>% 
  group_by(name) %>% 
  summarise(competitor_points = mean(points)) %>% 
  summarise(overall_point_avg = mean(competitor_points))
  
# More than 50s
decathlon_clean %>% 
  filter(x400m > 50) %>% 
  group_by(name) %>% 
  summarise(competitor_points = mean(points)) %>% 
  summarise(overall_point_avg = mean(competitor_points))
