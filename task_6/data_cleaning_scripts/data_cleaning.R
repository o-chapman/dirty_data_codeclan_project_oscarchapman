#### Loading libraries and reading data

library(janitor)
library(tidyverse)

dogs_raw <- clean_names(read_csv("task_6/raw_data/dog_survey.csv"))


#### Investigating data

glimpse(dogs_raw)

dogs_raw %>% 
  group_by(x10, x11) %>% 
  summarise(n())

# columns x10 and x11 are filled with NA data, so they will be removed
dogs <- dogs_raw %>% 
  select(-x10, -x11)


# check for duplicate rows
sum(duplicated(dogs))

# there are 8 duplicates to be removed
dogs_distinct <- distinct(dogs)


#### Cleaning bad values

# cleaning dog_size col
dogs %>% 
  group_by(dog_size) %>% 
  summarise(n())

good_size_values <- c("XS", "S", "M", "L", "XL")

dogs_distinct_size <- dogs_distinct %>% 
  mutate(dog_size = if_else(dog_size %in% good_size_values,
                            dog_size,
                            NA_character_))

# cleaning dog_gender
dogs %>% 
  group_by(dog_gender) %>% 
  summarise(n())

dogs_size_gender <- dogs_distinct_size %>% 
  mutate(dog_gender = if_else(str_detect(dog_gender, "^[MF]$") == TRUE,
                            dog_gender,
                            NA_character_))

# cleaning dog ages
# the column is currently character so I want to isolate the age part
dogs_size_gender_age <- dogs_size_gender %>% 
  mutate(dog_age = as.numeric(str_extract(dog_age, "\\d+")))

# cleaning amount_spent_on_dog_food
dogs_clean <- dogs_size_gender_age %>% 
  mutate(amount_spent_on_dog_food = as.numeric(str_extract(amount_spent_on_dog_food, "\\d+")))

#### Making sure there is one dogs info per row

dogs_clean %>% 
  group_by(last_name, dog_size, dog_gender) %>% 
  summarise(occurences = n()) %>% 
  filter(occurences > 1)
# We have nothing coming back from the query so it is acceptable to assume that
# there are no occurrences of the same dog

write_csv(dogs_clean, "task_4/clean_data/dogs_clean.csv")
