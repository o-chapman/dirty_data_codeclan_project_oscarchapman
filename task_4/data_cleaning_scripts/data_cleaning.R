# Loading libraries and Data

library(tidyverse)
library(here)
library(janitor)
library(readxl)

candy_2015 <- clean_names(read_excel("task_4/raw_data/boing-boing-candy-2015.xlsx"))

candy_2016 <- clean_names(read_excel("task_4/raw_data/boing-boing-candy-2016.xlsx"))

candy_2017 <- clean_names(read_excel("task_4/raw_data/boing-boing-candy-2017.xlsx"))

########################


#### EXPLORING DATA BEFORE CLEANING


## JOINING 2015 and 2016

# checking intersects and outersects of the dataset columns to see what to keep/omit
# and which rows have the same data but different col names

# intersect of 2016 and 2015
intersect_2015_2016 <- intersect(names(candy_2015), names(candy_2016))

# list of cols in 2015 and NOT 2016
setdiff_2015_2016 <- setdiff(names(candy_2015), names(candy_2016))
remove_list_2015 <- setdiff_2015_2016[14:31]

# list of cols in 2016 and NOT 2015
setdiff_2016_2015 <- setdiff(names(candy_2016), names(candy_2015))
remove_list_2016 <- setdiff_2016_2015[c(19, 25, 26:30)]


## RENAMING AND OMITTING COLUMNS

candy_2015_clean_names <- candy_2015 %>% 
  rename("age" = how_old_are_you,
         "trick_treating" = are_you_going_actually_going_trick_or_treating_yourself,
         "bonkers_candy" =  bonkers,
         "hersheys_dark_chocolate" = dark_chocolate_hershey,
         "hersheys_kisses" = hershey_s_kissables,
         "licorice_yes_black" = licorice
         ) %>% 
  select(-any_of(remove_list_2015))

candy_2016_clean_names <- candy_2016 %>% 
  rename("age" = how_old_are_you,
         "trick_treating" = are_you_going_actually_going_trick_or_treating_yourself,
         "country" = which_country_do_you_live_in,
         "state/province" = which_state_province_county_do_you_live_in,
         "gender" = your_gender,
         "box_o_raisins" = boxo_raisins) %>% 
  select(-any_of(remove_list_2016))

candy_2015_2016 <- candy_2015_clean_names %>% 
  full_join(candy_2016_clean_names)

## JOINING NEW DATASET TO 2016

# Firstly removing the "Q**_" from the start of each column

colnames(candy_2017) <- str_replace_all(colnames(candy_2017), "^q\\w{1,3}_", "")

# intersects and setdiffs with 2017
intersect_2017 <- intersect(names(candy_2015_2016), names(candy_2017))

setdiff_201516_2017 <- setdiff(names(candy_2015_2016), names(candy_2017))
remove_list_201516 <- setdiff_201516_2017[16:29]

setdiff_2017_201516 <- setdiff(names(candy_2017), names(candy_2015_2016))
remove_list_2017 <- setdiff_2017_201516[c(7:10, 14:24)]

  
## RENAMING AND OMITTING COLUMNS

candy_2015_2016_clean_names <- candy_2015_2016 %>% 
  select(-any_of(remove_list_201516))

candy_2017_clean_names <- candy_2017 %>% 
  rename(
    "trick_treating" = going_out,
    "state/province" = state_province_county_etc,
    "mary_janes" = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
    "x100_grand_bar" = "100_grand_bar",
    "box_o_raisins" = boxo_raisins
  ) %>% 
  select(-any_of(remove_list_2017))

candy_full <- candy_2015_2016_clean_names %>% 
  full_join(candy_2017_clean_names)

######################

#### Making sure col types are consistent and values are reasonable

## COUNTRY

# changing everything to lowercase firstly and changing numbers to NAs
candy_full <- candy_full %>% 
  mutate(country = tolower(country),
         country = case_when(is.na(as.numeric(country)) == FALSE ~ as.character(NA),
                             TRUE ~ country))

# Firstly getting a list of all inputs occurring in country column
candy_full %>% 
  group_by(country) %>% 
  summarise(n())

# Looks like the best way is to go through this and sort each value into the correct country

country_values <- candy_full %>% 
  group_by(country) %>% 
  summarise(n()) %>% 
  pull(country)

usa_names <- country_values[c(1, 4:6, 12, 38, 48:49, 54:55, 57, 62, 72, 77, 80:87, 89:90, 94:97, 100:127)]

country_values_not_usa <- setdiff(country_values, usa_names)

uk_names <- country_values_not_usa[c(20, 52, 66, 69:71)]

canada_names <- country_values_not_usa[c(9:10)]

# As we are only bothered about explicitly defining people from these three countries
# I will leave the other countries as they are.

candy_clean <- candy_full %>% 
  mutate(country = case_when(country %in% usa_names ~ "usa",
                             country %in% uk_names ~ "uk",
                             country %in% canada_names ~ "canada",
                             TRUE ~ country))


## GENDER

# Checking what values occur
candy_full %>% 
  group_by(gender) %>% 
  summarise(n())
# Actually seems to be reasonable values so won't change anything


## AGE

# Having glimpsed the data it's clear there's loads of weird values
# Strings as well as very high or low numbers occur, which need to be nulled for analysis

options(scipen = 999) # Taking away scientific notation

candy_clean <- candy_clean %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age = case_when(age > 110 ~ as.numeric(NA),
                         age < 5 ~ as.numeric(NA),
                         TRUE ~ age))


## STATE/PROVINCE

# Decided to remove this column as not needed in any of the analysis questions
candy_clean <- candy_clean %>% 
  select(-"state/province")


## Going trick or treating or not

candy_full %>% 
  group_by(trick_treating) %>% 
  summarise(n())
# Again, seems to be reasonable values


## Time column

# Checking to see if any NAs exist in the 2015 and 2016 datasets
candy_2015 %>% 
  filter(is.na(timestamp))

candy_2016 %>% 
  filter(is.na(timestamp))
# No NAs so we can assume that any NAs in the timestamp column will be from 2017
# in our combined dataset (as 2017 doesn't have a timestamp column)

# Changing any NAs to 2017 and shortening the other columns to just the year
candy_clean <- candy_clean %>% 
  mutate(timestamp = case_when(str_detect(timestamp, "^2015.+") == TRUE ~ "2015",
                               str_detect(timestamp, "^2016.+") == TRUE ~ "2016",
                               TRUE ~ "2017"))


## Last check through columns to omit any columns that don't list candy

#colnames(candy_clean)

candy_clean <- candy_clean %>% 
  select(-cash_or_other_forms_of_legal_tender,
         -dental_paraphenalia,
         -glow_sticks,
         -generic_brand_acetaminophen,
         -creepy_religious_comics_chick_tracts,
         -hugs_actual_physical_hugs,
         -vicodin,
         -broken_glow_stick,
         -internal_id)

## Adding an index column to separate each entry in the survey

candy_clean <- candy_clean %>% 
  rowid_to_column("index")


#### Now we can write the clean data into a csv

write_csv(candy_clean, "task_4/clean_data/candy_clean.csv")
