---
title: "Analysis - task_4"
output:
  html_document:
    keep_md: yes
    df_print: kable
date: '2022-03-31'
---

# Loading libraries and reading in data


```r
library(here)
```

```
## here() starts at D:/CodeClan/codeclan_homework/week_4/dirty_data_codeclan_project_oscarchapman
```

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.6     v dplyr   1.0.8
## v tidyr   1.2.0     v stringr 1.4.0
## v readr   2.1.2     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
candy <- read_csv(here("task_4/clean_data/candy_clean.csv"))
```

```
## Rows: 9362 Columns: 107
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (104): trick_treating, butterfinger, x100_grand_bar, anonymous_brown_glo...
## dbl   (3): index, timestamp, age
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```
# 1. What is the total number of candy ratings given across the three years. (Number of candy ratings, not the number of raters. Don’t count missing values)


```r
# making a long version of candy firstly as it's better for analysing the candy data

candy_long <- candy %>% 
  pivot_longer(cols = -c(index, timestamp, age, gender, country, trick_treating),
               names_to = "candy_name",
               values_to = "rating")

candy_long %>% 
  filter(is.na(rating) == FALSE) %>% 
  nrow()
```

```
## [1] 677527
```

# 2. What was the average age of people who are going out trick or treating?


```r
candy %>% 
  filter(trick_treating == "Yes") %>% 
  summarise(avg_age = mean(age, na.rm = TRUE))
```

<div class="kable-table">

| avg_age|
|-------:|
| 35.0904|

</div>

# 3. What was the average age of people who are not going trick or treating?


```r
candy %>% 
  filter(trick_treating == "No") %>% 
  summarise(avg_age = mean(age, na.rm = TRUE))
```

<div class="kable-table">

|  avg_age|
|--------:|
| 39.16249|

</div>

# 4. For each of joy, despair and meh, which candy bar recieved the most of these ratings? 

## JOY

```r
candy_long %>% 
  group_by(candy_name) %>% 
  filter(rating == "JOY") %>%  
  summarise(number_of_joys = n()) %>%
  slice_max(number_of_joys, n = 2)
```

<div class="kable-table">

|candy_name                 | number_of_joys|
|:--------------------------|--------------:|
|any_full_sized_candy_bar   |           7589|
|reese_s_peanut_butter_cups |           7369|

</div>
Seen as any full sized candy bar is too general, we can take reece's peanut butter cups
to be the most liked candy.

## DESPAIR

```r
candy_long %>% 
  group_by(candy_name) %>% 
  filter(rating == "DESPAIR") %>%  
  summarise(number_of_despairs = n()) %>%
  slice_max(number_of_despairs, n = 1)
```

<div class="kable-table">

|candy_name              | number_of_despairs|
|:-----------------------|------------------:|
|gum_from_baseball_cards |               7341|

</div>


## MEH

```r
candy_long %>% 
  group_by(candy_name) %>% 
  filter(rating == "MEH") %>%  
  summarise(number_of_mehs = n()) %>%
  slice_max(number_of_mehs, n = 1)
```

<div class="kable-table">

|candy_name | number_of_mehs|
|:----------|--------------:|
|lollipops  |           1570|

</div>

# 5. How many people rated Starburst as despair?


```r
candy_long %>% 
  filter(candy_name == "starburst",
         rating == "DESPAIR") %>% 
  nrow()
```

```
## [1] 1990
```

# 6. What was the most popular candy bar by this rating system for each gender in the dataset ?

## Firstly I'm going to append a point column (1 for JOY, 0 for MEH, -1 for DESPAIR) to the dataset

```r
candy_long_points <- candy_long %>% 
  mutate(point = case_when(rating == "JOY" ~ 1,
                           rating == "MEH" ~ 0,
                           rating == "DESPAIR" ~ -1,
                           TRUE ~ as.numeric(NA)))
```


```r
candy_long_points %>% 
  filter(gender != "I'd rather not say",
         is.na(gender) == FALSE,
         candy_name != "any_full_sized_candy_bar") %>%
  group_by(gender, candy_name) %>% 
  summarise(total_points = sum(point, na.rm = TRUE)) %>% 
  slice_max(total_points) %>% 
  head(3)
```

```
## `summarise()` has grouped output by 'gender'. You can override using the
## `.groups` argument.
```

<div class="kable-table">

|gender |candy_name                 | total_points|
|:------|:--------------------------|------------:|
|Female |reese_s_peanut_butter_cups |          768|
|Male   |reese_s_peanut_butter_cups |         1443|
|Other  |twix                       |           32|

</div>
For people identifying other than male or female, we have twix taking the first spot
(after any full sized bar which I filtered out as it's too general), whereas for male
and female we have Reece's peanut butter cups being the most popular

# 7. What was the most popular candy bar in each year?

I've gone by the definition of the most popular bar being the one gaining the most total points that year 

```r
candy_long_points %>% 
  filter(candy_name != "any_full_sized_candy_bar") %>% 
  group_by(timestamp, candy_name) %>% 
  summarise(total_points = sum(point, na.rm = TRUE)) %>% 
  slice_max(total_points)
```

```
## `summarise()` has grouped output by 'timestamp'. You can override using the
## `.groups` argument.
```

<div class="kable-table">

| timestamp|candy_name                 | total_points|
|---------:|:--------------------------|------------:|
|      2015|reese_s_peanut_butter_cups |         4375|
|      2016|kit_kat                    |          920|
|      2017|reese_s_peanut_butter_cups |         1403|

</div>

# 8. What was the most popular candy bar by this rating for people in US, Canada, UK, and all other countries?

Firstly changing countries not listed above to have the value "other country"

```r
candy_long_points %>% 
  filter(candy_name != "any_full_sized_candy_bar") %>% 
  mutate(country = case_when(!(country %in% c("usa", "uk", "canada")) ~ "other",
            TRUE ~ country)) %>% 
  group_by(country, candy_name) %>% 
  summarise(total_points = sum(point, na.rm = TRUE)) %>% 
  slice_max(total_points)
```

```
## `summarise()` has grouped output by 'country'. You can override using the
## `.groups` argument.
```

<div class="kable-table">

|country |candy_name                   | total_points|
|:-------|:----------------------------|------------:|
|canada  |kit_kat                      |          229|
|other   |reese_s_peanut_butter_cups   |         4470|
|uk      |lindt_truffle                |           32|
|uk      |rolos                        |           32|
|uk      |tolberone_something_or_other |           32|
|usa     |reese_s_peanut_butter_cups   |         1979|

</div>
