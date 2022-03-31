---
title: "Analysis - task_4"
output:
  html_document:
    keep_md: yes
    df_print: paged
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

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["avg_age"],"name":[1],"type":["dbl"],"align":["right"]}],"data":[{"1":"35.0904"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# 3. What was the average age of people who are not going trick or treating?


```r
candy %>% 
  filter(trick_treating == "No") %>% 
  summarise(avg_age = mean(age, na.rm = TRUE))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["avg_age"],"name":[1],"type":["dbl"],"align":["right"]}],"data":[{"1":"39.16249"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
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

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["candy_name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["number_of_joys"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"any_full_sized_candy_bar","2":"7589"},{"1":"reese_s_peanut_butter_cups","2":"7369"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
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

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["candy_name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["number_of_despairs"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"gum_from_baseball_cards","2":"7341"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


## MEH

```r
candy_long %>% 
  group_by(candy_name) %>% 
  filter(rating == "MEH") %>%  
  summarise(number_of_mehs = n()) %>%
  slice_max(number_of_mehs, n = 1)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["candy_name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["number_of_mehs"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"lollipops","2":"1570"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
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
  slice_max(total_points)
```

```
## `summarise()` has grouped output by 'gender'. You can override using the
## `.groups` argument.
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["gender"],"name":[1],"type":["chr"],"align":["left"]},{"label":["candy_name"],"name":[2],"type":["chr"],"align":["left"]},{"label":["total_points"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Female","2":"reese_s_peanut_butter_cups","3":"768"},{"1":"Male","2":"reese_s_peanut_butter_cups","3":"1443"},{"1":"Other","2":"twix","3":"32"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
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

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["timestamp"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["candy_name"],"name":[2],"type":["chr"],"align":["left"]},{"label":["total_points"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"2015","2":"reese_s_peanut_butter_cups","3":"4375"},{"1":"2016","2":"kit_kat","3":"920"},{"1":"2017","2":"reese_s_peanut_butter_cups","3":"1403"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
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

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["country"],"name":[1],"type":["chr"],"align":["left"]},{"label":["candy_name"],"name":[2],"type":["chr"],"align":["left"]},{"label":["total_points"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"canada","2":"kit_kat","3":"229"},{"1":"other","2":"reese_s_peanut_butter_cups","3":"4470"},{"1":"uk","2":"lindt_truffle","3":"32"},{"1":"uk","2":"rolos","3":"32"},{"1":"uk","2":"tolberone_something_or_other","3":"32"},{"1":"usa","2":"reese_s_peanut_butter_cups","3":"1979"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
