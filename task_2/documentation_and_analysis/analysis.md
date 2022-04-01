---
title: "task_2 - Analysis"
output:
  html_document:
    keep_md: yes
    df_print: kable
date: '2022-03-28'
---



# Reading in Data and Libraries


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
library(here)
```

```
## here() starts at D:/CodeClan/codeclan_homework/week_4/dirty_data_codeclan_project_oscarchapman
```

```r
cakes <- read_csv(here("task_2/clean_data/cake_clean.csv"))
```

```
## Rows: 612 Columns: 4
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (3): cake, ingredient, measure
## dbl (1): amount
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# 1. Which cake has the most cocoa in it?


```r
cakes %>%
  filter(ingredient == "cocoa") %>% 
  slice_max(amount)
```

<div class="kable-table">

|cake               |ingredient | amount|measure    |
|:------------------|:----------|------:|:----------|
|one bowl chocolate |cocoa      |     10|tablespoon |

</div>

# 2. For sponge cake, how many cups of ingredients are used in total?


```r
cakes %>% 
  filter(cake == "sponge" & measure  == "cup") %>%
  summarise(total_cups = sum(as.numeric(amount)))
```

<div class="kable-table">

| total_cups|
|----------:|
|        3.5|

</div>

# 3. How many ingredients are measured in teaspoons?


```r
cakes %>% 
  distinct(ingredient, measure) %>% 
  filter(measure == "teaspoon") %>% 
  nrow()
```

```
## [1] 8
```

# 4. Which cake has the most unique ingredients?

I have defined the most unique cake to be the one with the most unique ingredients
(a unique ingredient being one only used in one cake)


```r
cakes %>% 
  mutate(amount = na_if(amount, 0)) %>% 
  drop_na(amount) %>% 
  group_by(ingredient) %>% 
  mutate(ingredient_no = n()) %>% 
  ungroup() %>% 
  filter(ingredient_no == 1) %>% 
  group_by(cake) %>% 
  summarise(no_unique_ingredients = n()) %>% 
  slice_max(no_unique_ingredients)
```

<div class="kable-table">

|cake  | no_unique_ingredients|
|:-----|---------------------:|
|angel |                     2|

</div>

# 5. Which ingredients are used only once?


```r
cakes %>% 
  mutate(amount = na_if(amount, 0)) %>% 
  drop_na(amount) %>% 
  group_by(ingredient) %>% 
  summarise(no_occurences = n()) %>% 
  filter(no_occurences == 1)
```

<div class="kable-table">

|ingredient      | no_occurences|
|:---------------|-------------:|
|bananas         |             1|
|cream of tartar |             1|
|crushed ice     |             1|
|dried currants  |             1|
|egg white       |             1|
|nutmeg          |             1|
|nuts            |             1|
|zwiebach        |             1|

</div>

