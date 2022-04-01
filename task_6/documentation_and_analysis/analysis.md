---
title: "Analysis - task_6"
output:
  html_document:
    keep_md: yes
    df_print: kable
date: '2022-03-31'
---



# Loading libraries and reading data


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
dogs <- read_csv(here("task_4/clean_data/dogs_clean.csv"))
```

```
## Rows: 300 Columns: 9
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (6): title, first_name, last_name, email, dog_size, dog_gender
## dbl (3): id, amount_spent_on_dog_food, dog_age
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# The client only counts a valid email address as one ending in ‘.com’. How many survey results have a valid email address.


```r
dogs %>% 
  filter(str_detect(email, ".com$")) %>% 
  nrow()
```

```
## [1] 181
```


# What’s the average amount spent on dog food for each dog size.


```r
dogs %>% 
  filter(!is.na(dog_size)) %>% 
  group_by(dog_size) %>% 
  summarise(avg_amount = mean(amount_spent_on_dog_food, na.rm = TRUE))
```

<div class="kable-table">

|dog_size | avg_amount|
|:--------|----------:|
|L        |   50.73171|
|M        |   46.34286|
|S        |   50.06383|
|XL       |   50.66935|
|XS       |   56.92593|

</div>


# For owners whose surname starts with a letter in the second half of the alphabet (N onwards) what is the average age of their dog?


```r
dogs %>% 
  filter(str_detect(last_name, "^[N-Z]")) %>% 
  summarise(avg_age = mean(dog_age, na.rm = TRUE))
```

<div class="kable-table">

|  avg_age|
|--------:|
| 52.94737|

</div>


# The dog_age column is the age in dog years. If the conversion is 1 human year = 6 dog years, then what is the average human age for dogs of each gender?


```r
dogs %>% 
  filter(!is.na(dog_gender)) %>% 
  group_by(dog_gender) %>% 
  summarise(avg_human_age = mean(dog_age, na.rm = TRUE)/6)
```

<div class="kable-table">

|dog_gender | avg_human_age|
|:----------|-------------:|
|F          |      7.911836|
|M          |      8.392086|

</div>


# Create a plot of results of question 4.
