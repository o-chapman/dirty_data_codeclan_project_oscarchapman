---
title: "task_1 - Analysis"
output:
  html_document:
    keep_md: yes
    df_print: kable
date: '2022-03-28'
---



# Load in the libraries and clean data


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
decathlon <- read_csv(here("task_1/clean_data/decathlon_clean.csv"))
```

```
## Rows: 41 Columns: 14
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (2): name, competition
## dbl (12): x100m, long_jump, shot_put, high_jump, x400m, x110m_hurdle, discus...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# 1. Who had the longest long jump seen in the data?


```r
decathlon %>% 
  slice_max(long_jump)
```

<div class="kable-table">

|name | x100m| long_jump| shot_put| high_jump| x400m| x110m_hurdle| discus| pole_vault| javeline| x1500m| rank| points|competition |
|:----|-----:|---------:|--------:|---------:|-----:|------------:|------:|----------:|--------:|------:|----:|------:|:-----------|
|clay | 10.44|      7.96|    15.23|      2.06| 49.19|        14.13|  50.11|        4.9|    69.71|    282|    2|   8820|OlympicG    |

</div>

# 2. What was the average 100m time in each competition?


```r
decathlon %>% 
  group_by(competition) %>% 
  summarise(avg_100m_time = mean(x100m))
```

<div class="kable-table">

|competition | avg_100m_time|
|:-----------|-------------:|
|Decastar    |      11.17538|
|OlympicG    |      10.91571|

</div>

# 3. Who had the highest total points across both competitions?


```r
decathlon %>% 
  group_by(name) %>% 
  summarise(total_points = sum(points)) %>% 
  slice_max(total_points)
```

<div class="kable-table">

|name   | total_points|
|:------|------------:|
|sebrle |        17110|

</div>

# 4. What was the shot-put scores for the top three competitors in each competition?


```r
decathlon %>%
  group_by(competition) %>% 
  slice_max(shot_put, n = 3) %>% 
  select(competition, name, shot_put)
```

<div class="kable-table">

|competition |name   | shot_put|
|:-----------|:------|--------:|
|Decastar    |yurkov |    15.19|
|Decastar    |sebrle |    14.83|
|Decastar    |karpov |    14.77|
|OlympicG    |sebrle |    16.36|
|OlympicG    |karpov |    15.93|
|OlympicG    |macey  |    15.73|

</div>

# 5. What was the average points for competitors who ran the 400m in less than 50 seconds vs. those than ran 400m in more than 50 seconds?

## We have some competitors who have run both under 50s and over 50s but we can just include them in both under 50s and over

## Less than 50s


```r
decathlon %>% 
  filter(x400m < 50) %>% 
  group_by(name) %>% 
  summarise(competitor_points = mean(points)) %>% 
  summarise(overall_point_avg = mean(competitor_points))
```

<div class="kable-table">

| overall_point_avg|
|-----------------:|
|          8083.214|

</div>

## More than 50s


```r
decathlon %>% 
  filter(x400m > 50) %>% 
  group_by(name) %>% 
  summarise(competitor_points = mean(points)) %>% 
  summarise(overall_point_avg = mean(competitor_points))
```

<div class="kable-table">

| overall_point_avg|
|-----------------:|
|          7727.167|

</div>
