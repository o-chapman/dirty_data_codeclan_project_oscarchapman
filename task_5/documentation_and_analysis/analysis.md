---
title: "Analysis - task_5"
output:
  html_document:
    keep_md: yes
    df_print: kable
date: '2022-03-31'
---



## Reading in data and loading libraries


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
rwa <- read_csv(here("task_5/clean_data/rwa_clean.csv"))
```

```
## Rows: 9881 Columns: 64
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (7): ip_country, education, urban, gender, hand, major, age_cat
## dbl (57): candidate, rwa_score, q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# 1. What’s the average RWA score for each gender?


```r
rwa %>% 
  group_by(gender) %>% 
  summarise(avg_rwa_score = mean(rwa_score))
```

<div class="kable-table">

|gender | avg_rwa_score|
|:------|-------------:|
|female |      3.500905|
|male   |      3.887854|
|other  |      3.157436|
|NA     |      3.684210|

</div>

# 2. What’s the average RWA score for left handed people vs. right handed people.


```r
rwa %>% 
  group_by(hand) %>% 
  summarise(avg_rwa_score = mean(rwa_score), number_of_people = n())
```

<div class="kable-table">

|hand  | avg_rwa_score| number_of_people|
|:-----|-------------:|----------------:|
|both  |      3.933828|              303|
|left  |      3.731101|             1172|
|right |      3.731706|             8358|
|NA    |      3.947917|               48|

</div>

# 3. What’s the average family size for each type of childhood?


```r
rwa %>% 
  group_by(urban) %>% 
  summarise(avg_family_size = mean(familysize))
```

<div class="kable-table">

|urban    | avg_family_size|
|:--------|---------------:|
|rural    |        2.792988|
|suburban |        2.622197|
|urban    |        2.497587|
|NA       |        2.548673|

</div>


# 4. What’s the average time to take the test for each education level?


```r
rwa %>% 
  group_by(education) %>% 
  summarise(avg_test_time = mean(testelapse))
```

<div class="kable-table">

|education             | avg_test_time|
|:---------------------|-------------:|
|graduate degree       |      330.0297|
|high schol            |      512.4967|
|less than high school |      380.4805|
|university degree     |      623.3965|
|NA                    |     5294.5980|

</div>


# 5. Create a plot of results of question 4.




# 6. What’s the average RWA score for people based on age


```r
rwa %>% 
  group_by(age_cat) %>% 
  summarise(avg_rwa_score = mean(rwa_score), number_of_people = n()) %>% 
  arrange(avg_rwa_score)
```

<div class="kable-table">

|age_cat  | avg_rwa_score| number_of_people|
|:--------|-------------:|----------------:|
|Over 60  |      3.470048|              631|
|41-60    |      3.548293|             1963|
|26-40    |      3.582254|             2702|
|18-25    |      3.903232|             2939|
|Under 18 |      4.032898|             1646|

</div>

