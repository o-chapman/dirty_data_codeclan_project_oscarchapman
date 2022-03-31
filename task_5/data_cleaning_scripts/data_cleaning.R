## Loading libraries and reading in data

library(tidyverse)

rwa_raw <- janitor::clean_names(read_csv("task_5/raw_data/rwa.csv"))


## Creating a column with an entree's RWA (Right Wing Authoritarian) score

# Inverse answers (i.e. high score implies a lower rwa score for that questions and vice versa)
inverse_questions <- c(4, 6, 8, 9, 11, 13, 15, 18, 20, 21) - 2
# -2 at the end as we omit the first two questions as they were warm-up ones

question_colnames <- colnames(rwa_raw)[3:22]

rwa_indexed_score <- rwa_raw %>% 
  mutate(normal_q_score = rowSums(across(all_of(question_colnames[-inverse_questions]))),
         inverse_q_score = 10*length(inverse_questions) - rowSums(across(all_of(inverse_questions))),
         rwa_score = (normal_q_score + inverse_q_score)/20,
         .before = 1) %>% 
  select(-normal_q_score, -inverse_q_score) %>% 
  rowid_to_column("candidate")
# Also added an index column which relates to each candidate's responses to the survey


## Taking out unneeded columns

rwa_clean <- rwa_indexed_score %>% 
  select(-starts_with(c("tipi", "vcl")), -screenh, -screenw, -surveyaccurate)


## Recoding some columns to give values more meaning

rwa_test <- rwa_clean %>% 
  mutate(gender = recode(gender, 1 = "male", 2 = "female", 3 = "other", 0 = as.character(NA)))

## Writing the clean data to csv

write_csv(rwa_indexed_score_reduced, "task_5/clean_data/rwa_clean.csv")
