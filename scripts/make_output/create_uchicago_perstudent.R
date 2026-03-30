library(tidyverse)
library(janitor)

rm(list = ls())

setwd("/Users/arijacob/Documents/Maroon Article")

classification = read_csv("Data/major_classification.csv") %>%
  clean_names() %>%
  mutate(
    major = str_replace_all(major, " ", " ")
  ) %>%
  rename(cip_name = major)

awards_by_year = read_csv("Data/IvyPlus/awards_by_year.csv") %>%
  filter(institution == "University of Chicago")

data = read_csv("Data/2023_2000_major_gender_race.csv") %>%
  mutate(
    year = str_sub(year, 1, 4),
    year = as.numeric(year),
    cip_name = str_replace_all(cip_name, " ", "")
  ) %>% 
  filter(
    award_level == "Bachelor's degree"
  ) %>%
  group_by(year) %>%
  ungroup() %>%
  left_join(
    classification
  ) %>%
  mutate(
    classification = factor(
      classification,
      levels = c(
        "Social Sciences",
        "Economics",
        "STEM",
        "Humanities and Art",
        "Math and Allied Fields",
        "CS and Data Science",
        "Area Studies and Languages"))
  ) %>%
  left_join(
    awards_by_year, by = "year"
  ) %>%
  drop_na(total_award_year)
write_csv(data, "Data/Output/uchicago_perstudent.csv")  
