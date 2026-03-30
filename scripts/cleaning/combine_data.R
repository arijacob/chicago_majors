library(tidyverse)
library(janitor)
library(readxl)

rm(list = ls())

setwd("/Users/arijacob/Documents/Maroon Article")

file_list = list.files(path = "Data/Cleaned", pattern = "*.csv", full.names = TRUE)
# remote the one with 2024
file_list = file_list[!str_detect(file_list, "2024")]

data_list = lapply(file_list, read_csv)

# across all data frames, make all columns characters
data_list = lapply(data_list, function(df) {
  df %>% mutate(across(everything(), as.character)) 
})



combined_data = bind_rows(data_list) %>%
  mutate(
    award_level = coalesce(award, award_level),
    major = if_else(is.na(major), "all", major)
  ) %>%
  select(-group, -award) %>%
  mutate(
    race = case_when(
      race %in% c("non_resident_alien", "u_s_nonresident", "nonresident_alien") ~ "International",
      race %in% c("race_ethnicity_unknown", "race_unknown") ~ "Unknown",
      race %in% c("black_non_hispanic", "black_or_african_american") ~ "Black",
      race %in% c("white_non_hispanic", "white") ~ "White",
      race %in% c("hispanic", "hispanic_latino") ~ "Hispanic",
      race %in% c("native_hawaiian_or_other_pacific_islander", 
                  "asian_or_pacific_islander", "asian", "native_hawaiian_or_other_pacific_islander") ~ "AAPI",
      race %in% c("american_indian_or_alaska_native", "american_indian_or_alaskan_native") ~ "American Indian or Alaskan Native",
      TRUE ~ race
    ),
    gender = case_when(
      gender %in% c("Men", "Men ") ~ "Men",
      gender %in% c("Women", "Women ") ~ "Women",
      gender %in% c("Total", "Total ") ~ "Total",
      TRUE ~ gender
    ),
    cip_name = trimws(cip_name),
    award_level = case_when(
      award_level %in% c("5 ", "Bachelor's degree", "Bachelors degrees ") ~ "Bachelor's degree",
      award_level %in% c("7 ", "Masters degrees ", "12 ", "Master's degrees") ~ "Master's degree",
      award_level %in% c("15 ", "Doctors degrees ", "Doctor's degree  research / scholarship", "") ~ "Doctor's degree",
      award_level %in% c("9 ", "10 ") ~ "Other",
      TRUE ~ award_level
    ),
    cip_code_broad = case_when(
        substr(cip_code, 2, 2) == "." ~ paste0("0", substr(cip_code, 1, 1)),
        TRUE ~ substr(cip_code, 1, 2)
      ),
    count = str_replace(count, " ", ""),
    count_test = as.numeric(count),
    major = case_when(
      major %in% c("1st", "1") ~ 1,
      major %in% c("2nd", "2") ~ 2,
      major == "all" ~ NA
    )
  ) %>%
  filter(gender != "Total", race != "total") %>%
  drop_na(gender) 
write_csv(combined_data, "Data/2023_2000_major_gender_race.csv")
