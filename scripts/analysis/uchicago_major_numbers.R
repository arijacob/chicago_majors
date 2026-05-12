# Data from: https://nces.ed.gov/ipeds/reported-data/144050?year=2023&surveyNumber=3
library(tidyverse)
library(janitor)
library(extrafont)
library(readxl)

rm(list = ls())

theme_custom <- function(base_size = 19, base_family = "Georgia") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "solid", color = "grey90",
                                        linewidth = .25),
      panel.grid.minor = element_blank(),
      
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.1, "cm"),
      
      # Add axis lines
      axis.line = element_line(color = "black", linewidth = 0.3),
      
      title = element_text(size = 18),
      
      legend.text = element_text(size = 16),
      legend.key.size  = unit(1.2, "cm")
    )
}

setwd("/Users/arijacob/Documents/GitHub/chicago_majors")


majors = read_xlsx("data/classifications/chicago_majors_cip_crosswalk.xlsx") %>%
  select(1:3) %>%
  filter(
    !(chicago_major %in% c(
      "Applied Math",
      "Environmental/Urban Studies",
      "Computational and Applied Math",
      "Philosophy and Allied Fields"
    )
    )
  ) %>%
  drop_na() 

data = read_csv("data/ipeds/cleaned/major_numbers.csv") %>%
  filter(instnm == "University of Chicago") %>%
  group_by(instnm, year) %>%
  mutate(
    total_students = sum(total[major_number == 1]),
    total_degrees = sum(total)
  ) %>%
  ungroup() %>%
  left_join(
    majors, by = c('cipcode' = "cip_code")
  ) %>%
  select(
    year, total_students, total, chicago_major, division
  ) %>%
  group_by(chicago_major, year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  ) %>%
  ungroup() %>%
  mutate(
    major = ifelse(is.na(chicago_major), "Other", chicago_major)
  ) %>%
  select(major, year, share_students)
write_csv(data, "public/data/major_shares.csv")
