# Data from: https://nces.ed.gov/ipeds/reported-data/144050?year=2023&surveyNumber=3
library(tidyverse)
library(janitor)
library(extrafont)
library(fixest)
library(broom)

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
      
      legend.text = element_text(size = base_size * 1.2),
      legend.key.size  = unit(1.2, "cm")
    )
}

loadfonts(device = "pdf")

setwd("/Users/arijacob/Documents/Maroon Article")

data = read_csv("Data/Output/uchicago_perstudent.csv") %>%
  mutate(
    share = count / total_award_year,
    race = if_else(
      race %in% c("Other", "American Indian or Alaskan Native", "two_or_more_races", "Unknown"), "Other", race))
  )

model = feols(share ~ year^2*classification*gender, data = data)
summary(model)


data %>%
  filter(
    #!(race %in% c("White", "Hispanic")),
         classification == "Economics"
    ) %>%
  group_by(year, race) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_award_year)
  ) %>%
  mutate(
    share = total_awards / total_awards_year
  ) %>%
  group_by(race) %>%
  mutate(
    share_rel = share / first(share)
  ) %>%
  ggplot(aes(x = year, y = share, color = race)) +
  geom_point() +
  geom_line() +
  theme_custom()

gender_overtime = data %>%
  group_by(year, gender, classification) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  ungroup() %>%
  mutate(
    share = total_awards / total_awards_year
  )

gender_overtime %>%
  ggplot(aes(x = year, y = share, color = classification)) +
  geom_point() +
  geom_line() +
  facet_wrap(~gender) +
  labs(
    color = NULL,
    x = "Year",
    y = "Share of students"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_custom()

gender_change = gender_overtime %>%
  filter(year <= 2002 | year >= 2020) %>%
  mutate(
    early = if_else(year <= 2002, "2000-2002", "2020-2023")
  ) %>%
  group_by(early, classification, gender) %>%
  summarise(
    share = mean(share)
  ) %>%
  pivot_wider(
    names_from = early,
    values_from = share
  ) %>%
  mutate(
    change = (`2020-2023` - `2000-2002`) / `2000-2002`
  )


minority_overtime = data %>%
  group_by(year, race) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  ungroup() %>%
  mutate(
    share_total_awards = total_awards / total_awards_year 
  ) %>%
  mutate(
    race = if_else(race %in% c("Unknown", "two_or_more_races"), "Other", race)
  )

minority_overtime %>%
  filter(race != "Other", race != "American Indian or Alaskan Native") %>%
  ggplot(aes(x = year, y = share_total_awards, color = race)) +
  geom_point() +
  geom_line() +
  labs(
    color = NULL,
    x = "Year",
    y = "Share of students"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_custom()

without_minorities = data %>%
  mutate(
    minority = if_else(race %in% c("Black", "Hispanic", "Unknown",
                           "American Indian or Alaskan Native", "two_or_more_races"),
                       1, 0)
  ) %>%
  filter(minority == 0) %>%
  group_by(year) %>%
  mutate(
    total_awards_year = sum(count)
  ) %>%
  group_by(year, classification) %>%
  mutate(
    total_awards = sum(count)
  ) %>%
  ungroup() %>%
  mutate(
    share_total_awards = total_awards / total_awards_year
  )

without_minorities %>%
  ggplot(aes(x = year, y = share_total_awards, color = classification)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  theme_custom()




test = data %>%
  drop_na(major) %>%
  group_by(year) %>%
  mutate(
    number_students = sum(count[major == 1])
  ) %>%
  group_by(year, classification) %>%
  summarise(
    total_awards = sum(count[major == 1]),
    number_students = first(number_students)
  ) %>%
  ungroup() %>%
  mutate(
    share_total_awards = total_awards / number_students
  )

test %>%
  ggplot(aes(x = year, y = share_total_awards, color = classification)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  theme_custom()



