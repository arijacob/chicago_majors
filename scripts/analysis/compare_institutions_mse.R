# Data from: https://nces.ed.gov/ipeds/reported-data/144050?year=2023&surveyNumber=3
library(tidyverse)
library(janitor)
library(extrafont)

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

data = read_csv("Data/Output/ivyplus_perstudent.csv")


all_institution_shares = data %>%
  group_by(year, cip_name, institution) %>%
  summarise(
    count = sum(count),
    total_count = first(total_award_year),
    .groups = "drop"
  ) %>%
  mutate(
    share = count / total_count
  )

uchicago_shares = all_institution_shares %>%
  filter(institution == "University of Chicago" & (year == 2022 | year == 2005)) %>%
  select(cip_name, share, year)

institution_mse = data.frame()

for (college in unique(data$institution)) {
  
  institution_share = all_institution_shares %>%
    filter(institution == college & year == 2022) %>%
    select(cip_name, share, year) %>%
    rename(institution_share = share) %>%
    bind_rows(
      all_institution_shares %>%
        filter(institution == college & year == 2005) %>%
        select(cip_name, share, year) %>%
        rename(institution_share = share)
    )
  
  comparison = uchicago_shares %>%
    full_join(institution_share, by = c("cip_name", "year")) %>%
    replace_na(list(share = 0, institution_share = 0)) %>%
    group_by(year) %>%
    summarise(
      mse = mean((share - institution_share)^2)
    ) 
  
  institution_mse = bind_rows(
    institution_mse,
    data.frame(
      institution = college,
      mse_2022 = comparison %>% filter(year == 2022) %>% pull(mse),
      mse_2005 = comparison %>% filter(year == 2005) %>% pull(mse)
    )
  )
  
}
