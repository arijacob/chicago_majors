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


classification = read_csv("Data/major_classification.csv") %>%
  clean_names() %>%
  mutate(
    major = str_replace_all(major, " ", " ")
  ) %>%
  rename(cip_name = major)


df = read_csv("Data/uchicago_econ_degrees.csv") %>%
  clean_names() %>%
  filter(term == "Total") %>%
  select(-term, -total) %>%
  mutate(
    year = as.integer(substr(year, 1, 4))
      ) %>%
  filter(year < 2025) %>%
  pivot_longer(
    cols = -year,
    names_to = "track",
    values_to = "count"
  ) %>%
  group_by(year) %>%
  mutate(
    share = count / sum(count)
  ) %>%
  mutate(
    track = case_when(
      track == "bus_econ" ~ "Business Economics",
      track == "standard" ~ "Standard & DS",
      track == "data_sci" ~ "Standard & DS",
      TRUE ~ track
    )
  ) %>%
  group_by(year, track) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) 



total_econ_counts = read_csv("Data/2023_2000_major_gender_race.csv") %>%
  mutate(
    year = str_sub(year, 1, 4),
    year = as.numeric(year),
    cip_name = str_replace_all(cip_name, " ", "")
  ) %>% 
  filter(
    award_level == "Bachelor's degree"
  ) %>%
  group_by(year) %>%
  mutate(
    total_awards_year = sum(count)
  ) %>%
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
  group_by(year) %>%
  mutate(
    total_awards = sum(count)
  ) %>%
  filter(
    classification == "Economics"
  ) %>%
  group_by(year, classification) %>%
  summarise(
    count = sum(count),
    total_awards_year = first(total_awards_year)
  )

total_awards_df = total_econ_counts %>%
  select(year, total_awards_year)

total_econ_df = total_econ_counts %>%
  select(year, classification, count) %>%
  filter(year < 2018) %>%
  rename(
    track = classification
  )


df_econ = bind_rows(total_econ_df, df) %>%
  left_join(total_awards_df, by = "year") %>%
  pivot_wider(
    names_from = track,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(
    total_econ = `Business Economics` + Economics + `Standard & DS`,
    total_econ_without_bizecon = if_else(year < 2017, NA_real_, `Standard & DS` + `Economics`)
  ) %>%
  drop_na(total_awards_year) %>%
  select(1, 2, 6, 7) %>%
  pivot_longer(
    cols = -c(year, total_awards_year),
    names_to = "track",
    values_to = "count"
  ) %>%
  mutate(
    share = count / total_awards_year,
    track = case_when(
      track == "total_econ" ~ "Economics",
      track == "total_econ_without_bizecon" ~ "Without Business Economics",
      TRUE ~ track
    )
  )


ggplot(df_econ, aes(x = year, y = share)) +
 # geom_vline(xintercept = 2017, linetype = "dashed", color = "grey50") +
  
  # Draw maroon (Without Business Economics) first
  geom_line(data = subset(df_econ, track == "Without Business Economics"),
            aes(color = track, linetype = track)) +
  geom_point(data = subset(df_econ, track == "Without Business Economics"),
             aes(color = track, linetype = track)) +
  
  # Then draw black (Economics) on top
  geom_line(data = subset(df_econ, track == "Economics"),
            aes(color = track, linetype = track)) +
  geom_point(data = subset(df_econ, track == "Economics"),
             aes(color = track, linetype = track)) +
  
  labs(x = "Year",
       y = "Share of total degrees awarded",
       color = NULL) +
  annotate("text", x = 2016, y = 0.14, label = "Without Business", color = "darkred", size = 4.5, family = "Georgia") +
  annotate("text", x = 2016, y = 0.13, label = "Economics", color = "darkred", size = 4.5, family = "Georgia") +
  scale_color_manual(values = c("Economics" = "black", "Without Business Economics" = "darkred")) +
  theme_custom() +
  ylim(0.1, 0.3) +
  theme(legend.position = "none")
ggsave("Figures/uchicago/with_double_major/econ_by_track.png", width = 8, height = 5)

