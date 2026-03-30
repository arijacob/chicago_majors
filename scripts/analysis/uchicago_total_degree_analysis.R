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

data = read_csv("Data/Output/uchicago_perdegree.csv")

data %>%
  filter( 
    cip_name %in% c("Public Policy Analysis", "Public Policy Analysis, General") |
    grepl("Econ", cip_name)
      ) %>%
  mutate(
    classification = case_when(
      grepl("Econ", cip_name) ~ "Economics",
      TRUE ~ "Public Policy")
  ) %>%
  group_by(year, classification) %>%
  summarise(
    count = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = count / total_awards_year
  ) %>%
  ggplot(aes(x = year, y = share, color = classification)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey60") +
  geom_line() +
  geom_point() +
  theme_custom()


data %>%
  filter(grepl("Sociology", cip_name)) %>%
  group_by(year) %>%
  summarise(
    count = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = count / total_awards_year
  ) %>%
  ggplot(aes(x = year, y = share)) +
  geom_line() +
  geom_smooth() +
  theme_custom()

  

# Major Groups Overtime ---------------------------------------------------
groups_overtime = data %>%
  group_by(year, classification) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = total_awards / total_awards_year
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  geom_line(linewidth = 0.4) +
  geom_point(size = 1.5) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.4, by = 0.05)
                       ) +
  labs(
    x = NULL,
    y = "Share of toal degrees awarded",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.text = element_text(size = 12))
groups_overtime
ggsave("Figures/uchicago/per_degree/major_groups_overtime.png", groups_overtime, width = 10, height = 5)

select_groups_overtime = data %>%
  # bind_rows(data_2023_2024) %>%
  group_by(year, classification) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = total_awards / total_awards_year
  ) %>%
  filter(
    classification %in% c("Social Sciences", "Math and Allied Fields", "CS and Data Science")
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.4, by = 0.05)
  ) +
  labs(
    x = NULL,
    y = "Share of toal degrees awarded",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.text = element_text(size = 12),
        legend.position = "bottom")
select_groups_overtime
ggsave("Figures/uchicago/per_degree/select_major_groups_overtime.png", select_groups_overtime, width = 8, height = 5)


econ_vs_humanities = data %>%
  group_by(year, classification) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = total_awards / total_awards_year
  ) %>%
  filter(
    classification %in% c("Economics", "Humanities and Art")
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Economics" = "#0076bd", "Humanities and Art" = "#7bb14e")) +
  labs(
    x = NULL,
    y = "Share of toal degrees awarded",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(
    legend.position = c(0.3, 0.8),
    legend.text = element_text(size = 16)
    )
econ_vs_humanities
ggsave("Figures/uchicago/per_degree/econ_vs_humanities.png", econ_vs_humanities, width = 8, height = 5)

# Major Level Plots -------------------------------------------------------
humanities_decline = data %>%
  filter(
    grepl("History", cip_name) |
    grepl("English", cip_name)
  ) %>%
  mutate(
    major_name = case_when(
      grepl("History", cip_name) ~ "History",
      grepl("English", cip_name) ~ "English",
    ),
    major_name = factor(major_name, levels = c("History", "English"))
  ) %>%
  group_by(year, major_name) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = total_awards / total_awards_year
  ) %>%
  ggplot(aes(x = year, y = share, color = major_name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("History" = "#0076bd", "English" = "#7bb14e")) +
  labs(
    x = NULL,
    y = "Share of toal degrees awarded",
    color = NULL) +
  ylim(0.02, 0.1) +
  theme_custom() +
  theme(
    legend.position = c(0.7, 0.8),
    legend.text = element_text(size = 16)
  )
humanities_decline
ggsave("Figures/uchicago/per_degree/humanities_decline.png", humanities_decline, width = 8, height = 5)

econ_overtime = data %>%
  filter(
    grepl("Econ", cip_name)
  ) %>%
  group_by(year) %>%
  summarise(
    total_econ_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    econ_share = total_econ_awards / total_awards_year
  ) %>%
  ggplot(aes(x = year, y = econ_share)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey70") +
  annotate("text", x = 2010.5, y = 0.28, label = "Business Economics created",
           hjust = 0, vjust = -0.5, size = 5, family = "Georgia") +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  labs(
    x = NULL,
    y = "Share of toal degrees awarded"
  ) +
  ylim(0.1, 0.4) +
  theme_custom()
econ_overtime  
ggsave("Figures/uchicago/per_degree/econ_overtime.png", econ_overtime, width = 8, height = 5)

math_women = data %>%
  filter(
    grepl("Math", classification),
    gender == "Women"
  ) %>%
  group_by(year) %>%
  summarise(
    total_econ_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    econ_share = total_econ_awards / total_awards_year
  ) %>%
  ggplot(aes(x = year, y = econ_share)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  scale_color_brewer(palette = "Set2") +
  annotate("text", x = 2009, y = 0.03, label = "Women majors in mathematics",
           hjust = 0, vjust = -0.5, size = 5, family = "Georgia") +
  annotate("text", x = 2010, y = 0.028, label = "and related fields",
           hjust = 0, vjust = -0.5, size = 5, family = "Georgia") +
  labs(
    x = NULL,
    y = "Share of total degrees awarded",
    color = NULL
  ) +
  theme_custom() +
  theme(legend.position = "bottom")
math_women
ggsave("Figures/uchicago/per_degree/women_math.png", math_women, width = 8, height = 5)


anthro_overtime = data %>%
  filter(
    grepl("Anthro", cip_name)
  ) %>%
  group_by(year) %>%
  summarise(
    total_econ_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    econ_share = total_econ_awards / total_awards_year
  ) %>%
  ggplot(aes(x = year, y = econ_share)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  labs(
    x = NULL,
    y = "Share of total degrees awarded",
    title = "Anthropology"
  ) +
  ylim(0.0, 0.05) +
  theme_custom()
anthro_overtime  
ggsave("Figures/uchicago/per_degree/anthro_overtime.png", anthro_overtime, width = 8, height = 5)

change = data %>%
  filter(
    year >= 2020 | year <= 2007
  ) %>%
  group_by(year, classification) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    early = case_when(
      year <= 2007 ~ "2005-2007",
      year >= 2020 ~ "2020-2022"
    ),
    share = total_awards / total_awards_year
  ) %>%
  group_by(classification, early) %>%
  summarise(
    share = mean(share)
  ) %>%
  pivot_wider(names_from = early,
              values_from = share) %>%
  mutate(
    decrease = ifelse(`2020-2022` < `2005-2007`, "Yes", "No")
  ) %>%
  pivot_longer(cols = c(`2005-2007`, `2020-2022`),
               names_to = "early",
               values_to = "share"
               ) %>%
  mutate(
    classification = factor(classification,
                            levels = c(
                              "Area Studies and Languages", "CS and Data Science", "Math and Allied Fields",
                              "Humanities and Art", "STEM", "Economics", "Social Sciences"
                            ))
  )
                            
                            
ggplot(change, aes(y = classification,
                   x = share,
                   group = classification
                   )) +
  geom_line(linetype = "dashed", size = 0.7, aes(color = decrease)) +
  geom_point(size = 2.5, aes(color = early)) +
  scale_color_manual(values = c("2005-2007" = "grey35", "2020-2022" = "#7bb14e",
                                "Yes" = "red", "No" = "grey")) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_custom() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.5, by = 0.05)
                       ) +
  theme(
    legend.position = c(0.8, 0.3),
    legend.text = element_text(size = 16)
  ) 
ggsave("Figures/uchicago/per_degree/change_2007_2020.png", width = 9, height = 5)
