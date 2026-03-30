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

data = read_csv("Data/Output/uchicago_perstudent.csv") %>%
  rename(total_awards_year = total_award_year)


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
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.text = element_text(size = 12))
groups_overtime
ggsave("Figures/uchicago/per_student/major_groups_overtime_uchicago.png", groups_overtime, width = 9, height = 5)

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
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.text = element_text(size = 12),
        legend.position = "bottom")
select_groups_overtime
ggsave("Figures/uchicago/per_student/select_major_groups_overtime.png", select_groups_overtime, width = 8, height = 5)

data %>% 
  filter(classification == "Math and Allied Fields") %>%
  distinct(cip_name)


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
  bind_rows(
    data.table(
      year = c(2023, 2024),
      classification = c("Economics", "Economics"),
      total_awards = c(0, 0),
      total_awards_year = c(0, 0),
      share = c(0.368, 0.41)
    )
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Economics" = "#0076bd", "Humanities and Art" = "#7bb14e")) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(
    legend.position = c(0.3, 0.8),
    legend.text = element_text(size = 16)
  ) +
  ylim(0.1, 0.45)
econ_vs_humanities
ggsave("Figures/uchicago/per_student/econ_vs_humanities.png", econ_vs_humanities, width = 8, height = 5)


groups_overtime_gender = data %>%
  filter(gender != "all") %>%
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
  group_by(year, classification, gender) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = total_awards / total_awards_year
  ) %>%
  group_by(classification, gender) %>%
  mutate(
    share_rel_beginning = share / first(share)
  ) %>%
  #filter(classification != "CS and Data Science" & classification != "Math and Allied Fields") %>%
  ggplot(aes(x = year, y = share, color = classification)) +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Degrees awarded (as share of total)",
    color = NULL) +
  facet_wrap(~gender) +
  theme_custom() +
  theme(legend.text = element_text(size = 12))
groups_overtime_gender


# Major Level Plots -------------------------------------------------------
humanities_decline = data %>%
  filter(
    (grepl("Philosophy", cip_name) |
      grepl("English", cip_name) | 
      grepl("History", cip_name)) & 
      cip_name != "History and Philosophy of Science and Technology" &
      cip_name != "Art History, Criticism and Conservation"
  ) %>%
  mutate(
    major_name = case_when(
      grepl("Philosophy", cip_name) ~ "Philosophy",
      grepl("English", cip_name) ~ "English",
      grepl("History, General", cip_name) ~ "History"
    ),
    major_name = factor(major_name, levels = c("English" ,"History",  "Philosophy"))
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
  scale_color_manual(values = c("Philosophy" = "#0076bd", "English" = "#7bb14e", "History" = "purple")) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL) +
  ylim(0.0, 0.1) +
  theme_custom() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.text = element_text(size = 16)
  )
humanities_decline
ggsave("Figures/uchicago/per_student/humanities_decline.png", humanities_decline, width = 8, height = 5)

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
    y = "Share of students"
  ) +
  theme_custom()
econ_overtime  
ggsave("Figures/uchicago/per_student/econ_overtime.png", econ_overtime, width = 8, height = 5)

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
ggsave("Figures/uchicago/per_student/women_math.png", math_women, width = 8, height = 5)


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
ggsave("Figures/uchicago/per_student/anthro_overtime.png", anthro_overtime, width = 8, height = 5)


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
  )

%>%
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


ggplot(change, aes(
  x = classification,
  y = share,
  fill = early
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.75) +
  scale_fill_manual(values = c(
    "2005-2007" = "grey35",
    "2020-2022" = "#7bb14e"
  )) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_custom() +
  theme(
    legend.position = c(0.15, 0.8),
    legend.text = element_text(size = 16)
  )
ggsave("Figures/uchicago/per_student/change_2007_2020.png", width = 9, height = 5)