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

loadfonts(device = "pdf")

setwd("/Users/arijacob/Documents/GitHub/chicago_majors")

classifications = read_xlsx("data/classifications/final_classification.xlsx") %>%
  mutate(
    cip_code = as.character(cip_code)
  )

data = read_csv("data/ipeds/cleaned/major_numbers.csv") %>%
  left_join(classifications, by = c("cipcode" = "cip_code")) %>%
  group_by(instnm, year) %>%
  mutate(
    total_students = sum(total[major_number == 1]),
    total_degrees = sum(total)
  ) %>%
  ungroup()


class_naas = read_xlsx("data/classifications/Degree_Program_Code_Catalog.xlsx", sheet = 2, skip = 3) %>%
  inner_join(
    data %>% filter(is.na(major_name)) %>% distinct(cipcode),
    by = c("Code" = "cipcode")
  ) %>%
  select(1, 2)

uchicago_econ_2024 = 0.41

share_humanities = data %>%
  filter(
    classification == "Humanities and Arts"
  ) %>%
  group_by(instnm, year) %>%
  summarise(
    share_students = sum(total) / first(total_students),
    share_degrees = sum(total) / first(total_degrees),
    total_students = first(total_students)
  ) %>%
  mutate(
    is_uchicago = ifelse(instnm == "University of Chicago", "University of Chicago", "Other Ivy Plus")
  ) %>%
  mutate(
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    share_students = weighted.mean(share_students, total_students)
  ) %>%
  drop_na()

share_humanities %>%
  ggplot(aes(x = year, y = share_students, color = factor(is_uchicago)))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of students",
    title  = "Humanities and Arts",
    color = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy Plus" = "#737373")) +
  theme_custom() +
  #ylim(0.05, 0.2) +
  theme(legend.position = c(0.7, 0.85),
        legend.text = element_text(size = 16))
ggsave("output/ivyplus/per_student/share_humanities_arts.png", width = 7.5, height = 4)
write_csv(share_humanities, "output/ivyplus/per_student/share_humanities_arts.csv")


share_econ = data %>%
  filter(classification == "Business and Economics") %>%
  group_by(instnm, year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students),
    share_degrees = sum(total) / first(total_degrees),
    total_students = first(total_students)
  ) %>%
  mutate(
    is_uchicago = ifelse(instnm == "University of Chicago", "University of Chicago", "Other Ivy Plus")
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    share_students = weighted.mean(share_students, total_students)
  ) %>%
  drop_na() %>%
  bind_rows(
    tibble(
      is_uchicago = "University of Chicago",
      year = 2025,
      share_students = uchicago_econ_2024
    )
  ) %>%
  mutate(
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy Plus"))
  )

ggplot(share_econ, aes(x = year, y = share_students, color = factor(is_uchicago)))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of students",
    title  = "Economics and Business Majors",
    color = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy Plus" = "#737373")) +
  theme_custom() +
  theme(legend.position = c(0.3, 0.86),
        legend.text = element_text(size = 16))
ggsave("output/ivyplus/per_student/share_econ_business.png", width = 7.5, height = 4)
write_csv(share_econ, "output/ivyplus/per_student/share_econ_business.csv")

# social_sciences_cip = c(42, 45, 44, 22, 30.05, 30.23, 30.17, 30.28)

share_ss = data %>%
  filter(classification == "Other Social Sciences") %>%
  group_by(instnm, year) %>%
  summarise(
    share_students = sum(total) / first(total_students),
    share_degrees = sum(total) / first(total_degrees),
    total_students = first(total_students),
    total_awards = first(total_degrees)
  ) %>%
  mutate(
    is_uchicago = ifelse(instnm == "University of Chicago", "University of Chicago", "Other Ivy Plus")
  ) %>%
  mutate(
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    share_students = weighted.mean(share_students, total_students)
  ) %>%
  drop_na()

ggplot(share_ss, aes(x = year, y = share_students, color = factor(is_uchicago), shape = is_uchicago))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of students",
    title  = "Other social science majors",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy Plus" = "#737373")) +
  theme_custom() +
  ylim(0.15, 0.4) +
  theme(legend.position = c(0.8, 0.9),
        legend.text = element_text(size = 13))
ggsave("output/ivyplus/per_student/share_social_sciences.png", width = 7.5, height = 4)
write_csv(share_ss, "output/ivyplus/per_student/share_social_sciences.csv")


share_ss_hum = data %>%
  filter(classification %in% c("Other Social Sciences", "Humanities and Arts")) %>%
  group_by(instnm, year) %>%
  summarise(
    share_students = sum(total) / first(total_students),
    share_degrees = sum(total) / first(total_degrees),
    total_students = first(total_students),
    total_awards = first(total_degrees)
  ) %>%
  mutate(
    is_uchicago = ifelse(instnm == "University of Chicago", "University of Chicago", "Other Ivy Plus")
  ) %>%
  mutate(
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    share_students = weighted.mean(share_students, total_students),
    share_degrees = weighted.mean(share_degrees, total_awards)
  ) %>%
  drop_na() %>%
  pivot_longer(cols = c(share_students, share_degrees), names_to = "metric", values_to = "value") %>%
  mutate(
    metric = case_when(
      metric == "share_students" ~ "Share of students",
      metric == "share_degrees" ~ "Share of degrees"
    ),
    metric = factor(metric, levels = c("Share of students", "Share of degrees"))
  )

ggplot(share_ss_hum, aes(x = year, y = value, color = factor(is_uchicago), shape = is_uchicago))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "",
    title  = "Social science, humanities, and arts majors",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy Plus" = "#737373")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~metric, scale = "free_y") +
  theme_custom() +
  theme(legend.position = "bottom")
ggsave("output/ivyplus/per_student/share_social_sciences_humanities.png", width = 10, height = 5)
write_csv(share_ss_hum, "output/ivyplus/per_student/share_social_sciences_humanities.csv")

share_humanities_degrees = data %>%
  filter(classification == "Humanities and Arts") %>%
  group_by(instnm, year, classification) %>%
  summarise(
    share_degrees = sum(total) / first(total_degrees),
    total_degrees = first(total_degrees)
  ) %>%
  mutate(
    is_uchicago = ifelse(instnm == "University of Chicago", "University of Chicago", "Other Ivy Plus")
  ) %>%
  mutate(
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    share_degrees = weighted.mean(share_degrees, total_degrees)
  ) %>%
  drop_na()

ggplot(share_humanities_degrees, aes(x = year, y = share_degrees, color = factor(is_uchicago)))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of degrees",
    title  = "Humanities and Arts Majors",
    color = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy Plus" = "#737373")) +
  theme_custom() +
  ylim(0.075, 0.3) +
  theme(legend.position = c(0.7, 0.85),
        legend.text = element_text(size = 16))
ggsave("output/ivyplus/per_degree/share_humanities_arts.png", width = 7.5, height = 4)
write_csv(share_humanities_degrees, "output/ivyplus/per_degree/share_humanities_arts.csv")


share_ss_degrees = data %>%
  filter(classification == "Other Social Sciences") %>%
  group_by(instnm, year) %>%
  summarise(
    share_degrees = sum(total) / first(total_degrees),
    total_degrees = first(total_degrees)
  ) %>%
  mutate(
    is_uchicago = ifelse(instnm == "University of Chicago", "University of Chicago", "Other Ivy Plus")
  ) %>%
  mutate(
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    share_degrees = weighted.mean(share_degrees, total_degrees)
  ) %>%
  drop_na()

ggplot(share_ss_degrees, aes(x = year, y = share_degrees, color = factor(is_uchicago)))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of degrees",
    title  = "Other Social Science Majors",
    color = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy Plus" = "#737373")) +
  theme_custom() +
  ylim(0.10, 0.375) +
  theme(legend.position = c(0.7, 0.85),
        legend.text = element_text(size = 16))
ggsave("output/ivyplus/per_degree/share_social_sciences.png", width = 7.5, height = 4)
write_csv(share_ss_degrees, "output/ivyplus/per_degree/share_social_sciences.csv")


wharton_uchicago = data %>%
  filter(
    instnm %in% c("University of Chicago", "University of Pennsylvania")
  ) %>%
  filter(
   classification == "Business and Economics"
  ) %>%
  group_by(instnm, year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  ) %>%
  bind_rows(
    data.frame(instnm = "University of Chicago",
    year = 2025,
    share_students = uchicago_econ_2024
    )
  ) %>%
  mutate(
    instnm = factor(instnm, levels = c("University of Pennsylvania", "University of Chicago")),
    year = year - 1
  )
  
ggplot(wharton_uchicago, aes(x = year, y = share_students, color = instnm, shape = instnm)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey") +
  annotate("text", x = 2010.25, y = 0.425, label = "Business Economics created",
           hjust = 0, vjust = -0.5, size = 4, family = "Georgia") +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of students",
    title  = "Business and economics majors",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(
    values = c("University of Chicago" = "#800000",
                                "University of Pennsylvania" = "#011F5B")
  ) +
  scale_x_continuous(breaks = seq(2005, 2024, 3)) +
  ylim(0.15, 0.45) +
  theme_custom() +
  theme(legend.position = c(0.3, 0.5),
        legend.text = element_text(size = 10))
ggsave("output/ivyplus/per_student/wharton_uchicago.png", width = 7.5, height = 4)
write_csv(wharton_uchicago, "output/ivyplus/per_student/wharton_uchicago.csv")
