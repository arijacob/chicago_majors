# Data from: https://nces.ed.gov/ipeds/reported-data/144050?year=2023&surveyNumber=3
library(tidyverse)
library(janitor)
library(extrafont)
library(readxl)

rm(list = ls())

theme_custom <- function(base_size = 17, base_family = "Georgia") {
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

setwd("/Users/arijacob/Documents/GitHub/chicago_majors")

classifications = read_xlsx("data/classifications/final_classification.xlsx") %>%
  mutate(
    cip_code = as.character(cip_code)
  ) %>%
  mutate(
    classification = if_else(
      classification == "Business and Economics", "Economics", classification
    )
 ) 

data = read_csv("data/ipeds/cleaned/major_numbers.csv") %>%
  filter(instnm == "University of Chicago") %>%
  left_join(classifications, by = c("cipcode" = "cip_code")) %>%
  group_by(instnm, year) %>%
  mutate(
    total_students = sum(total[major_number == 1]),
    total_degrees = sum(total)
  ) %>%
  ungroup()

uchicago_econ_2024 = 0.41

humanities_econ = data %>%
  filter(
    classification %in% c("Humanities and Arts", "Economics")
  )  %>%
  group_by(year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students),
  ) %>%
  bind_rows(
    data.frame(
      year = c(2025),
      classification = c("Economics"),
      share_students = c(uchicago_econ_2024)
    )
  )

ggplot(humanities_econ, aes(x = year, y = share_students, color = classification, shape = classification)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Economics" = "#0076bd", "Humanities and Arts" = "#7bb14e")) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  ylim(0.1, 0.45) +
  theme(
    legend.position = c(0.3, 0.72),
    legend.text = element_text(size = 16)
  )
ggsave("output/uchicago/per_student/economics_humanities.png", width = 8, height = 5)
write_csv(humanities_econ, "output/uchicago/per_student/economics_humanities.csv")

uchicago_economics = data %>%
  filter(classification == "Economics") %>%
  group_by(year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  ) %>%
  bind_rows(
    data.frame(
      year = 2025,
      share_students = uchicago_econ_2024
    )
  ) %>%
  mutate(
    year = year - 1
  ) %>%
  filter(year >= 2005)


lm(share_students ~ year, data = uchicago_economics %>% filter(year < 2018))

ggplot(uchicago_economics, aes(x = year, y = share_students)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey") +
  
  annotate("text", x = 2011, y = 0.425,
           label = "Business Economics created",
           hjust = 0, vjust = -0.5, size = 4, family = "Georgia") +
  
  geom_segment(
    x = 2005,
    xend = 2024,
    y = -9.444735 + 0.004808 * 2005,
    yend = -9.444735 + 0.004808 * 2024,
    color = "grey85"
  ) +
  annotate("text", x = 2020, y = 0.23,
           label = "Pre-2018 trend",
           hjust = 0, vjust = -0.5, size = 4, family = "Georgia") +
  
  annotate("segment",
           x = 2019.9, xend = 2019,
           y = 0.24, yend = 0.255,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "grey85") +
  
  geom_point() +
  geom_line() +
  
  labs(
    x = NULL,
    y = "Share of students",
    title = "Graduating economics majors at UChicago"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  coord_cartesian(ylim = c(0.15, 0.45)) +
  theme_custom()
ggsave("output/uchicago/per_student/uchicago_economics_trend.png", width = 7.5, height = 4)


english_history_students = data %>%
  filter(
    major_name == "English Language and Literature, General"
    | major_name == "History, General"
  ) %>%
  mutate(
    classification = case_when(
      major_name == "English Language and Literature, General" ~ "English",
      major_name == "History, General" ~ "History",
      major_name == "Philosophy" ~ "Philosophy"
    )
  ) %>%
  group_by(year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students),
  )

ggplot(english_history_students, aes(x = year, y = share_students, color = classification, shape = classification)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Humanities majors at Chicago",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(values = c("English" = "#7bb14e", "History" = "#0076bd")) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  theme_custom() +
  theme(
    legend.position = c(0.7, 0.8),
    legend.text = element_text(size = 16)
  )
ggsave("output/uchicago/per_student/english_history_trend.png", width = 7.5, height = 4)
write_csv(english_history_students, "output/uchicago/per_student/english_history_trend.csv")


philosphy_students = data %>%
  filter(
    major_name == "Philosophy"
  ) %>%
  group_by(year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  )


english_polysci_students = data %>%
  filter(
    major_name == "English Language and Literature, General"
    | major_name == "Political Science and Government, General"
  ) %>%
  mutate(
    classification = case_when(
      major_name == "English Language and Literature, General" ~ "English",
      major_name == "Political Science and Government, General" ~ "Political Science",
    )
  ) %>%
  group_by(year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students),
  ) %>%
  mutate(
    classification = factor(classification, levels = c("Political Science", "English"))
  )

ggplot(english_polysci_students, aes(x = year, y = share_students, color = classification, shape = classification)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Shrinking humanities and social sciences",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(values = c("English" = "#7bb14e", "Political Science" = "#0076bd")) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  theme_custom() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.text = element_text(size = 16)
  )
ggsave("output/uchicago/per_student/english_polisci_trend.png", width = 7.5, height = 4)

english_polysci_students = english_polysci_students %>%
  pivot_wider(names_from = classification, values_from = share_students)
write_csv(english_polysci_students, "output/uchicago/per_student/english_polisci_trend.csv")


philosphy_students = data %>%
  filter(
    major_name == "Philosophy"
  ) %>%
  group_by(year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  )
ggplot(philosphy_students, aes(x = year, y = share_students)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Philosophy majors at Chicago"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  ylim(0.01, 0.07) +
  theme_custom()
ggsave("output/uchicago/per_student/philosophy_trend.png", width = 7.5, height = 4)
write_csv(philosphy_students, "output/uchicago/per_student/philosophy_trend.csv")



plot_major_trend = function(major_name, title) {
  data %>%
    filter(
      major_name == !!major_name
    ) %>%
    group_by(year) %>%
    summarise(
      share_students = sum(total) / first(total_students)
    ) %>%
    ggplot(aes(x = year, y = share_students)) +
    geom_line() +
    geom_point() +
    labs(
      x = NULL,
      y = "Share of students",
      title = title
    ) +
    scale_x_continuous(breaks = seq(2005, 2025, 3)) +
    theme_custom()
}


plot_major_trend("Political Science and Government, General", "Political science majors at Chicago")
ggsave("output/uchicago/per_student/polisci_trend.png", width = 7.5, height = 4)


plot_major_trend("Data Science, General", "Political science majors at Chicago")

substitution = data %>%
  filter(
    classification == "Humanities and Arts"
    # & major_name %in% c("Linguistics", "Comparative Literature", "Classical Studies",
    #                                "English Language and Literature", "General Humanities/Liberal Studies",
    #                                "Selected Interdisciplinary Studies", "Philosphy", "Religion", "History",
    #                                "Study of the Arts"))
  | major_name %in% c("Public Policy Analysis, General", "Public Policy Analysis, Other")
  | major_name %in% c("Mathematics, General", "Statistics, General")
  ) %>%
  mutate(
    classification = case_when(
      classification == "Humanities and Arts" ~ "Humanities and Art",
      grepl("economics", major_name, ignore.case = TRUE) ~ "Economics",
      major_name == "Public Policy Analysis, General" ~ "Public Policy",
      major_name %in% c("Mathematics, General", "Statistics, General") ~ "Math and Stats"
    )
  ) %>%
  group_by(year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students),
  ) 

ggplot(substitution, aes(x = year, y = share_students, color = classification, shape = classification)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey85") +
  annotate("text", x = 2014, y = 0.375, label = "Business economics created", size = 4, family = "Georgia") +
  annotate("text", x = 2008, y = 0.22, label = "Humanities and Art", size = 5, family = "Georgia") +
  annotate("segment",
           x = 2007, xend = 2009,
           y = 0.24, yend = 0.3,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "grey85") +
  annotate("text", x = 2008.25, y = 0.14, label = "Math and Stats", size = 5, family = "Georgia") +
  annotate("segment",
           x = 2010.6, xend = 2012,
           y = 0.125, yend = 0.1,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "grey85") +
  annotate("text", x = 2021, y = 0.025, label = "Public Policy", size = 5, family = "Georgia") +
  annotate("segment",
           x = 2019.25, xend = 2018.5,
           y = 0.0375, yend = 0.085,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "grey85") +
  geom_line() +
  geom_point() +
  labs(
    title = "Substitution in response to business economics",
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(values = c(
    "Humanities and Art" = "#7bb14e",
    "Economics" = "#0076bd",
    "Public Policy" = "#d95f02",
    "Math and Stats" = "#1b9e77"
  )) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  ylim(0, 0.4) +
  theme_custom() +
  theme(
    legend.position = "none"
  )
ggsave("output/uchicago/per_student/substitution_trend.png", width = 7.5, height = 4)
write_csv(substitution, "output/uchicago/per_student/substitution_trend.csv")


post_bizecon_change = data %>%
  group_by(year, major_name) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  ) %>%
  group_by(major_name) %>%
  filter(
    any(year == 2018 & share_students > 0.005)
  ) %>%
  summarise(
    change = share_students[year == 2024] / share_students[year == 2018]
  )

substitution = data %>%
  filter(
    major_name %in% c(
      "Mathematics, General", "Statistics, General",
      "Public Policy Analysis, General"
    )
  ) %>%
  mutate(
    classification = case_when(
      major_name %in% c("Mathematics, General", "Statistics, General") ~ "Math and Stats",
      major_name == "Public Policy Analysis, General" ~ "Public Policy",
      major_name == "Political Science and Government, General" ~ "Political Science",
      major_name == "Sociology, General" ~ "Sociology",
      major_name %in% c("Counseling Psychology", "Experimental Psychology") ~ "Psychology",
      grepl("econ", major_name, ignore.case = TRUE) ~ "Economics",
      major_name == "Biology/Biological Sciences, General" ~ "Biology",
      classification %in% c("Humanities", "Fine & Performing Arts") ~ "Arts and Humanities"
    )
  ) %>%
  group_by(year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  )

ggplot(substitution, aes(x = year, y = share_students,
                         color = classification, shape = classification)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey50") + 
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL
  ) +
  scale_color_manual(values = c(
    "Math and Stats" = "#1b9e77",
    "Public Policy" = "#d95f02",
    "Arts and Humanities" = "#7570b3"
  )) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  theme_custom() +
  theme(
    legend.position = c(0.25, 0.75),
    legend.text = element_text(size = 15)
  )
ggsave("output/uchicago/per_student/substitution_trend_no_humanities.png", width = 8, height = 5)  

  
  
ss_select = data %>%
  filter(
    major_name %in% c("Anthropology, General", "Political Science and Government, General")
  ) %>%
  mutate(
    major_name = case_when(
      major_name == "Anthropology, General" ~ "Anthropology",
      major_name == "Political Science and Government, General" ~ "Political Science"
    ),
    major_name = factor(major_name, levels = c("Political Science", "Anthropology"))
  ) %>%
  group_by(major_name, year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  )

ggplot(ss_select, aes(x = year, y = share_students, color = major_name, shape = major_name)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Shrinking social science majors",
    color = NULL,
    shape = NULL
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  theme_custom() +
  scale_color_manual(values = c(
    "Anthropology" = "#1b9e77",
    "Political Science" = "#d95f02"
  )) +
  theme(
    legend.position = c(0.8, 0.85),
    legend.text = element_text(size = 12)
  )
ggsave("output/uchicago/per_student/ss_select_trend.png", width = 7.5, height = 4)  
write_csv(ss_select, "output/uchicago/per_student/ss_select_trend.csv")
  
phil_history = data %>%
  filter(
    major_name %in% c("History, General", "Philosophy")
  ) %>%
  mutate(
    major_name = case_when(
      major_name == "History, General" ~ "History",
      major_name == "Philosophy" ~ "Philosophy"
    ),
    major_name = factor(major_name, levels = c("History", "Philosophy"))
  ) %>%
  group_by(major_name, year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  )

ggplot(phil_history, aes(x = year, y = share_students, color = major_name, shape = major_name)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Philsophy and history majors are stable",
    color = NULL,
    shape = NULL
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  theme_custom() +
  scale_color_manual(values = c(
    "Philosophy" = "#0076bd",
    "History" = "#7bb14e"
  )) +
  theme(
    legend.position = c(0.8, 0.85),
    legend.text = element_text(size = 12)
  )
ggsave("output/uchicago/per_student/phil_history.png", width = 7.5, height = 4)  
write_csv(phil_history, "output/uchicago/per_student/phil_history_trend.csv")

