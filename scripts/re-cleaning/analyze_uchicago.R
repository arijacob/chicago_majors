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

classifications = read_xlsx("data/classifications/Degree_Program_Code_Catalog.xlsx", sheet = 2, skip = 3) %>%
  clean_names() %>%
  select(code, cip_code, humanities_discipline, classification = bachelors)

data = read_csv("data/ipeds/cleaned/major_numbers.csv") %>%
  filter(instnm == "University of Chicago") %>%
  left_join(classifications, by = c("cipcode" = "code")) %>%
  group_by(instnm, year) %>%
  mutate(
    total_students = sum(total[major_number == 1]),
    total_degrees = sum(total)
  )


change = data %>%
  group_by(year, cip_code, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  ) %>%
  filter(
    year %in% c(2005, 2024),
    classification %in% c("Humanities", "Fine & Performing Arts")
    ) %>%
  pivot_wider(names_from = year, values_from = share_students) %>%
  mutate(
    change = `2024` - `2005`
  )
  

uchicago_econ_2024 = 0.41

humanities_econ = data %>%
  filter(
    (
      (classification == "Humanities" | classification == "Fine & Performing Arts") 
           # & humanities_discipline %in% c("Linguistics", "Comparative Literature", "Classical Studies",
           #                              "English Language and Literature", "General Humanities/Liberal Studies",
           #                              "Selected Interdisciplinary Studies", "Philosphy", "Religion", "History",
           #                              "Study of the Arts", "Cultural, Ethnic, and Gender Studies")
          )
         | grepl("economics", cip_code, ignore.case = TRUE)) %>%
  mutate(
    classification = case_when(
      classification %in% c("Humanities", "Fine & Performing Arts") ~ "Humanities and Art",
      grepl("economics", cip_code, ignore.case = TRUE) ~ "Economics"
    )
  ) %>%
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
  scale_color_manual(values = c("Economics" = "#0076bd", "Humanities and Art" = "#7bb14e")) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  ylim(0.1, 0.45) +
  theme(
    legend.position = c(0.3, 0.85),
    legend.text = element_text(size = 16)
  )
ggsave("output/uchicago/per_student/economics_humanities.png", width = 8, height = 5)

uchicago_economics = data %>%
  filter(grepl("economics", cip_code, ignore.case = T)) %>%
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
  
  annotate("text", x = 2010.5, y = 0.425,
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
    cip_code == "English Language and Literature, General"
    | cip_code == "History, General"
  ) %>%
  mutate(
    classification = case_when(
      cip_code == "English Language and Literature, General" ~ "English",
      cip_code == "History, General" ~ "History",
      cip_code == "Philosophy" ~ "Philosophy"
    )
  ) %>%
  group_by(year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students),
  )

ggplot(english_history_students, aes(x = year, y = share_students, color = classification)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "English and history majors at Chicago",
    color = NULL
  ) +
  scale_color_manual(values = c("English" = "#7bb14e", "History" = "#0076bd")) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  theme_custom() +
  theme(
    legend.position = c(0.7, 0.8),
    legend.text = element_text(size = 16)
  )
ggsave("output/uchicago/per_student/english_history_trend.png", width = 7.5, height = 4)

philosphy_students = data %>%
  filter(
    cip_code == "Philosophy"
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


plot_major_trend = function(cip_code, title) {
  data %>%
    filter(
      cip_code == !!cip_code
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



substitution =data %>%
  filter(
    classification == "Humanities" | classification == "Fine & Performing Arts"
  | grepl("economics", cip_code, ignore.case = TRUE)
  | cip_code == "Public Policy Analysis, General"
  | cip_code %in% c("Mathematics, General", "Statistics, General")
  ) %>%
  mutate(
    classification = case_when(
      classification %in% c("Humanities", "Fine & Performing Arts") ~ "Humanities and Art",
      grepl("economics", cip_code, ignore.case = TRUE) ~ "Economics",
      cip_code == "Public Policy Analysis, General" ~ "Public Policy",
      cip_code %in% c("Mathematics, General", "Statistics, General") ~ "Math and Stats"
    )
  ) %>%
  group_by(year, classification) %>%
  summarise(
    share_students = sum(total) / first(total_students),
  ) 

ggplot(substitution, aes(x = year, y = share_students, color = classification)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL
  ) +
  scale_color_manual(values = c(
    "Humanities and Art" = "#7bb14e",
    "Economics" = "#0076bd",
    "Public Policy" = "#d95f02",
    "Math and Stats" = "#1b9e77"
  )) +
  scale_x_continuous(breaks = seq(2005, 2025, 3)) +
  theme_custom()


post_bizecon_change = data %>%
  group_by(year, cip_code) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  ) %>%
  group_by(cip_code) %>%
  filter(
    any(year == 2018 & share_students > 0.005)
  ) %>%
  summarise(
    change = share_students[year == 2024] / share_students[year == 2018]
  )

substitution = data %>%
  filter(
    cip_code %in% c(
      "Mathematics, General", "Statistics, General",
      "Public Policy Analysis, General"
    )
    | classification %in% c("Humanities", "Fine & Performing Arts")
    # | grepl("econ", cip_code, ignore.case = TRUE)
  ) %>%
  mutate(
    classification = case_when(
      cip_code %in% c("Mathematics, General", "Statistics, General") ~ "Math and Stats",
      cip_code == "Public Policy Analysis, General" ~ "Public Policy",
      cip_code == "Political Science and Government, General" ~ "Political Science",
      cip_code == "Sociology, General" ~ "Sociology",
      cip_code %in% c("Counseling Psychology", "Experimental Psychology") ~ "Psychology",
      grepl("econ", cip_code, ignore.case = TRUE) ~ "Economics",
      cip_code == "Biology/Biological Sciences, General" ~ "Biology",
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
    legend.position = c(0.25, 0.5),
    legend.text = element_text(size = 15)
  )
ggsave("output/uchicago/per_student/substitution_trend.png", width = 8, height = 5)  
  
  
  
ss_select = data %>%
  filter(
    cip_code %in% c("Anthropology, General", "Political Science and Government, General")
  ) %>%
  mutate(
    cip_code = case_when(
      cip_code == "Anthropology, General" ~ "Anthropology",
      cip_code == "Political Science and Government, General" ~ "Political Science"
    ),
    cip_code = factor(cip_code, levels = c("Political Science", "Anthropology"))
  ) %>%
  group_by(cip_code, year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  )
ggplot(ss_select, aes(x = year, y = share_students, color = cip_code)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Select social science majors at Chicago",
    color = NULL
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
  
  
phil_history = data %>%
  filter(
    cip_code %in% c("History, General", "Philosophy")
  ) %>%
  mutate(
    cip_code = case_when(
      cip_code == "History, General" ~ "History",
      cip_code == "Philosophy" ~ "Philosophy"
    ),
    cip_code = factor(cip_code, levels = c("History", "Philosophy"))
  ) %>%
  group_by(cip_code, year) %>%
  summarise(
    share_students = sum(total) / first(total_students)
  )
ggplot(phil_history, aes(x = year, y = share_students, color = cip_code, shape = cip_code)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Select social science majors at Chicago",
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

  
