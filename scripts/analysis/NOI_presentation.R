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
      
      title = element_text(size = 18),
      
      legend.text = element_text(size = 16),
      legend.key.size  = unit(1.2, "cm")
    )
}

loadfonts(device = "pdf")

setwd("/Users/arijacob/Documents/Maroon Article")

# Chicago Econ ------------------------------------------------------------

data = read_csv("Data/Output/uchicago_perstudent.csv") %>%
  rename(total_awards_year = total_award_year)

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
  ) 

econ_humanities_first_half_plot = econ_vs_humanities %>%
  mutate(
    share = ifelse(year >= 2014, NA, share)
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
econ_humanities_first_half_plot
ggsave("Figures/noi/econ_v_hum_first_half.png",
       plot = econ_humanities_first_half_plot,
       width = 7, height = 4)

econ_humanities_plot = econ_vs_humanities %>%
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
econ_humanities_plot
ggsave("Figures/noi/econ_v_hum_full.png",
       plot = econ_humanities_plot,
       width = 7, height = 4)


# Business Economics ------------------------------------------------------
econ_humanities_plot_bizecon = econ_vs_humanities %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey50") +
  annotate("text", x = 2018.5, y = 0.43, label = "Business Economics created",
           hjust = 0, vjust = -0.5, size = 3.5, family = "Georgia") +
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
econ_humanities_plot_bizecon
ggsave("Figures/noi/econ_v_hum_bizecon.png",
       plot = econ_humanities_plot_bizecon,
       width = 7, height = 4)

groups_overtime = data %>%
  group_by(year, classification) %>%
  summarise(
    total_awards = sum(count),
    total_awards_year = first(total_awards_year)
  ) %>%
  mutate(
    share = total_awards / total_awards_year
  )

groups_overtime_ss = groups_overtime %>%
  filter(
    !(classification %in% c("Economics", "Humanities and Art")),
  ) %>%
  mutate(
    share = ifelse(classification %in% c("STEM", "Math and Allied Fields", "CS and Data Science", "Area Studies and Languages"),
                   NA, share)
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  annotate("text", x = 2016, y = 0.40, label = "Social Sciences",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#e78cc4") +
  geom_line(linewidth = 0.4) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(
      "Social Sciences" = "#e78cc4",
      "STEM" = "#a9d95b",
      "Math and Allied Fields" = "#8fa2cb",
      "CS and Data Science" = "#fc9065",
      "Area Studies and Languages" = "#70c5aa"
    )
  ) +  ylim(0,0.45) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  #                    breaks = seq(0, 0.45, by = 0.05)
  # ) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.position = "none")
groups_overtime_ss
ggsave("Figures/noi/other_fields_overtime_ss.png",
       plot = groups_overtime_ss,
       width = 7, height = 4)

groups_overtime_sciences = groups_overtime %>%
  filter(
    !(classification %in% c("Economics", "Humanities and Art"))
  ) %>%
  mutate(
    share = ifelse(classification %in% c("Math and Allied Fields", "CS and Data Science", "Area Studies and Languages"),
                   NA, share)
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  annotate("text", x = 2016, y = 0.40, label = "Social Science",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#e78cc4") +
  annotate("text", x = 2007, y = 0.25, label = "Science",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#a9d95b") +
  geom_line(linewidth = 0.4) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(
      "Social Sciences" = "#e78cc4",
      "STEM" = "#a9d95b",
      "Math and Allied Fields" = "#8fa2cb",
      "CS and Data Science" = "#fc9065",
      "Area Studies and Languages" = "#70c5aa"
    )
  ) +  ylim(0,0.45) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  #                    breaks = seq(0, 0.45, by = 0.05)
  # ) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.position = "none")
groups_overtime_sciences
ggsave("Figures/noi/other_fields_overtime_science.png",
       plot = groups_overtime_sciences,
       width = 7, height = 4)

groups_overtime_math = groups_overtime %>%
  filter(
    !(classification %in% c("Economics", "Humanities and Art"))
  ) %>%
  mutate(
    share = ifelse(classification %in% c("CS and Data Science", "Area Studies and Languages"),
                   NA, share)
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  annotate("text", x = 2016, y = 0.40, label = "Social Sciences",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#e78cc4") +
  annotate("text", x = 2007, y = 0.25, label = "Science",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#a9d95b") +
  annotate("text", x = 2013, y = 0.16, label = "Math +",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#8fa2cb") +
  geom_line(linewidth = 0.4) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(
      "Social Sciences" = "#e78cc4",
      "STEM" = "#a9d95b",
      "Math and Allied Fields" = "#8fa2cb",
      "CS and Data Science" = "#fc9065",
      "Area Studies and Languages" = "#70c5aa"
    )
  ) +
  ylim(0,0.45) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  #                    breaks = seq(0, 0.45, by = 0.05)
  # ) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.position = "none")
groups_overtime_math
ggsave("Figures/noi/other_fields_overtime_math.png",
       plot = groups_overtime_math,
       width = 7, height = 4)

groups_overtime_cs = groups_overtime %>%
  filter(
    !(classification %in% c("Economics", "Humanities and Art"))
  ) %>%
  mutate(
    share = ifelse(classification %in% c("Area Studies and Languages"),
                   NA, share)
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  annotate("text", x = 2016, y = 0.40, label = "Social Sciences",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#e78cc4") +
  annotate("text", x = 2007, y = 0.25, label = "Sciences",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#a9d95b") +
  annotate("text", x = 2013, y = 0.16, label = "Math +",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#8fa2cb") +
  annotate("text", x = 2015, y = 0.085, label = "CS and Data Science",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#fc9065") +
  geom_line(linewidth = 0.4) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(
      "Social Sciences" = "#e78cc4",
      "STEM" = "#a9d95b",
      "Math and Allied Fields" = "#8fa2cb",
      "CS and Data Science" = "#fc9065",
      "Area Studies and Languages" = "#70c5aa"
    )
  ) +  
  ylim(0,0.45) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  #                    breaks = seq(0, 0.45, by = 0.05)
  # ) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.position = "none")
groups_overtime_cs
ggsave("Figures/noi/other_fields_overtime_cs.png",
       plot = groups_overtime_cs,
       width = 7, height = 4)


groups_overtime_area = groups_overtime %>%
  filter(
    !(classification %in% c("Economics", "Humanities and Art"))
  ) %>%
  ggplot(aes(x = year, y = share, color = classification, shape = classification)) +
  annotate("text", x = 2016, y = 0.40, label = "Social Sciences",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#e78cc4") +
  annotate("text", x = 2007, y = 0.25, label = "Sciences",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#a9d95b") +
  annotate("text", x = 2013, y = 0.16, label = "Math +",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#8fa2cb") +
  annotate("text", x = 2015, y = 0.085, label = "CS and Data Science",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#fc9065") +
  annotate("text", x = 2018, y = 0.0075, label = "Area Studies",
           hjust = 0, vjust = -0.5, size = 4.5, family = "Georgia", color = "#70c5aa") +
  geom_line(linewidth = 0.4) +
  geom_line(linewidth = 0.4) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(
      "Social Sciences" = "#e78cc4",
      "STEM" = "#a9d95b",
      "Math and Allied Fields" = "#8fa2cb",
      "CS and Data Science" = "#fc9065",
      "Area Studies and Languages" = "#70c5aa"
    )
  ) +  
  ylim(0,0.45) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  #                    breaks = seq(0, 0.45, by = 0.05)
  # ) +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL,
    shape = NULL) +
  theme_custom() +
  theme(legend.position = "none")
groups_overtime_area
ggsave("Figures/noi/other_fields_overtime_area.png",
       plot = groups_overtime_area,
       width = 7, height = 4)


# Ivy-Plus ----------------------------------------------------------------
humanities_cip = c(23, 24, 38, 50, 54, 39)
social_sciences_cip = c(42, 45, 45.06, 44)

data = read_csv("Data/Output/ivyplus_perstudent.csv") %>%
  mutate(
    cip_code = as.numeric(str_replace_all(cip_code, "'", ""))
  )

economics_data = data %>%
  filter(cip_name == "Economics", year >= 2005) %>%
  group_by(institution, year) %>%
  summarise(
    economics_count = sum(count),
    total_count = first(students)
  ) %>%
  mutate(
    economics_share = economics_count / total_count,
    is_uchicago = ifelse(institution == "University of Chicago", "University of Chicago", "Other Ivy-Plus"),
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy-Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    economics_share = weighted.mean(economics_share, total_count)
  ) %>%
  bind_rows(
    data.frame(
      is_uchicago = "University of Chicago",
      year = 2024,
      economics_share = 0.41
    )
  )

economics_data %>%
  mutate(
    economics_share = ifelse(is_uchicago != "University of Chicago", NA, economics_share)
  ) %>%
  ggplot(aes(x = year, y = economics_share, color = factor(is_uchicago)))+
  geom_point(size = 1.75) +
  geom_line() +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Economics Majors",
    color = NULL
  ) +
  theme_custom() +
  ylim(0, 0.5) +
  theme(legend.position = c(0.3, 0.85))
ggsave("Figures/noi/ivy_plus/ivyplus_economics_noivy.png",
       width = 7, height = 4)

economics_data %>%
  ggplot(aes(x = year, y = economics_share, color = factor(is_uchicago)))+
  geom_point(size = 1.75) +
  geom_line() +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Economics Majors",
    color = NULL
  ) +
  theme_custom() +
  ylim(0, 0.5) +
  theme(legend.position = c(0.3, 0.85))
ggsave("Figures/noi/ivy_plus/ivyplus_economics.png",
       width = 7, height = 4)

other_social_science = data %>%
  filter(
    cip_code %in% social_sciences_cip,
    year >= 2005
  ) %>%
  select(institution, year, cip_name, count, students) %>% 
  group_by(institution, year, cip_name, students) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(
    id_cols = c(institution, year, students),
    names_from = cip_name, values_from = count) %>%
  mutate(
    social_sciences = coalesce(`Social Sciences`, `Social sciences`)
  ) %>%
  mutate(
    social_sciences = social_sciences - Economics
  ) %>%
  group_by(year, institution) %>%
  summarise(
    awards = sum(social_sciences),
    total_awards = first(students)
  ) %>%
  mutate(
    share = awards / total_awards,
    is_uchicago = ifelse(institution == "University of Chicago", "University of Chicago", "Other Ivy-Plus"),
    is_uchicago = factor(is_uchicago,  levels = c("University of Chicago", "Other Ivy-Plus"))
  ) %>%
  group_by(institution) %>%
  mutate(
    share_rel = share / first(share)
  ) %>%
  group_by(year, is_uchicago) %>%
  summarise(
    share = weighted.mean(share, total_awards)
  )

other_social_science %>%
  mutate(
    share = ifelse(is_uchicago != "University of Chicago", NA, share)
  ) %>%
  ggplot(aes(x = year, y = share, color = factor(is_uchicago))) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Non-economics Social Sciences",
    color = NULL
  ) +
  ylim(0.0, 0.25) +
  theme_custom() +
  theme(legend.position = c(0.77, 0.83),
        legend.text = element_text(size = 16)
  )
ggsave("Figures/noi/ivy_plus/ivyplus_othersocialscience_noivy.png",
       width = 7, height = 4)

other_social_science %>%
  ggplot(aes(x = year, y = share, color = factor(is_uchicago))) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  labs(
    x = NULL,
    y = "Share of students",
    title = "Non-economics Social Sciences",
    color = NULL
  ) +
  ylim(0.0, 0.25) +
  theme_custom() +
  theme(legend.position = c(0.77, 0.83),
        legend.text = element_text(size = 16)
  )
ggsave("Figures/noi/ivy_plus/ivyplus_othersocialscience.png",
       width = 7, height = 4)


humanities_overtime = data %>%
  filter(
    cip_name != "Economics", 
    cip_code %in% humanities_cip,
    year >= 2005
  ) %>%
  group_by(year, institution) %>%
  summarise(
    humanities_awards = sum(count),
    total_awards = first(students)
  ) %>%
  mutate(
    humanities_share = humanities_awards / total_awards,
    is_uchicago = ifelse(institution == "University of Chicago", "University of Chicago", "Other Ivy-Plus")
  ) %>%
  mutate(
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy-Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    humanities_share = weighted.mean(humanities_share, total_awards)
  ) %>%
  drop_na()

humanities_overtime %>%
  mutate(
    humanities_share = ifelse(is_uchicago != "University of Chicago", NA, humanities_share)
  ) %>%
  ggplot(aes(x = year, y = humanities_share, color = factor(is_uchicago)))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of students",
    title  = "Humanities and Arts Majors",
    color = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  theme_custom() +
  ylim(0.075, 0.3) +
  theme(legend.position = c(0.7, 0.85),
        legend.text = element_text(size = 16))
ggsave("Figures/noi/ivy_plus/ivyplus_hum_noivy.png",
       width = 7, height = 4)

humanities_overtime %>%
  ggplot(aes(x = year, y = humanities_share, color = factor(is_uchicago)))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of students",
    title  = "Humanities and Arts Majors",
    color = NULL
  ) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  theme_custom() +
  ylim(0.075, 0.3) +
  theme(legend.position = c(0.7, 0.85),
        legend.text = element_text(size = 16))
ggsave("Figures/noi/ivy_plus/ivyplus_hum.png",
       width = 7, height = 4)
