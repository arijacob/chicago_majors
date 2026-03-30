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

# Load data ---------------------------------------------------------------

data = read_csv("Data/Output/ivyplus_perdegree.csv") %>%
  rename(total_award_year = total_awards_year)

economics_data = data %>%
  filter(cip_name == "Economics") %>%
  group_by(institution, year) %>%
  summarise(
    economics_count = sum(count),
    total_count = first(total_award_year)
  ) %>%
  mutate(
    economics_share = economics_count / total_count,
    is_uchicago = ifelse(institution == "University of Chicago", "University of Chicago", "Other Ivy-Plus"),
    is_uchicago = factor(is_uchicago, levels = c("University of Chicago", "Other Ivy-Plus"))
  ) %>%
  group_by(is_uchicago, year) %>%
  summarise(
    economics_share = mean(economics_share)
  )

economics_data %>%
  ggplot(aes(x = year, y = economics_share, color = factor(is_uchicago)))+
  geom_point(size = 1.75) +
  geom_line() +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  labs(
    x = NULL,
    y = "Share of total degrees awarded",
    title = "Economics Majors",
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  theme_custom() +
  ylim(0, 0.40) +
  theme(legend.position = c(0.3, 0.85))
ggsave("Figures/ivyplus/per_degree/ivyplus_economics_share_overtime.png", width = 8, height = 5)


humanities_overtime = data %>%
  filter(cip_name != "Economics", 
         cip_name %in% c(
           "English language and literature/letters", 
           "Visual and Performing Arts",
           "Liberal arts and sciences, general studies and humanities",
           "Philosophy and religious studies",
           "History",
           "English Language and Literature/Letters",
           "Liberal Arts and Sciences, General Studies and Humanities",
           "Philosophy and Religious Studies",
           "Visual and performing arts"
         )
  ) %>%
  group_by(year, institution) %>%
  summarise(
    humanities_awards = sum(count),
    total_awards = first(total_award_year)
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
    humanities_share = mean(humanities_share)
  )

humanities_overtime %>%
  ggplot(aes(x = year, y = humanities_share, color = factor(is_uchicago)))+
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Share of total degrees awarded",
    title  = "Humanities and Arts Majors",
    color = NULL
  ) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  theme_custom() +
  ylim(0.075, 0.30) +
  theme(legend.position = c(0.7, 0.85),
        legend.text = element_text(size = 16))
ggsave("Figures/ivyplus/per_degree/ivyplus_humanities_share_overtime.png", width = 8, height = 5)

# data %>%
#   filter(cip_name != "Economics", 
#          cip_name %in% c(
#            "Mathematics and Statistics", 
#            "Mathematics and statistics",
#            "Computer and information sciences and support services",
#            "Computer and Information Sciences and Support Services")
#   ) %>%
#   group_by(year, institution) %>%
#   summarise(
#     humanities_awards = sum(count),
#     total_awards = first(total_award_year)
#   ) %>%
#   mutate(
#     humanities_share = humanities_awards / total_awards,
#     is_uchicago = ifelse(institution == "University of Chicago", 1, 0)
#   ) %>%
#   group_by(institution) %>%
#   mutate(
#     humanities_share_rel = humanities_share / first(humanities_share)
#   ) %>%
#   ggplot(aes(x = year, y = humanities_share, color = factor(is_uchicago), group = institution))+
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = c("gray80", "#32a852")) +
#   labs(
#     x = NULL,
#     y = "Share of Degrees in Math, CS, and Statistics",
#     color = NULL
#   ) +
#   scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
#   theme_custom() +
#   theme(legend.position = "none")
# ggsave("Figures/ivyplus/ivyplus_math_cs_stats_share.pdf", width = 8, height = 6)


other_social_science = data %>%
  filter(
    cip_name %in% c(
      "Social Sciences",
      "Economics",
      "Social sciences")
  ) %>%
  select(institution, year, cip_name, count, total_award_year) %>% 
  group_by(institution, year, cip_name, total_award_year) %>%
  summarise(count = sum(count)) %>%
  pivot_wider(
    id_cols = c(institution, year, total_award_year),
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
    total_awards = first(total_award_year)
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
    share = mean(share)
  )

other_social_science %>%
  ggplot(aes(x = year, y = share, color = factor(is_uchicago))) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("University of Chicago" = "#800000", "Other Ivy-Plus" = "#737373")) +
  labs(
    x = NULL,
    y = "Share of total degrees awarded",
    title = "Non-economics Social Sciences",
    color = NULL
  ) +
  ylim(0.07, 0.30) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) +
  theme_custom() +
  theme(legend.position = c(0.77, 0.83),
        legend.text = element_text(size = 16))
ggsave("Figures/ivyplus/per_degree/ivyplus_social_sciences.png", width = 8, height = 5)
