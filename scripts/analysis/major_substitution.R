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

public_policy_switch = data %>%
  filter( 
    cip_name %in% c("Public Policy Analysis", "Public Policy Analysis, General",
                    "Political Science, General", "Political Science and Government, General") |
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
  annotate(
    "text",
    x = 2014.5,
    y = 0.35,
    label = "Business economics created",
    size = 4.5,
    color = "grey20",
    family = "Georgia"
  ) +
  scale_color_brewer(palette = "Set2") +
  geom_line() +
  geom_point() +
  theme_custom() +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL
  ) +
  theme(legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 16)) +
  ylim(0.1, 0.4)
public_policy_switch
ggsave("Figures/uchicago/per_student/pubpol_substituion.png", width = 8, height = 5)


quant_and_hum_substitution = data %>%
  filter( 
  grepl("Math", cip_name) |
    grepl("Stat", cip_name) |
      grepl("Econ", cip_name) |
    classification == "Humanities and Art"
) %>%
  mutate(
    classification = case_when(
      grepl("Econ", cip_name) ~ "Economics",
      grepl("Math", cip_name) | grepl("Stat", cip_name) ~ "Math and statistics",
      TRUE ~ classification)
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
  annotate(
    "text",
    x = 2015,
    y = 0.35,
    label = "Business economics created",
    size = 4.5,
    color = "grey20",
    family = "Georgia"
  ) +
  scale_color_brewer(palette = "Set2") +
  geom_line() +
  geom_point() +
  theme_custom() +
  labs(
    x = NULL,
    y = "Share of students",
    color = NULL
  ) +
  theme(legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 16))
quant_and_hum_substitution
ggsave("Figures/uchicago/per_student/quant_hum_substituion.png", width = 9, height = 5)
# braoder_majors_df = data %>%
#   mutate(
#     cip_name = str_remove(cip_name, ", General"),
#     cip_name = str_remove(cip_name, ", Other")
#   ) %>%
#   mutate(major_group = case_when(
#     
#     major %in% c(
#       "Computer Science",
#       "Computer and Information Sciences, Gen.",
#       "Computer Programming",
#       "Systems Science and Theory"
#     ) ~ "Computer Science & Computing",
#     
#     major %in% c(
#       "Economics",
#       "Econometrics and Quantitative Economics"
#     ) ~ "Economics",
#     
#     # 7. Psychology & Cognitive Sciences
#     major %in% c(
#       "Psychology",
#       "Experimental Psychology",
#       "Counseling Psychology",
#       "Cognitive Science"
#     ) ~ "Psychology & Cognitive Science",
#     
#     # 8. Environmental & Earth Studies
#     major %in% c(
#       "Environmental Science/Studies",
#       "Environmental Studies",
#       "Geography"
#     ) ~ "Environmental & Earth Studies",
#     
#     # 9. Political Science & Policy
#     major %in% c(
#       "Political Science",
#       "Political Science and Government",
#       "International Relations and Affairs",
#       "International Policy Analysis"
#     ) ~ "Political Science",
#     
#     # 11. History & History of Science
#     major %in% c(
#       "History & Philosophy of Science and Tech",
#       "History and Philosophy of Science and Technology",
#       "Social Sciences and History"
#     ) ~ "History of Science",
#     
#     # 15. European Languages & Classics
#     major %in% c(
#       "Romance Languages and Literatures",
#       "Romance Languages, Literatures, and Linguistics",
#       "German Language and Literature",
#       "Classics & Classical Languages and Lit",
#       "Classics/Classical Languages, Lit & Linguistics",
#       "Classics and Classical Languages, Literatures, and Linguistics"
#     ) ~ "European Languages & Classics",
#     
#     # 16. Slavic, Russian & Eurasian Studies
#     major %in% c(
#       "Russian Studies",
#       "Russian and Slavic Area Studies",
#       "Slavic Lang. & Lit. (Other Than Russian)",
#       "Slavic Languages, Literatures, and Linguistics"
#     ) ~ "Slavic & Russian Studies",
#     
#     # 17. East & South Asian Languages and Studies
#     major %in% c(
#       "East Asian Languages, Literatures, and Linguistics",
#       "East/Southeast Asian Lang. & Lit., Oth.",
#       "South Asian Studies",
#       "South Asian Languages and Literatures",
#       "South Asian Languages, Literatures, and Linguistics"
#     ) ~ "East & South Asian Studies",
#     
#     # 18. Middle Eastern & Near Eastern Studies
#     major %in% c(
#       "Near and Middle Eastern Studies",
#       "Mid Eastern Languages & Literatures, Oth",
#       "Middle/Near Eastern/Semitic Languages, Lit & Linguistics",
#       "Middle/Near Eastern and Semitic Languages, Literatures, and Linguistics",
#       "Ancient Near Eastern/Biblical Languages, Lit & Linguistics",
#       "Ancient Near Eastern and Biblical Languages, Literatures, and Linguistics"
#     ) ~ "Middle Eastern & Near Eastern Studies",
#     
#     # 19. Religious & Theological Studies
#     major %in% c(
#       "Theology/Theological Studies",
#       "Biblical & Oth Theological Lang. & Lit.",
#       "Jewish/Judaic Studies"
#     ) ~ "Religious & Theological Studies",
#     
#     # 20. Ethnic, Cultural & Gender Studies
#     major %in% c(
#       "African Studies",
#       "Latin American Studies",
#       "Ethnic and Cultural Studies",
#       "Ethnic, Cultural Minority, and Gender Studies",
#       "Ethnic, Cultural Minority, Gender, and Group Studies"
#     ) ~ "Ethnic, Cultural & Gender Studies",
#     
#     # 21. Arts, Film & Media
#     major %in% c(
#       "Art",
#       "Art History, Criticism and Conservation",
#       "Music",
#       "Visual and Performing Arts",
#       "Film/Cinema Studies",
#       "Film/Cinema/Video Studies",
#       "Film/Cinema/Media Studies",
#       "Directing and Theatrical Production",
#       "Design and Visual Communications",
#       "Digital Communication and Media/Multimedia"
#     ) ~ "Arts, Film & Media",
#     
#     # 22. Education & Liberal Studies
#     major %in% c(
#       "Education",
#       "Liberal Arts & Sciences/Liberal Studies",
#       "Liberal Arts and Sciences/Liberal Studies"
#     ) ~ "Education & Liberal Studies",
#     
#     TRUE ~ cip_name
#   ))
# 
# major_changes = braoder_majors_df %>%
#   filter(year %in% c(2017, 2018, 2021, 2022)) %>%
#   group_by(year, major_group) %>%
#   summarise(
#     count = sum(count),
#     total_awards_year = first(total_awards_year)
#   ) %>%
#   mutate(
#     share = count / total_awards_year,
#     post = year >= 2021
#   ) %>%
#   group_by(major_group, post) %>%
#   summarise(
#     share = mean(share)
#   ) %>%
#   pivot_wider(names_from = post, values_from = share) %>%
#   mutate(
#     change = `TRUE` - `FALSE`,
#     percent_change = change / `FALSE` * 100
#   )
#   
#   
# data %>%
#   filter( 
#    grepl("History", cip_name) |
#       grepl("Econ", cip_name)
#   ) %>%
#   mutate(
#     classification = case_when(
#       grepl("Econ", cip_name) ~ "Economics",
#       TRUE ~ "Public Policy")
#   ) %>%
#   group_by(year, classification) %>%
#   summarise(
#     count = sum(count),
#     total_awards_year = first(total_awards_year)
#   ) %>%
#   mutate(
#     share = count / total_awards_year
#   ) %>%
#   ggplot(aes(x = year, y = share, color = classification)) +
#   geom_vline(xintercept = 2018, linetype = "dashed", color = "grey60") +
#   scale_color_brewer(palette = "Set2") +
#   geom_line() +
#   geom_point() +
#   theme_custom() +
#   labs(
#     x = NULL,
#     y = "Share of Degrees Awarded",
#     color = NULL
#   ) +
#   theme(legend.position = "bottom")
# 
# # Pub pol, Phil, politcal sience, biology, 

