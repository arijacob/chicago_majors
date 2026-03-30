# Data from: https://nces.ed.gov/ipeds/reported-data/144050?year=2023&surveyNumber=3
library(tidyverse)
library(janitor)
library(extrafont)

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

setwd("/Users/arijacob/Documents/Maroon Article")

data = read_xlsx("Data/student_outcomes.xlsx") %>%
  clean_names() %>%
  mutate(
    industry = trimws(industry),
    year = factor(year, levels = c("2007", "2008", "2009", "2010", "2011",
                                   "2012", "2013", "2014", "2015", "2016",
                                   "2017", "2018", "2019", "2020", "2021-2022", "2023", "2024", "2025"))
  )

finance_business_data = data %>%
  filter(grepl("Finan", industry) | grepl("Business", industry)) %>%
  group_by(year) %>%
  summarise(
    share = sum(share),
    .groups = "drop"
  ) %>%
  bind_rows(
    data.frame(
      year = c("2018", "2019", "2020"),
      share = c(NA, NA, NA)
    )
  ) %>%
  mutate(
    idx = row_number(),
    share_interp = zoo::na.approx(share, idx, na.rm = FALSE),
    is_na = is.na(share),
    year = factor(year, levels = c("2007", "2008", "2009", "2010", "2011",
                                   "2012", "2013", "2014", "2015", "2016",
                                   "2017", "2018", "2019", "2020", "2021-2022", "2023", "2024", "2025"))
  ) 


ggplot(finance_business_data, aes(x = year, y = share)) +
  annotate("segment",
           x = "2017",
           xend = "2021-2022",
           y = 33.0 , yend = 34.0, 
           linetype = "dashed", color = "grey") +
  geom_line(group = 1) +
  geom_point() +
  labs(
    x = NULL,
    y = "Percent of employed graduates",
    subtitle = "Graduates in Finance and Business Industries"
  ) +
  scale_y_continuous(
    limits = c(10, 40),
    breaks = seq(10, 40, by = 5),
    labels = function(x) paste0(x, "%")
  ) +
  theme_custom() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("Figures/uchicago/outcomes/finance_business.png", width = 10, height = 6)


consulting = data %>%
  filter(grepl("Consult", industry)) %>%
  filter(year != "2024") %>%
  group_by(year) %>%
  summarise(
    share = sum(share),
    .groups = "drop"
  ) %>%
  bind_rows(
    data.frame(
      year = c("2018", "2019", "2020", "2024"),
      share = c(NA, NA, NA, NA)
    )
  ) %>%
  ggplot(aes(x = year, y = share)) +
  geom_point() +
  geom_line(group = 1) +
  labs(
    x = NULL,
    y = "Percent of Employed Graduates",
  ) +
  theme_custom() +
  ylim(5, 20)
consulting
