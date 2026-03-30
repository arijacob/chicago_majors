# Data from: https://nces.ed.gov/ipeds/reported-data/144050?year=2023&surveyNumber=3
library(tidyverse)
library(janitor)
library(extrafont)

rm(list = ls())

setwd("/Users/arijacob/Documents/Maroon Article")

csv_files <- list.files(path = "Data/IvyPlus/major_numbers", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

column_v_1 = c("id", "institution", "year", "major", "cip_code", "cip_name", "award_level", "count", "idx_c")
column_v_2 = c("id", "institution", "year", "cip_code", "cip_name", "major", "award_level", "count", "idx_c")
column_v_3 = c("id", "institution", "year", "award_level", "cip_code", "cip_name", "major", "count", "idx_c")

for (file in csv_files) {
  # Extract filename without extension
  year_folder <- basename(dirname(file))      # "2020-2021"
  second_year <- sub(".*-", "", year_folder)  # "2021"
  
  name <- paste0("ivyplus_", second_year)
  print(name)

  # Read the CSV
  data <- read_csv(file, show_col_types = FALSE) 
  
  # Assign it to the global environment with that name
  assign(name, data, envir = .GlobalEnv)
}


colnames(ivyplus_2001) = column_v_2
colnames(ivyplus_2002) = column_v_2
colnames(ivyplus_2003) = column_v_2
colnames(ivyplus_2004) = column_v_2
colnames(ivyplus_2005) = column_v_2
colnames(ivyplus_2006) = column_v_2
colnames(ivyplus_2007) = column_v_2
colnames(ivyplus_2008) = column_v_2
colnames(ivyplus_2009) = column_v_2
colnames(ivyplus_2010) = column_v_1
colnames(ivyplus_2011) = column_v_1
colnames(ivyplus_2012) = column_v_1
colnames(ivyplus_2013) = column_v_1
colnames(ivyplus_2014) = column_v_1
colnames(ivyplus_2015) = column_v_1
colnames(ivyplus_2016) = column_v_1
colnames(ivyplus_2017) = column_v_1
colnames(ivyplus_2018) = column_v_1
colnames(ivyplus_2019) = column_v_1
colnames(ivyplus_2020) = column_v_1
colnames(ivyplus_2021) = column_v_1
colnames(ivyplus_2022) = column_v_3
colnames(ivyplus_2023) = column_v_1
colnames(ivyplus_2024) = column_v_1

combined_data = bind_rows(
 ivyplus_2001, ivyplus_2002, ivyplus_2003,
 ivyplus_2004, ivyplus_2005, ivyplus_2006,
 ivyplus_2007, ivyplus_2008, ivyplus_2009,
 ivyplus_2010, ivyplus_2011, ivyplus_2012,
 ivyplus_2013, ivyplus_2014, ivyplus_2015,
 ivyplus_2016, ivyplus_2017, ivyplus_2018,
 ivyplus_2019, ivyplus_2020, ivyplus_2021,
 ivyplus_2022, ivyplus_2023, ivyplus_2024
) %>%
  mutate(year = year - 1)

# awards_file <- list.files(path = "Data/IvyPlus/degrees_awarded", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
# 
# for (file in awards_file) {
#   # Extract filename without extension
#   name <- tools::file_path_sans_ext(basename(file))
#   
#   # Read the CSV
#   data <- read_csv(file, show_col_types = FALSE) 
#   
#   colnames(data) = c("id", "institution", "year", "total_award_year")
#   
#   # Assign it to the global environment with that name
#   assign(name, data, envir = .GlobalEnv)
# }
# 
# awards_by_year = bind_rows(
#   awards_2006, awards_2007, awards_2008,
#   awards_2009, awards_2010, awards_2011,
#   awards_2012, awards_2013, awards_2014,
#   awards_2015, awards_2016, awards_2017,
#   awards_2018, awards_2019, awards_2020,
#   awards_2021, awards_2022, awards_2023
# ) %>%
#   mutate(
#     year = year - 1
#   )

awards_by_year = combined_data %>%
  filter(cip_code == "'99'") %>%
  distinct(institution, year, major, count) %>%
  group_by(institution, year) %>%
  summarise(
    total_awards = sum(count),
    students = count[major == "First major"],
    .groups = "drop"
  )
  
write_csv(awards_by_year, "Data/IvyPlus/awards_by_year.csv")

data = combined_data %>%
  left_join(awards_by_year, by = c("institution", "year")) %>%
  mutate(
    cip_name = gsub("\\.", "", cip_name)
  ) %>%
  filter(
    cip_code != "'99'",
    major == "First major"
    )

write_csv(data, "Data/Output/ivyplus_primarymajor.csv")

data_second_major = combined_data %>%
  left_join(awards_by_year, by = c("institution", "year")) %>%
  mutate(
    cip_name = gsub("\\.", "", cip_name)
  ) %>%
  filter(
    cip_code != "'99'"
  )
write_csv(data_second_major, "Data/Output/ivyplus_perstudent.csv")


data_perdegree = combined_data %>%
  mutate(
    cip_name = gsub("\\.", "", cip_name)
  ) %>%
  filter(
    cip_name != "'99'"
  ) %>%
  group_by(institution, year) %>%
  mutate(
    total_awards_year = sum(count[cip_name != "Economics"])
  )
write_csv(data_perdegree, "Data/Output/ivyplus_perdegree.csv")
