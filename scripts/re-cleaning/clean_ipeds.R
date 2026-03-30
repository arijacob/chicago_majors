library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

rm(list = ls())

setwd("/Users/arijacob/Documents/GitHub/chicago_majors")

major_number_dir <- "data/ipeds/raw/major_numbers"
var_names_dir <- "data/ipeds/raw/var_names"

var_labels_files <- list.files(
  path = var_names_dir,
  pattern = "\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

colnames_data <- map_dfr(
  var_labels_files,
  ~ read_xlsx(.x, sheet = 2) %>%
    mutate(
      source_file = basename(.x),
      year = str_extract(basename(.x), "\\d{4}"),
      
    )
)

var_lookup = colnames_data %>%
  select(year, varname, varTitle)

# find all matching csv files
numbers_files <- list.files(
  path = major_number_dir,
  pattern = "_[rR][vV].*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)


read_and_bind_rv <- function(files, var_lookup) {
  
  map_dfr(files, function(f) {
    
    year_i <- str_extract(f, "\\d{4}") %>% as.integer()
    print(year_i)
    df <- read_csv(f, show_col_types = FALSE)
    
    rename_map <- var_lookup %>%
      filter(year == year_i) %>%
      distinct(varTitle, varname) %>%
      deframe()
    
    df <- df %>%
      mutate(
        year = year_i,
        source_file = basename(f)
      )
    
    if (year_i <= 2007) {
      df = df %>%
        select(year, UNITID, CIPCODE,AWLEVEL, MAJORNUM, CRACE24) %>%
        rename(
          school_id = UNITID,
          cipcode = CIPCODE,
          award_level = AWLEVEL,
          major_number = MAJORNUM,
          total = CRACE24
        )
    } else {
      df = df %>%
        select(year, UNITID, CIPCODE, AWLEVEL, MAJORNUM, CTOTALT) %>%
        rename(
          school_id = UNITID,
          cipcode = CIPCODE,
          award_level = AWLEVEL,
          major_number = MAJORNUM,
          total = CTOTALT
        ) %>%
        mutate(
          award_level = as.character(award_level) 
        )
    }
    
    df
  })
}

characteristics = read_csv("data/ipeds/raw/characteristics/hd2019.csv") %>%
  clean_names() %>%
  select(unitid, instnm)

major_numbers_data <- read_and_bind_rv(
  files = numbers_files,
  var_lookup = var_lookup
)

completitions_final = major_numbers_data %>%
  left_join(characteristics, by = c("school_id" = "unitid")) %>%
  filter(
    instnm %in% c(
      "University of Chicago",
      "Harvard University",
      "University of Pennsylvania",
      "Princeton University",
      "Brown University",
      "Yale University",
      "Columbia University in the City of New York",
      "Dartmouth College",
      "Cornell University",
      "Stanford University",
      "Massachusetts Institute of Technology",
      "Duke University"
    ),
    cipcode != 99,
    award_level == "5" | award_level == "05"
  ) %>%
  select(-award_level)

write_csv(completitions_final, "data/ipeds/cleaned/major_numbers.csv")