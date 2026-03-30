library(tidyverse)
library(janitor)
library(readxl)

rm(list = ls())
setwd("/Users/arijacob/Documents/Maroon Article")


raw_2024_2023 = read_xlsx("Data/Excel/2023_2024_major.xlsx") %>%
  clean_names()

cleaned_2024_2023 = raw_2024_2023 %>%
  slice(-1) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "-", "0"))) %>%
  pivot_longer(
    cols = c("bachelor", "postgraduate", "master", "doctor"),
    names_to = "award_level",
    values_to = "count"
  ) %>%
  mutate(
    year = "2023-2024"
  ) %>%
  rename(cip_name = program)
  

raw_2023_2022 = read_csv("Data/Excel/2023_2022_major.csv") %>%
  clean_names()

cleaned_2023_2022 = raw_2023_2022 %>%
  mutate(across(everything(), as.character)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Total", "Women")) {
        c(NA, NA, NA, head(row_vals, -2))  # shift right within same width
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2023_2022)))%>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2023_2022)) %>%
  select(-16) %>%
  mutate(
    is_code = !is.na(cip_code) & grepl("^[0-9]", cip_code),
    cip_name_val = lead(cip_code) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cip_code %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code) %>%
  drop_na(gender) %>%
  fill(award_level, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program)%>%
  pivot_longer(
    cols = c(5:14),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award_level = trimws(str_remove_all(award_level, "[0-9-]")),
    year = "2022-2023"
  )

raw_2022_2021 = read_csv("Data/Excel/2022_2021_major.csv") %>%
  clean_names()

cleaned_2022_2021 = raw_2022_2021 %>%
  mutate(across(everything(), as.character)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Total", "Women")) {
        c(NA, NA, NA, head(row_vals, -2))  # shift right within same width
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2022_2021)))%>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2022_2021)) %>%
  select(-16) %>%
  mutate(
    is_code = !is.na(cip_code) & grepl("^[0-9]", cip_code),
    cip_name_val = lead(cip_code) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cip_code %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code) %>%
  drop_na(gender) %>%
  fill(award_level, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program)%>%
  pivot_longer(
    cols = c(5:14),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award_level = trimws(str_remove_all(award_level, "[0-9-]")),
    year = "2021-2022"
  )

raw_2021_2020 = read_csv("Data/Excel/2021_2020_major.csv") %>%
  clean_names()

cleaned_2021_2020 = raw_2021_2020 %>%
  mutate(across(everything(), as.character)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Total", "Women")) {
        c(NA, NA, NA, head(row_vals, -2))  # shift right within same width
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2021_2020)))%>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2021_2020)) %>%
  select(-16) %>%
  mutate(
    is_code = !is.na(cip_code) & grepl("^[0-9]", cip_code),
    cip_name_val = lead(cip_code) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cip_code %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code) %>%
  drop_na(gender) %>%
  fill(award_level, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program)%>%
  pivot_longer(
    cols = c(5:14),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award_level = trimws(str_remove_all(award_level, "[0-9-]")),
    year = "2020-2021"
  )

raw_2020_2019 = read_csv("Data/Excel/2020_2019_major.csv") %>%
  clean_names()

cleaned_2020_2019 = raw_2020_2019 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women " , "Total ")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2020_2019))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2020_2019)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cip_code) & grepl("^[0-9]", cip_code),
    cip_name_val = lead(cip_code) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cip_code %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code) %>%
  drop_na(gender) %>%
  fill(award_level, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program)%>%
  pivot_longer(
    cols = c(5:14),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award_level = trimws(str_remove_all(award_level, "[0-9-]")),
    year = "2019-2020"
  )

raw_2019_2018 = read_csv("Data/Excel/2019_2018_major.csv") %>%
  clean_names()

cleaned_2019_2018 = raw_2019_2018 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2019_2018))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2019_2018)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1)%>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program) %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2018-2019"
  )


raw_2018_2017 = read_csv("Data/Excel/2018_2017_major.csv") %>%
  clean_names()

cleaned_2018_2017 = raw_2018_2017 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2018_2017))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2018_2017)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program)%>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2017-2018"
  )

raw_2017_2016 = read_csv("Data/Excel/2017_2016_major.csv") %>%
  clean_names()

cleaned_2017_2016 = raw_2017_2016 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2017_2016))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2017_2016)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program) %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2016-2017"
  )

raw_2016_2015 = read_csv("Data/Excel/2016_2015_major.csv") %>%
  clean_names()

cleaned_2016_2015 = raw_2016_2015 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2016_2015))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2016_2015)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  )%>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program) %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2015-2016"
  )


raw_2015_2014 = read_csv("Data/Excel/2015_2014_major.csv") %>%
  clean_names()

cleaned_2015_2014 = raw_2015_2014 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2016_2015))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2016_2015)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program) %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2014-2015"
  )


raw_2014_2013 = read_csv("Data/Excel/2014_2013_major.csv") %>%
  clean_names()

cleaned_2014_2013 = raw_2014_2013 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2014_2013))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2014_2013)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program) %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2013-2014"
  )


raw_2013_2012 = read_csv("Data/Excel/2013_2012_major.csv") %>%
  clean_names()

cleaned_2013_2012 = raw_2013_2012 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2013_2012))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2013_2012)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program) %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2012-2013"
  )


raw_2012_2011 = read_csv("Data/Excel/2012_2011_major.csv") %>%
  clean_names()

cleaned_2012_2011 = raw_2012_2011 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2012_2011))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2012_2011)) %>%
  select(-c(16:17))%>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  select(-distance_education_program) %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2011-2012"
  )


raw_2011_2010 = read_csv("Data/Excel/2011_2010_major.csv") %>%
  clean_names()

cleaned_2011_2010 = raw_2011_2010 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1)) 
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2011_2010))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2011_2010)) %>%
  select(-c(15:16)) %>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code),
    award = trimws(str_remove_all(award, "[0-9-]")),
    award = if_else(award == "", NA_character_, award),
    award = lead(award)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    year = "2010-2011"
  )


raw_2010_2009 = read_csv("Data/Excel/2010_2009_major.csv",skip = 1 ) %>%
  clean_names() %>%
  select(-x2)

cleaned_2010_2009 = raw_2010_2009 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2010_2009))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2010_2009)) %>%
  select(-c(15:16)) %>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    award = if_else(award == "", NA_character_, award)
  ) %>%
  # shift up award by one
  mutate(award = lead(award))%>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  pivot_longer(
    cols = c(4:13),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2009-2010"
  )


raw_2009_2008 = read_xlsx("Data/Excel/2009_2008_major.xlsx") %>%
  clean_names() 

cleaned_2009_2008 = raw_2009_2008 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2009_2008))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2009_2008)) %>%
  select(-c(13:14)) %>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    award = if_else(award == "", NA_character_, award)
  ) %>%
  # shift up award by one
  mutate(award = lead(award)) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2008-2009",
    cip_code = str_sub(cip_code, 1, 6)
  )


raw_2008_2007 = read_xlsx("Data/Excel/2008_2007_major.xlsx") %>%
  clean_names() 

cleaned_2008_2007 = raw_2008_2007 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2008_2007))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2008_2007)) %>%
  select(-c(13:14)) %>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    award = if_else(award == "", NA_character_, award)
  ) %>%
  # shift up award by one
  mutate(award = lead(award)) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2007-2008",
    cip_code = str_sub(cip_code, 1, 6)
  )


raw_2007_2006 = read_xlsx("Data/Excel/2007_2006_major.xlsx") %>%
  clean_names() 

cleaned_2007_2006 = raw_2007_2006 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2007_2006))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2007_2006)) %>%
  select(-c(13:14)) %>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    award = if_else(award == "", NA_character_, award)
  ) %>%
  # shift up award by one
  mutate(award = lead(award)) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2006-2007",
    cip_code = str_sub(cip_code, 1, 6)
  )

raw_2006_2005 = read_xlsx("Data/Excel/2006_2005_major.xlsx") %>%
  clean_names() 

cleaned_2006_2005 = raw_2006_2005 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2006_2005))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2006_2005)) %>%
  select(-c(13:14)) %>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  slice(-1) %>%
  mutate(
    award = trimws(str_remove_all(award, "[0-9-]")),
    award = if_else(award == "", NA_character_, award)
  ) %>%
  # shift up award by one
  mutate(award = lead(award)) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2005-2006",
    cip_code = str_sub(cip_code, 1, 6)
  )


raw_2005_2004 = read_csv("Data/Excel/2005_2004_major.csv", skip= 2) %>%
  clean_names() 

cleaned_2005_2004 = raw_2005_2004 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2005_2004))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2005_2004)) %>%
  select(-c(13:15)) %>%
  mutate(
    is_code = !is.na(cipcode) & grepl("^[0-9]", cipcode),
    cip_name_val = lead(cipcode) %>%
      if_else(!grepl("^[0-9]", .), ., NA_character_),
    cip_code_val = cipcode %>%
      if_else(grepl("^[0-9]", .), ., NA_character_)
  ) %>%
  mutate(
    award_level = trimws(str_remove_all(award_level, "[0-9-]")),
    award_level = if_else(award_level == "", NA_character_, award_level)
  ) %>%
  # shift up award by one
  mutate(award_level = lead(award_level)) %>%
  mutate(
    cip_name = zoo::na.locf(cip_name_val, fromLast = FALSE, na.rm = FALSE),
    cip_code = zoo::na.locf(cip_code_val, fromLast = FALSE, na.rm = FALSE),
    group = cumsum(is_code)
  ) %>%
  select(-cip_name_val, -cip_code_val, -is_code, -cipcode) %>%
  drop_na(gender) %>%
  fill(award_level, .direction = "down") %>%
  fill(major, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2004-2005",
    cip_code = str_sub(cip_code, 1, 6)
  )


raw_2004_2003 = read_csv("Data/Excel/2004_2003_major.csv", skip = 3) %>%
  clean_names() 

cleaned_2004_2003 = raw_2004_2003 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2004_2003))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2004_2003)) %>%
  select(-c(12)) %>%
  slice(-c(1:2)) %>%
  mutate(
    # everything after first 8 characters of cip code
    cip_name = str_trim(str_sub(cip_code, 9, nchar(cip_code))),
    cip_code = str_trim(str_sub(cip_code, 1, 8))
  ) %>%
  fill(award_level, .direction = "down") %>%
  fill(cip_code, .direction = "down") %>%
  fill(cip_name, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2003-2004",
    cip_code = str_sub(cip_code, 1, 6),
    race = if_else(race == "nonres", "non_resident_alien", race),
    race = if_else(race == "race", "race_unknown", race)
  )

raw_2003_2002 = read_csv("Data/Excel/2003_2002_major.csv", skip = 3) %>%
  clean_names() 

cleaned_2003_2002 = raw_2003_2002 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2003_2002))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2003_2002)) %>%
  select(-c(12)) %>%
  slice(-c(1:2)) %>%
  mutate(
    # everything after first 8 characters of cip code
    cip_name = str_trim(str_sub(cip_code, 9, nchar(cip_code))),
    cip_code = str_trim(str_sub(cip_code, 1, 8))
  ) %>%
  fill(award_level, .direction = "down") %>%
  fill(cip_code, .direction = "down") %>%
  fill(cip_name, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2002-2003",
    cip_code = str_sub(cip_code, 1, 6),
    race = if_else(race == "nonres", "non_resident_alien", race),
    race = if_else(race == "race", "race_unknown", race)
  )

raw_2002_2001 = read_csv("Data/Excel/2002_2001_major.csv", skip = 3) %>%
  clean_names() 

cleaned_2002_2001 = raw_2002_2001 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2002_2001))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2002_2001)) %>%
  select(-c(12)) %>%
  slice(-c(1:2)) %>%
  mutate(
    # everything after first 8 characters of cip code
    cip_name = str_trim(str_sub(cip_code, 9, nchar(cip_code))),
    cip_code = str_trim(str_sub(cip_code, 1, 8))
  ) %>%
  fill(award_level, .direction = "down") %>%
  fill(cip_code, .direction = "down") %>%
  fill(cip_name, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2001-2002",
    cip_code = str_sub(cip_code, 1, 6),
    race = if_else(race == "nonres", "non_resident_alien", race),
    race = if_else(race == "race", "race_unknown", race)
  )

raw_2001_2000 = read_csv("Data/Excel/2001_2000_major.csv", skip = 3) %>%
  clean_names() 

cleaned_2001_2000 = raw_2001_2000 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), trimws)) %>%
  rowwise() %>%
  mutate(
    shifted = list({
      row_vals <- c_across(everything())
      if (row_vals[1] %in% c("Women" , "Total")) {
        c(NA, NA, head(row_vals, -1))
      } else {
        row_vals
      }
    })
  ) %>%
  ungroup() %>%
  select(-all_of(names(raw_2001_2000))) %>%  # drop originals
  unnest_wider(shifted, names_sep = "_") %>%  # expand safely
  setNames(names(raw_2001_2000)) %>%
  select(-c(12)) %>%
  slice(-c(1:2)) %>%
  mutate(
    # everything after first 8 characters of cip code
    cip_name = str_trim(str_sub(cip_code, 9, nchar(cip_code))),
    cip_code = str_trim(str_sub(cip_code, 1, 8))
  ) %>%
  fill(award_level, .direction = "down") %>%
  fill(cip_code, .direction = "down") %>%
  fill(cip_name, .direction = "down") %>%
  pivot_longer(
    cols = c(4:11),
    names_to = "race",
    values_to = "count"
  ) %>%
  mutate(
    year = "2000-2001",
    cip_code = str_sub(cip_code, 1, 6),
    race = if_else(race == "nonres", "non_resident_alien", race),
    race = if_else(race == "race", "race_unknown", race)
  )

# write all dataframes that start with cleaned
list_of_dfs = ls(pattern = "^cleaned_")
for (df_name in list_of_dfs) {
  df = get(df_name)
  write_csv(df, paste0("Data/Cleaned/", df_name, ".csv"))
}

