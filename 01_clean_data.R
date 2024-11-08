#libraries--------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(conflicted)
conflicts_prefer(dplyr::filter)
#functions---------------
source("functions.R")
#read in the data------------------------------
wide <- read_excel(here("data",
                             "Apprenticeship Data of Registration and Level Dates.xlsx"),
                        sheet = "Data",
                        na = "NULL")|>
  clean_names()|>
  rename(reg_date=registration_start_date,
         completion_date=registration_complete_date)

last_observed <- wide|>
  summarize(across(contains("date"), ~max(., na.rm = TRUE)))|>
  pivot_longer(cols = everything())|>
  summarize(value=max(value))|>
  pull(value)

#make sure no level dates precede registration date
real_corrected <- wide|>
  mutate(level1_date=if_else(level1_date<reg_date, reg_date, level1_date),
         level2_date=if_else(level2_date<reg_date, reg_date, level2_date),
         level3_date=if_else(level3_date<reg_date, reg_date, level3_date),
         level4_date=if_else(level4_date<reg_date, reg_date, level4_date)
  )|>
  select(-program_name, -registration_status_desc)|>
  mutate(last_observed=floor_date(ymd(last_observed), unit="month"))


#correct the missing dates recursively backwards.
real_corrected|>
  mutate(level4_date=pmap_dbl(list(reg_date,
                               level4_date,
                               completion_date,
                               completion_date,
                               technical_training_level),
                          missing_to_reg_date, ttl_gte=4),
         level3_date=pmap_dbl(list(reg_date,
                               level3_date,
                               level4_date,
                               completion_date,
                               technical_training_level),
                          missing_to_reg_date, ttl_gte=3),
         level2_date=pmap_dbl(list(reg_date,
                               level2_date,
                               level3_date,
                               completion_date,
                               technical_training_level),
                          missing_to_reg_date, ttl_gte=2),
         level1_date=pmap_dbl(list(reg_date,
                               level1_date,
                               level2_date,
                               completion_date,
                               technical_training_level),
                          missing_to_reg_date, ttl_gte=1)
         )|>
  filter((level2_date>=level1_date) %>% replace_na(TRUE), #about 1.8% of records have non-sequential level dates: drop them
         (level3_date>=level2_date) %>% replace_na(TRUE),
         (level4_date>=level3_date) %>% replace_na(TRUE),
         (completion_date>=level4_date) %>% replace_na(TRUE))|>
  mutate(across(contains("date"), ~floor_date(ymd(.), unit="month")))|>
  write_csv(here("out","real_data.csv"))

