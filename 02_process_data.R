#libraries-------------------
library(tidyverse)
library(here)
library(survival)
library(survminer)
library(conflicted)
conflicts_prefer(lubridate::interval)
conflicts_prefer(dplyr::filter)
#functions-------------------------------
source("functions.R")
#read in the data----------------------------

real<- read_csv(here("out","real_data.csv"))

# split into eras

min_reg_date <- min(real$reg_date)
max_reg_date <- max(real$reg_date)
days_in_a_third <- round(as.numeric(max_reg_date-min_reg_date)/3)
one_third <- min_reg_date+days(days_in_a_third)
two_third <- max_reg_date-days(days_in_a_third)
first <- paste0("Registered: ",min_reg_date,"--",one_third)
third <- paste0("Registered: ", two_third,"--",max_reg_date)
second <- paste0("Registered: ",one_third+days(1),"--",two_third-days(1))

long_with_eras <- real|>
  mutate(era=case_when(reg_date<one_third ~ first,
                       reg_date>two_third ~ third,
                       TRUE ~ second)
  )|>
  pivot_longer(cols=contains("date"), names_to = "event", values_to = "event_date")

write_rds(long_with_eras, here("out","long_with_eras.rds"))
#calculate the time and status for the various levels----------------------------------

time_from_previous <- long_with_eras|>
  group_by(technical_training_level)|>
  nest()|>
  mutate(data=map(data, ~pivot_wider(.x, names_from = event, values_from = event_date)),
         level1=map2(data, technical_training_level, level_time_and_status, "reg_date", "level1_date", 1),
         level2=map2(data, technical_training_level, level_time_and_status, "level1_date", "level2_date", 2),
         level3=map2(data, technical_training_level, level_time_and_status, "level2_date", "level3_date", 3),
         level4=map2(data, technical_training_level, level_time_and_status, "level3_date", "level4_date", 4)
         )

time_from_previous <- time_from_previous|>
  full_join(tibble(technical_training_level=1:4,
                   completion=list(time_and_status(time_from_previous$data[time_from_previous$technical_training_level==1][[1]], "level1_date", "completion_date"),
                                   time_and_status(time_from_previous$data[time_from_previous$technical_training_level==2][[1]], "level2_date", "completion_date"),
                                   time_and_status(time_from_previous$data[time_from_previous$technical_training_level==3][[1]], "level3_date", "completion_date"),
                                   time_and_status(time_from_previous$data[time_from_previous$technical_training_level==4][[1]], "level4_date", "completion_date")
                   )
                   )
            )|>
  ungroup()|>
  select(-data)|>
  pivot_longer(cols = -technical_training_level, names_to = "level", values_to = "data")|>
  unnest(data)|>
  select(level, trade_desc, era, status, time)

#get the time and status for black box (the whole process)-------------------

black_box <- long_with_eras|>
  filter(event %in% c("reg_date", "completion_date"))|>
  pivot_wider(names_from = event, values_from = event_date)|>
  time_and_status("reg_date","completion_date")|>
  mutate(level="journeyperson")|>
  ungroup()|>
  select(level, trade_desc, era, status, time)

time_from_previous <- bind_rows(time_from_previous, black_box)

write_rds(time_from_previous, here("out", "time_and_status.rds"))

time_from_previous|>
  group_by(level, era, trade_desc)|>
  nest()|>
  mutate(km_model=map(data, survfit_wrapper),
         surv_dat = map(km_model, get_joint),
         years = map_dbl(surv_dat, expected_delay),
         probability = map_dbl(surv_dat, function(x) 1-min(x$surv))
  )|>
  select(-data,-km_model)|>
  write_rds(here("out", "nested_with_summary.rds"))



