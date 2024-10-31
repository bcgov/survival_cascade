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
#read in some fake data (for now)--------------------------

fake<- read_csv(here("out","fake_data.csv"))

# split into eras

min_reg_date <- min(fake$reg_date)
max_reg_date <- max(fake$reg_date)
days_in_a_third <- as.numeric(max_reg_date-min_reg_date)/3
one_third <- min_reg_date+days(days_in_a_third)
two_third <- max_reg_date-days(days_in_a_third)

#'Possible pre-apprenticeship levels have dates prior to registration: replaces with registration date

long_corrected <- fake|>
  mutate(era=case_when(reg_date<one_third ~ "first",
                       reg_date>two_third ~ "third",
                       TRUE ~ "second")
  )|>
  pivot_longer(cols=contains("date"), names_to = "event", values_to = "event_date")|>
  group_by(unique_key, trade_desc)|>
  nest()|>
  mutate(data=map(data, fix_dates))|>
  unnest(data)

write_rds(long_corrected, here("out","long_corrected.rds"))
#calculate the time and status for the various levels----------------------------------

time_from_previous <- long_corrected|>
  full_join(read_csv(here("out","max_observations.csv")))|>
  group_by(max_obs)|>
  nest()|>
  mutate(data=map(data, ~pivot_wider(.x, names_from = event, values_from = event_date)),
         level1=map2(data, max_obs, level_time_and_status, "reg_date", "level1_date", cutoff=2), #max_obs>cutoff=2 for all trades
         level2=map2(data, max_obs, level_time_and_status, "level1_date", "level2_date", cutoff=3), #max_obs>cutoff=3 for some trades
         level3=map2(data, max_obs, level_time_and_status, "level2_date", "level3_date", cutoff=4), #max_obs>cutoff=4 for some trades
         level4=map2(data, max_obs, level_time_and_status, "level3_date", "level4_date", cutoff=5) #max_obs>cutoff=5 for some trades
         )

time_from_previous <- time_from_previous|>
  full_join(tibble(max_obs=3:6,
                   completion=list(time_and_status(time_from_previous$data[time_from_previous$max_obs==3][[1]], "level1_date", "completion_date"),
                   time_and_status(time_from_previous$data[time_from_previous$max_obs==4][[1]], "level2_date", "completion_date"),
                   time_and_status(time_from_previous$data[time_from_previous$max_obs==5][[1]], "level3_date", "completion_date"),
                   time_and_status(time_from_previous$data[time_from_previous$max_obs==6][[1]], "level4_date", "completion_date")
                   )
                   )
            )|>
  ungroup()|>
  select(-data)|>
  pivot_longer(cols = -max_obs, names_to = "level", values_to = "data")|>
  unnest(data)|>
  select(level, trade_desc, era, status, time)

#get the time and status for black box (the whole process)-------------------

black_box <- long_corrected|>
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



