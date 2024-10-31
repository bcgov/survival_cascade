library(tidyverse)
library(conflicted)
library(here)
library(survival)
library(tsibble)
conflicts_prefer(lubridate::interval)
source("functions.R")

time_and_status <- function(tbbl, start, end){#overwrite this function to NOT select era
  tbbl|>
    mutate(status=case_when(is.na(get(end)) & !is.na(get(start))~0,
                            !is.na(get(end))~1,
                            is.na(get(end)) & is.na(get(start))~ NA_real_),
           time=case_when(is.na(get(end)) & !is.na(get(start)) ~ interval(get(start), last_observed),
                          !is.na(get(end)) ~ interval(get(start), get(end)),
                          is.na(get(end)) & is.na(get(start))~ NA),
           time= time %/% months(1)
    )|>
    select(trade_desc, status, time)|>
    na.omit()
}

long_corrected <- read_rds(here("out", "long_corrected.rds"))

time_from_reg <- long_corrected|>
  full_join(read_csv(here("out","max_observations.csv")))|>
  group_by(max_obs, era)|>
  nest()|>
  mutate(data=map(data, ~pivot_wider(.x, names_from = event, values_from = event_date)),
         completion=map2(data, max_obs, level_time_and_status, "reg_date", "completion_date", cutoff=2), #max_obs>cutoff=2 for all trades
         level1=map2(data, max_obs, level_time_and_status, "reg_date", "level1_date", cutoff=2), #max_obs>cutoff=2 for all trades
         level2=map2(data, max_obs, level_time_and_status, "reg_date", "level2_date", cutoff=3), #max_obs>cutoff=3 for some trades
         level3=map2(data, max_obs, level_time_and_status, "reg_date", "level3_date", cutoff=4), #max_obs>cutoff=4 for some trades
         level4=map2(data, max_obs, level_time_and_status, "reg_date", "level4_date", cutoff=5) #max_obs>cutoff=5 for some trades
  )|>
  ungroup()|>
  select(-data)|>
  pivot_longer(cols = -c(max_obs, era), names_to = "level", values_to = "data")|>
  unnest(data)|>
  select(level, era, trade_desc, status, time)

time_from_reg|>
  group_by(trade_desc, era, level)|>
  nest()|>
  mutate(km_model=map(data, survfit_wrapper),
         time_and_surv=map(km_model, get_time_and_surv)
  )|>
  select(level, trade_desc, era, time_and_surv)|>
  unnest(time_and_surv)|>
  group_by(trade_desc, era)|>
  nest()|>
  mutate(data=map(data, pivot_wrapper),
         ncols=map_dbl(data, ncol))|>
  mutate(probs=map2(data, ncols, get_probs_wrapper),
         probs_long=map(probs, make_long),
         state_prob_plot=map2(probs_long, era, make_state_prob_plot))|>
  select(trade_desc, era, state_prob_plot)|>
  write_rds(here("out","state_prob_plots.rds"))



