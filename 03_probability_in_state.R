library(tidyverse)
library(conflicted)
library(here)
library(survival)
library(tsibble)
conflicts_prefer(lubridate::interval)
conflicts_prefer(dplyr::filter)
source("functions.R")

time_and_status <- function(tbbl, start, end){#overwrite this function to NOT select era (it is grouping variable)
  tbbl|>
    mutate(status=case_when(is.na(get(end)) & !is.na(get(start))~0,
                            !is.na(get(end))~1,
                            is.na(get(end)) & is.na(get(start))~ NA_real_),
           time=case_when(is.na(get(end)) & !is.na(get(start)) ~ interval(get(start), last_observed),
                          !is.na(get(end)) ~ interval(get(start), get(end)),
                          is.na(get(end)) & is.na(get(start))~ NA),
           time= time %/% months(1)
    )|>
    select(trade_desc, status, time)
}

long_with_eras <- read_rds(here("out", "long_with_eras.rds"))
third_era <- unique(long_with_eras$era)[str_detect(unique(long_with_eras$era),
                                                   as.character(max(ymd(str_sub(unique(long_with_eras$era),
                                                                                -10)))))]



time_from_reg <- long_with_eras|>
  filter(era==third_era)|>
  group_by(technical_training_level, era)|>
  nest()|>
  mutate(data=map(data, ~pivot_wider(.x, names_from = event, values_from = event_date)),
         # l1_before_reg=map_lgl(data, check_sequential, reg_date, level1_date),
         # l2_before_l1=map_lgl(data, check_sequential, level1_date, level2_date),
         # l3_before_l2=map_lgl(data, check_sequential, level2_date, level3_date),
         # l4_before_l3=map_lgl(data, check_sequential, level3_date, level4_date),
         completion=map2(data, technical_training_level, level_time_and_status, "reg_date", "completion_date", ttl_gte=0),
         level1=map2(data, technical_training_level, level_time_and_status, "reg_date", "level1_date", ttl_gte=1),
         level2=map2(data, technical_training_level, level_time_and_status, "reg_date", "level2_date", ttl_gte=2),
         level3=map2(data, technical_training_level, level_time_and_status, "reg_date", "level3_date", ttl_gte=3),
         level4=map2(data, technical_training_level, level_time_and_status, "reg_date", "level4_date", ttl_gte=4)
  )|>
  ungroup()|>
  select(-data)|>
  pivot_longer(cols = -c(technical_training_level, era), names_to = "level", values_to = "data")|>
  unnest(data)|>
  select(level, era, trade_desc, status, time)

#'Note that it is possible for state probabilities to be negative if the
#'survival curves have large steps, which is typical when sample size small...
#'make plots for only the trades that have non-negative state probabilities.

temp <- time_from_reg|>
  group_by(trade_desc, level, era)|>
  nest()|>
  mutate(km_model=map(data, survfit_wrapper),
         time_and_surv=map(km_model, get_time_and_surv))|>
  select(level, trade_desc, time_and_surv, era)|>
  unnest(time_and_surv)|>
  group_by(trade_desc, era)|>
  nest()|>
  mutate(data=map(data, pivot_wrapper),
         ncols=map_dbl(data, ncol))|>
  mutate(probs=map2(data, ncols, get_probs_wrapper),
         probs_long=map(probs, make_long),
         min_prob=map_dbl(probs_long, get_min_prob))|>
  filter(min_prob>=0)|>
  mutate(state_prob_plot=map2(probs_long, era, make_state_prob_plot))

temp|>
  select(trade_desc, state_prob_plot, era)|>
  write_rds(here("out","state_prob_plots.rds"))





