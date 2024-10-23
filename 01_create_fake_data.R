#libraries-----------------------
library(tidyverse)
library(readxl)
library(here)
#constants---------------------
nobs <- 320 # number of observations per trade
noise <- 1 # to investigate how noise influences results
current_month <- floor_date(today(), unit = "month")
possible_reg <- seq.Date(ymd("2016/01/01"), current_month-years(6), by = "month") #ensures dates before current month
#functions------------------------
add_level_dates <- function(max_obs, tbbl){
  num_levels <- max_obs-2
  tbbl|>
    mutate(level1_date=if_else(unique_key %% 2 == 0, reg_date+years(1), NA),
           level2_date=if_else(unique_key %% 4 == 0 & num_levels>1, reg_date+years(2), NA),
           level3_date=if_else(unique_key %% 8 == 0 & num_levels>2, reg_date+years(3), NA),
           level4_date= if_else(unique_key %% 16==0 & num_levels>3, reg_date+years(4), NA)
    )
}
add_completion_date <- function(max_obs, tbbl){
  divide_by <- 2^(max_obs-1)
  add_years <- max_obs-1
  tbbl|>
    mutate(completion_date=if_else(unique_key %% divide_by==0, reg_date+years(add_years), NA))
}
#read data-----------------------
stc_plus_sbt<- read_excel(here("data", "New_Apprenticeship_Registrations_May_2024.xlsx"), na = "NULL")|>
  select(Trade, STC_Trades, Functional_Trades_Group)|>
  filter(STC_Trades=="Y"| Functional_Trades_Group=="Structural Building Trades")|>
  distinct()|>
  pull(Trade)

max_observations <- read_excel(here("data", "Apprenticeship Technical Training Levels by Trade (May2024).xlsx"))|>
  filter(Trade_Desc %in% stc_plus_sbt)|>
  select(trade_desc=Trade_Desc, max_obs=`Number of Technical Training Levels`)|>
  mutate(max_obs=max_obs+2) # add in the registration date and the completion date

write_csv(max_observations, here("out","max_observations.csv"))

#create fake data--------------------
temp <- tibble(unique_key = seq(1:(nobs*length(stc_plus_sbt))),
               trade_desc = rep(stc_plus_sbt, nobs),
               reg_date = sample(possible_reg, size=length(stc_plus_sbt)*nobs, replace = TRUE),
               last_observed = current_month
               )|>
  full_join(max_observations)|>
  group_by(max_obs)|>
  nest()|>
  mutate(data=map2(max_obs, data, add_level_dates),
         data=map2(max_obs, data, add_completion_date))|>
  unnest(data)|>
  ungroup()|>
  select(-max_obs)

temp <- temp|>
  mutate(level1_date=level1_date+months(sample(c(-noise,noise), size=nrow(temp), replace=TRUE)),
         level2_date=level2_date+months(sample(c(-noise,noise), size=nrow(temp), replace=TRUE)),
         level3_date=level3_date+months(sample(c(-noise,noise), size=nrow(temp), replace=TRUE)),
         level4_date=level4_date+months(sample(c(-noise,noise), size=nrow(temp), replace=TRUE))
  )

write_csv(temp, here("out","fake_data.csv"))



