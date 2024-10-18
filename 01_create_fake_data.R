library(tidyverse)
library(readxl)
library(here)
#functions-------------------------------------

fake_the_data <- function(num_events, max_obs){
  num_levels <- max_obs-2
  level_names <- paste("Level", 1:num_levels)
  event <- c("Registration", level_names, "Completion")
  event_date <- sample(possible_dates, size = max_obs)|>
    sort()
  tibble(event=event, event_date=event_date)|>
    head(n=num_events)|>
    filter(event_date<today())
}

possible_dates <- seq.Date(floor_date(today()-years(8), unit = "month"),
                           floor_date(today()+years(1), unit = "month"),
                           by = "m")

which_trades<- read_excel(here("data",
                               "New_Apprenticeship_Registrations_May_2024.xlsx"),
                          na = "NULL")|>
  select(Trade, STC_Trades, Functional_Trades_Group)|>
  filter(STC_Trades=="Y"| Functional_Trades_Group=="Structural Building Trades")|>
  distinct()

write_csv(which_trades, here("out","which_trades.csv"))

stc_plus_sbt <- which_trades|>
  pull(Trade)

trade_desc <- rep(stc_plus_sbt, 100)
unique_key <- seq(1:length(trade_desc))

tbbl <- tibble(unique_key=unique_key, trade_desc=trade_desc)

#get the levels for each trade--------------------------------

max_observations <- read_excel(here("data",
                           "Apprenticeship Technical Training Levels by Trade (May2024).xlsx"))|>
  filter(Trade_Desc %in% stc_plus_sbt)|>
  select(trade_desc=Trade_Desc, max_obs=`Number of Technical Training Levels`)|>
  mutate(max_obs=max_obs+2) # add in the registration date and the completion date

write_csv(max_observations, here("out","max_observations.csv"))

full_join(tbbl, max_observations)|>
  group_by(unique_key)|>
  mutate(num_events=sample(c(1:max_obs, rep(max_obs,3)), size=1),
         events=map2(num_events, max_obs, fake_the_data),
         last_observed=floor_date(today(), unit="month")
         )|>
  select(-max_obs, -num_events)|>
  unnest(events)|>
  write_csv(here("out","fake_data.csv"))



