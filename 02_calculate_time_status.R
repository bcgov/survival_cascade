library(tidyverse)
library(here)
#functions
fix_dates <- function(tbbl){
  #'this function addresses issue that levels prior to registration may have event_date that are either missing
  #'or prior to registration... in either case we set them equal to the registration date.
  tbbl|>
    mutate(event_date=if_else(is.na(event_date),
                              tbbl$event_date[tbbl$event=="Registration"],
                              event_date),
           event_date=if_else(event_date<tbbl$event_date[tbbl$event=="Registration"],
                              tbbl$event_date[tbbl$event=="Registration"],
                              event_date)
           )
}
level_time_and_status <- function(tbbl, max_obs, start, end, greater_than){
  #'this function first tests to see if there is a level for the trade...
  #'if so it calls function to calculate the status and time
  if(max_obs>greater_than){
    time_and_status(tbbl, start, end)
  }else{
    #' if not it returns an empty tibble
    tibble()
  }
}
time_and_status <- function(tbbl, start, end){
  #browser()
  tbbl|>
    mutate(status=case_when(is.na(get(end)) & !is.na(get(start))~0,
                            !is.na(get(end))~1,
                            is.na(get(end)) & is.na(get(start))~ NA_real_),
           time=case_when(is.na(get(end)) & !is.na(get(start)) ~ interval(get(start), last_observed),
                          !is.na(get(end)) ~ interval(get(start),get(end)),
                          is.na(get(end)) & is.na(get(start))~ NA),
           time= time %/% months(1)
    )|>
    select(trade_desc, status, time)|>
    na.omit()
}

#read in some fake data (for now)--------------------------
fake <- read_csv(here("out","fake_data.csv"))
#'Possible pre-apprenticeship levels are either missing their dates, or have dates prior to registration:
#'this replaces with registration date
tbbl <- fake|>
  group_by(unique_key, trade_desc)|>
  nest()|>
  mutate(data=map(data, fix_dates))|>
  unnest(data)
#calculate the time and status for the various levels----------------------------------
nested <- tbbl|>
  full_join(read_csv(here("out","max_observations.csv")))|>
  group_by(max_obs)|>
  nest()|>
  mutate(data=map(data, ~pivot_wider(.x, names_from = event, values_from = event_date)),
         level1=map2(data, max_obs, level_time_and_status, "Registration", "Level 1", 2),
         level2=map2(data, max_obs, level_time_and_status, "Level 1", "Level 2", 3),
         level3=map2(data, max_obs, level_time_and_status, "Level 2", "Level 3", 4),
         level4=map2(data, max_obs, level_time_and_status, "Level 3", "Level 4", 5)
         )

nested <- nested|>
  full_join(tibble(max_obs=3:6,
                   completion=list(time_and_status(nested$data[nested$max_obs==3][[1]], "Level 1", "Completion"),
                   time_and_status(nested$data[nested$max_obs==4][[1]], "Level 2", "Completion"),
                   time_and_status(nested$data[nested$max_obs==5][[1]], "Level 3", "Completion"),
                   time_and_status(nested$data[nested$max_obs==6][[1]], "Level 4", "Completion")
                   )
                   )
            )|>
  ungroup()|>
  select(-max_obs, -data)

level1 <- nested|>
  select(level1)|>
  unnest(level1)|>
  group_by(trade_desc)|>
  nest()

level2 <- nested|>
  select(level2)|>
  unnest(level2)|>
  group_by(trade_desc)|>
  nest()

level3 <- nested|>
  select(level3)|>
  unnest(level3)|>
  group_by(trade_desc)|>
  nest()

level4 <- nested|>
  select(level4)|>
  unnest(level4)|>
  group_by(trade_desc)|>
  nest()

completion <- nested|>
  select(completion)|>
  unnest(completion)|>
  group_by(trade_desc)|>
  nest()





