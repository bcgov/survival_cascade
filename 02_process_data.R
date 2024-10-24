#libraries-------------------
library(tidyverse)
library(here)
library(survival)
library(survminer)
#functions-------------------------------
fix_dates <- function(tbbl){
  #'this function addresses issue that levels prior to registration may have event_date
  #'prior to registration... set equal to the registration date.
  tbbl|>
    mutate(event_date=if_else(event_date<tbbl$event_date[tbbl$event=="reg_date"],
                              tbbl$event_date[tbbl$event=="reg_date"],
                              event_date)
           )
}

level_time_and_status <- function(tbbl, max_obs, start, end, cutoff){
  #'this function tests to see if there is a level for the trade...
  #'if so it calls function time_and_status
  if(max_obs>cutoff){
    time_and_status(tbbl, start, end)
  }else{
    #' otherwise it returns an empty tibble
    tibble()
  }
  #'e.g. a one-level trade has max_obs=3 (registration, level_1, completion)
  #'so when cutoff=3 we calculate time and status only for trades that have 2 or more levels.
}

time_and_status <- function(tbbl, start, end){
  temp <- tbbl|>
    mutate(status=case_when(is.na(get(end)) & !is.na(get(start))~0,
                            !is.na(get(end))~1,
                            is.na(get(end)) & is.na(get(start))~ NA_real_),
           time=case_when(is.na(get(end)) & !is.na(get(start)) ~ interval(get(start), last_observed),
                          !is.na(get(end)) ~ interval(get(start),get(end)),
                          is.na(get(end)) & is.na(get(start))~ NA),
           time= time %/% months(1)
    )

  # |>
  #   select(trade_desc, status, time)|>
  #   na.omit()
}

survfit_wrapper <- function(tbbl) {
  survfit(Surv(time, status) ~ 1, tbbl)
}

get_joint <- function(mod_lst) {
  # calculate the joint probability of survival thus far and completion now: P(S and C)= P(S)*P(C|S)
  # Data doesn't make sense in some cases (for our purposes)... code below fixes.
  if(mod_lst$cumhaz[1]!=0 | mod_lst$surv[1]!=1){ #if survival curve doesn't start at surv=1...
    mod_lst$cumhaz=c(0, mod_lst$cumhaz) #at time =0 cumulative hazard should be 0
    mod_lst$surv=c(1, mod_lst$surv) #at time=0 the survival probability should be 1.
    mod_lst$time=c(0, mod_lst$time) #at time=0 time should be... 0
  }
  cumhaz <- c(mod_lst$cumhaz, NA_real_)
  surv <- c(1, mod_lst$surv) #pushes down surv by one row relative to cumhaz and time
  time <- c(mod_lst$time, NA_real_)

  tibble(
    haz_rate = c(0, diff(cumhaz)),
    surv = surv,
    time = time
  ) |>
    mutate(joint = haz_rate * surv)|>
    na.omit()
}

expected_delay <- function(tbbl){
  # weighted mean of delay
  tbbl|>
    mutate(joint_sum_to_one=joint/sum(joint))|> #adjust probabilities for non-completions.
    summarize(mean_delay_year=sum(time*joint_sum_to_one)/12)|>
    pull(mean_delay_year)
}

#read in some fake data (for now)--------------------------

fake<- read_csv(here("out","fake_data.csv"))

#'Possible pre-apprenticeship levels have dates prior to registration: replaces with registration date

tbbl <- fake|>
  pivot_longer(cols=contains("date"), names_to = "event", values_to = "event_date")|>
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
         level1=map2(data, max_obs, level_time_and_status, "reg_date", "level1_date", cutoff=2), #max_obs>cutoff=2 for all trades
         level2=map2(data, max_obs, level_time_and_status, "level1_date", "level2_date", cutoff=3), #max_obs>cutoff=3 for some trades
         level3=map2(data, max_obs, level_time_and_status, "level2_date", "level3_date", cutoff=4), #max_obs>cutoff=4 for some trades
         level4=map2(data, max_obs, level_time_and_status, "level3_date", "level4_date", cutoff=5) #max_obs>cutoff=5 for some trades
         )

#calculate time and status for completion

t_and_s <- nested|>
  full_join(tibble(max_obs=3:6,
                   completion=list(time_and_status(nested$data[nested$max_obs==3][[1]], "level1_date", "completion_date"),
                   time_and_status(nested$data[nested$max_obs==4][[1]], "level2_date", "completion_date"),
                   time_and_status(nested$data[nested$max_obs==5][[1]], "level3_date", "completion_date"),
                   time_and_status(nested$data[nested$max_obs==6][[1]], "level4_date", "completion_date")
                   )
                   )
            )|>
  ungroup()|>
  select(-data)|>
  pivot_longer(cols = -max_obs, names_to = "level", values_to = "data")|>
  unnest(data)|>
  select(level, trade_desc, status, time)

#get the time and status for black box (the whole process)-------------------

black_box <- tbbl|>
  filter(event %in% c("reg_date", "completion_date"))|>
  pivot_wider(names_from = event, values_from = event_date)|>
  time_and_status("reg_date","completion_date")|>
  mutate(level="journeyperson")|>
  ungroup()|>
  select(level, trade_desc, status, time)

all_data <- bind_rows(t_and_s, black_box)

write_rds(all_data, here("out", "time_and_status.rds"))

nested_with_summary <- all_data|>
  group_by(level, trade_desc)|>
  nest()|>
  mutate(km_model=map(data, survfit_wrapper),
         surv_dat = map(km_model, get_joint),
         years = map_dbl(surv_dat, expected_delay),
         probability = map_dbl(surv_dat, function(x) 1-min(x$surv))
  )|>
  select(-data,-km_model)

nested_with_summary|>
  write_rds(here("out", "nested_with_summary.rds"))







