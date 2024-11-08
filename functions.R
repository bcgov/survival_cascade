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
  divide_by <- 2^(max_obs-1) #e.g. if max_obs=4 (2 levels), divide_by=8 -> half of those who did level 2 complete
  add_years <- max_obs-1 #if they complete, completion occurs 1 year after final level.
  tbbl|>
    mutate(completion_date=if_else(unique_key %% divide_by==0, reg_date+years(add_years), NA))
}

fix_dates <- function(tbbl){
  #'this function addresses issue that levels prior to registration may have event_date
  #'prior to registration... set equal to the registration date.
  tbbl|>
    mutate(event_date=if_else(event_date<tbbl$event_date[tbbl$event=="reg_date"],
                              tbbl$event_date[tbbl$event=="reg_date"],
                              event_date)
    )
}

level_time_and_status <- function(tbbl, ttl, start, end, ttl_gte){
  #'this function tests to see if there is a level for the trade...
  #'if so it calls function time_and_status
  if(ttl>=ttl_gte){
    time_and_status(tbbl, start, end)
  }else{
    #' otherwise it returns an empty tibble
    tibble()
  }
}

time_and_status <- function(tbbl, start, end){
  tbbl|>
    mutate(status=case_when(is.na(get(end)) & !is.na(get(start))~0,
                            !is.na(get(end))~1,
                            is.na(get(end)) & is.na(get(start))~ NA_real_),
           time=case_when(is.na(get(end)) & !is.na(get(start)) ~ interval(get(start), last_observed),
                          !is.na(get(end)) ~ interval(get(start), get(end)),
                          is.na(get(end)) & is.na(get(start))~ NA),
           time= time %/% months(1)
    )|>
    select(trade_desc, era, status, time)|>
    na.omit()
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

get_time_and_surv <- function(mdl_lst){
  if(mdl_lst$time[1]!=0){
    time=c(0, mdl_lst$time)
    surv=c(1, mdl_lst$surv)
   }else{
     time=mdl_lst$time
     surv=mdl_lst$surv
   }
  tibble(time=time,
         surv=surv)|>
    as_tsibble(index=time)|> #definitely do not want to fill in gaps for forecasting!
    fill_gaps()|>
    fill(surv, .direction = "down")|>
    as_tibble()
}

pivot_wrapper <- function(tbbl){
  tbbl|>
    pivot_wider(names_from = level, values_from=surv)
}

trade_surv <- function(trade, tbbl){
  filtered <- tbbl|>
    filter(trade_desc==trade)
  filtered_fit <- survfit(Surv(time, status) ~ level + era, data = filtered)
  plt <- survminer::ggsurvplot(filtered_fit,
                               filtered,
                               color = "era",
                               fun="event",
                               xlim=c(0,8*12),
                               break.time.by=24,
                               xscale="m_y"
  )
  plt$plot+
    facet_wrap(~factor(level, levels=c('level1',
                                       'level2',
                                       'level3',
                                       'level4',
                                       'completion',
                                       'journeyperson')), scales = "free")+
    theme_gray()+
    theme(legend.position = "bottom")+
    labs(x="Time in years",
         y="Probability of Completion",
         color=NULL)+
    theme(text=element_text(size=15))+
    scale_colour_brewer(palette = "Dark2")
}

level_surv <- function(lev, tbbl){
  filtered <- tbbl|>
    filter(level==lev)
  filtered_fit <- survfit(Surv(time, status) ~ trade_desc + era, data = filtered)
  plt <- survminer::ggsurvplot(filtered_fit,
                               filtered,
                               fun="event",
                               color="era",
                               xlim=c(0,8*12),
                               break.time.by=12,
                               xscale="m_y")+
    labs(x="Time in years",
         y="Probability of Completion",
         colour=NULL)
  plt$plot+facet_wrap(~trade_desc)+
    theme_gray()+
    theme(legend.position = "bottom")+
    theme(text=element_text(size=12))+
    scale_colour_brewer(palette = "Dark2")
}
my_dt <- function(tbbl, round_digits=3) {
  DT::datatable(tbbl,
                filter = 'top',
                extensions = "Buttons",
                rownames = FALSE,
                options = list(
                  columnDefs = list(list(className = "dt-center", targets = "_all")),
                  paging = TRUE,
                  scrollX = TRUE,
                  scrollY = TRUE,
                  searching = TRUE,
                  ordering = TRUE,
                  dom = "Btip",
                  buttons = list(
                    list(extend = "csv", filename = "some_file_name"),
                    list(extend = "excel", filename = "some_file_name")
                  ),
                  pageLength = 35
                ))%>%
                DT::formatRound(purrr::map_lgl(.$x$data, is.numeric), digits = round_digits)

}

get_probs_wrapper <- function(tbbl, ncols){
  if(ncols==6){
    get_probs6(tbbl)
  }else if(ncols==5){
    get_probs5(tbbl)
  }else if(ncols==4){
    get_probs4(tbbl)
  }else if(ncols==3){
    get_probs3(tbbl)
  }else if(ncols==2){
    get_probs2(tbbl)
  }else{
    tibble()
  }
}

get_probs6 <- function(tbbl){
  tbbl|>
    mutate(`Completed`=1-completion,
           `Level 4`=completion-level4,
           `Level 3`=level4-level3,
           `Level 2`=level3-level2,
           `Level 1`=level2-level1,
           `No Levels`=level1,
           `Years since Registration`=time/12
    )
}

get_probs5 <- function(tbbl){
  tbbl|>
    mutate(`Completed`=1-completion,
           `Level 3`=completion-level3,
           `Level 2`=level3-level2,
           `Level 1`=level2-level1,
           `No Levels`=level1,
           `Years since Registration`=time/12
    )
}

get_probs4 <- function(tbbl){
  tbbl|>
    mutate(`Completed`=1-completion,
           `Level 2`=completion-level2,
           `Level 1`=level2-level1,
           `No Levels`=level1,
           `Years since Registration`=time/12
    )
}

get_probs3 <- function(tbbl){
  tbbl|>
    mutate(`Completed`=1-completion,
           `Level 1`=completion-level1,
           `No Levels`=level1,
           `Years since Registration`=time/12
    )
}

get_probs2 <- function(tbbl){
  tbbl|>
    mutate(`Completed`=1-completion,
           `No Levels`=completion,
           `Years since Registration`=time/12
    )
}


make_long <- function(tbbl){
  tbbl|>
    select(matches("^[A-Z]", ignore.case = FALSE))|> #columns where name starts with uppercase
    pivot_longer(cols=-`Years since Registration`,
                 names_to = "level",
                 values_to = "Probability of being in State")|>
    mutate(level=factor(level, levels=c("Completed",
                                        "Level 4",
                                        "Level 3",
                                        "Level 2",
                                        "Level 1",
                                        "No Levels")))
}

make_state_prob_plot <- function(tbbl, era){
  tbbl|>
    ggplot(aes(`Years since Registration`,
               `Probability of being in State` ,
               fill=level))+
    geom_area(alpha=.5)+
    scale_x_continuous(limits = c(0,8), breaks = 0:8)+
    scale_fill_brewer(palette = "Dark2")+
    theme_minimal()+
    labs(fill="State",
         title=paste("State probability plot for", era))+
    theme(text=element_text(size=16))

}

missing_to_reg_date <- function(reg_date,
                                level_date,
                                next_date,
                                completion_date,
                                technical_training_level,
                                ttl_gte){
  if((!is.na(next_date) | !is.na(completion_date)) &      #IF next level OR completion date exists AND
     technical_training_level >= ttl_gte &                           #this trade has this level AND
     is.na(level_date)){                                             #level date is missing
    reg_date                                          #THEN replace level date with the registration date
  }else{
    level_date                                                       #otherwise leave it alone
  }
}

check_sequential <- function(tbbl, start, stop){
  tbbl|>
    transmute(diff={{  stop  }}-{{  start  }}>=0)|>
    filter(diff==FALSE)|>
    nrow()>0
}

get_min_prob <- function(tbbl){
  min(tbbl$`Probability of being in State`)
}



