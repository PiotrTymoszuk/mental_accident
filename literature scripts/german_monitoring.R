# Data on anxiety (GAD-2) and depressive symptoms (PHQ-2) from 
# the German monitoring system (RKI) for the time range of the survey answers.
# Weighted mean of monthly estimates with mean 95% confidence intervals is 
# computed

  insert_head()
  
# container ------
  
  lit_desurv <- list()
  
# data --------
  
  insert_msg('Survey dates and surveillence data')
  
  ## survey dates 
  
  lit_desurv$time_range <- ptsd$dataset$survey_date %>% 
    as.Date %>% 
    range
  
  ## monitoring data
  
  lit_desurv$data <- 
    read_tsv('./data/publications/high_frequency_mental_health_surveillance.tsv') %>% 
    filter(date >= lit_desurv$time_range[1], 
           date <= lit_desurv$time_range[2]) %>% 
    filter(category == 'gesamt', 
           period_duration != '1_Woche', 
           instrument %in% c('GAD-2', 'PHQ-2'), 
           type == 'kategorial') %>% 
    mutate(instrument = factor(instrument, c('GAD-2', 'PHQ-2'))) %>% 
    select(instrument, date, fit, se, lwr, upr) %>% 
    blast(instrument)
  
# Calculation of weighted means -------
  
  insert_msg('Weighted means')
  
  lit_desurv$stats <- 
    list(TE = map(lit_desurv$data, ~.x$fit), 
         seTE = map(lit_desurv$data, ~.x$se)) %>% 
    pmap(metagen) %>% 
    map(~tibble(percent = .x$TE.fixed, 
                lower_ci = .x$lower.fixed, 
                upper_ci = .x$upper.fixed)) %>% 
    compress(names_to = 'variable') %>% 
    relocate(variable)
  
# END -------
  
  insert_tail()