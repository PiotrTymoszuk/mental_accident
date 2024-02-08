# Tables for the rebuttal letter

  insert_head()
  
# container -----
  
  rev_tables <- list()
  
# time to the prime traumatic event -------
  
  insert_msg('Time to the most important traumatic event')
  
  rev_tables$time_trauma <- ptsd$dataset %>% 
    count(prime_trauma_event_past) %>% 
    mutate(percent = n/sum(n) * 100,
           percent = signif(percent, 2)) %>% 
    set_names('Time from trauma event', 
              'Patricipants, N', 
              '% of cohort')
  