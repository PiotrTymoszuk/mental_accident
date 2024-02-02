# Comparison of prevalence of mental disorder symptoms with published reports

  insert_head()
  
# Tools ------
  
  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  
  library(bootStat)
  library(exda)
  library(meta)
  
  library(soucer)
  library(furrr)
  
  insert_head()
  
  set_rownames <- trafo::set_rownames
  
  source_all('./tools/tools.R', 
             message = TRUE, 
             crash = TRUE)
  
# globals -------
  
  insert_msg('Analysis globals')
  
  lit_globals <- list()

  lit_globals$mental_symptoms <- 
    c('dsm5_cluster_class', 
      'dsm5_B_class', 
      'dsm5_C_class', 
      'dsm5_D_class', 
      'dsm5_E_class', 
      'dsm5_all_class', 
      'unwilling_flashback', 
      'flashback_frequency', 
      'rs13_total_class', 
      'phq9_total_class', 
      'gad7_total_class', 
      'phqd_panic_total_class', 
      'phq_events_total_class', 
      'traumatic_event')
    
# analysis scripts -------
  
  insert_msg('Analysis scripts')
  
  ## frequency estimates
  
  c('./literature scripts/german_monitoring.R', 
    './literature scripts/own_estimates.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## plots

# END ------
  
  insert_tail()
  
