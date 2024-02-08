# Comparison of:
# 1) prevalence of mental disorder symptoms with published reports
# 2) distribution of accident sport types in the study cohort and in Austria
# (10-year average).

  insert_head()
  
# Tools ------
  
  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  library(readODS)
  
  library(bootStat)
  library(exda)
  library(meta)
  
  library(ggtext)
  
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
      'phq2_total_class', 
      'gad2_total_class', 
      'phq9_total_class', 
      'phq8_total_class', 
      'phqd_panic_total_class', 
      'phq_events_total_class', 
      'traumatic_event')
    
# analysis scripts -------
  
  insert_msg('Analysis scripts')
  
  c('./literature scripts/german_monitoring.R', 
    './literature scripts/own_estimates.R', 
    './literature scripts/austrian_microcensus.R', 
    './literature scripts/plots.R', 
    './literature scripts/kurasi.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()
  
