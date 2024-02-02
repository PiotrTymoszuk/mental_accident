# Sensitivity analyses for the mental cluster structure:
#
# 1) Effect of exclusion due to incomplete mental health battery

# tools -------

  library(tidyverse)
  library(rlang)
  library(trafo)
  library(stringi)

  library(clustTools)
  library(impute)

  library(furrr)
  library(soucer)

  insert_head()
  
  set_rownames <- trafo::set_rownames
  
  source_all('./tools/tools.R', 
             message = TRUE, 
             crash = TRUE)
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  c('./sensitivity scripts/globals.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# Sensitivity analyses --------
  
  insert_msg('Sensitivity analyses')
  
  ## sensitivity analysis for the most obvious confounders
  ## common plots and tables (summary)
  
  c('./sensitivity scripts/missing.R', 
    './sensitivity scripts/injury.R', 
    './sensitivity scripts/year.R', 
    './sensitivity scripts/gender.R', 
    './sensitivity scripts/trauma.R', 
    './sensitivity scripts/prior_accidents.R', 
    './sensitivity scripts/psych_support.R', 
    './sensitivity scripts/income.R', 
    './sensitivity scripts/education.R', 
    './sensitivity scripts/summary.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()