# Association of age, gender, mental illness, injury severity 
# and psychological support with accident circumstances and mental health

# tools -----

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(stringi)
  library(exda)
  library(soucer)
  library(furrr)
  library(rstatix)
  library(trafo)
  library(clustTools)
  
  insert_head()
  
  source_all('./tools/tools.R', 
             message = TRUE, 
             crash = TRUE)
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  fct_globals <- list()
  
  fct_globals$variables <- ptsd$var_lexicon %>% 
    filter(type %in% c('characteristic', 'response'))
  
# analysis scripts --------
  
  insert_msg('Analysis scripts')
  
  c('./factor scripts/age.R', 
    './factor scripts/gender.R', 
    './factor scripts/mental_illness.R') %>% 
    source_all(message = TRUE, encoding = TRUE)
  
# END ------
  
  insert_tail()