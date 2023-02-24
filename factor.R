# Association of age, gender, mental illness, rescue mode, injury severity 
# and psychological support with accident circumstances and mental health
#
# This factors were described to be associated with e.g. PTSD in literature
# Yet, none of them is a strong marker

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
  library(ggtext)
  library(MASS)
  library(lmqc)
  library(doParallel)
  library(caret)
  library(caretExtra)
  
  insert_head()
  
  select <- dplyr::select
  
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
    './factor scripts/mental_illness.R', 
    './factor scripts/prior_trauma.R', 
    './factor scripts/rescue.R',
    './factor scripts/severity.R', 
    './factor scripts/support.R', 
    './factor scripts/modeling.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()