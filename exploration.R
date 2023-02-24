# Exploratory data analysis: 
# cohort characteristic, numeric variable distribution, distribution plots

# tools ------

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(stringi)
  library(exda)
  library(soucer)
  library(furrr)
  library(rstatix)
  library(trafo)
  library(psych)
  library(clustTools)
  library(ggrepel)
  library(DescTools)

  insert_head()
  
  source_all('./tools/tools.R', 
             message = TRUE, 
             crash = TRUE)
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  eda_globals <- list()
  
  eda_globals$variables <- ptsd$var_lexicon %>% 
    filter(type %in% c('characteristic', 'response')) %>% 
    mutate(type = ifelse(format == 'numeric', 'violin', 'stack'), 
           types = ifelse(type == 'violin', 'wilcoxon_r', 'cramer_v')) %>% 
    select(variable, type, types)
  
  eda_globals$numeric_vars <- eda_globals$variables %>% 
    filter(type == 'violin') %>% 
    .$variable
  
  eda_globals$factor_vars <- eda_globals$variables %>% 
    filter(type != 'violin') %>% 
    .$variable
  
# launching the exploration scripts -----
  
  insert_msg('Launching the exploration scripts')
  
  c('./exploration scripts/missingness.R', 
    './exploration scripts/cohort.R', 
    './exploration scripts/distribution.R', 
    './exploration scripts/consistency.R', 
    './exploration scripts/bias.R', 
    './exploration scripts/partition.R', 
    './exploration scripts/pca.R') %>% 
    source_all(message = TRUE, 
               crash = TRUE)
  
  ## cached result of the power estimation analysis
  
  if(file.exists('./cache/power.RData')) {
    
    insert_msg('Loading cached power estimation results')
    
    load('./cache/power.RData')
    
  } else {
    
    source_all('./exploration scripts/power.R', 
               message = TRUE, crash = TRUE)
    
  }
  
# END -----
  
  insert_tail()