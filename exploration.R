# Exploratory data analysis: 
# cohort characteristic, numeric variable distribution, distribution plots
# Export of the formatted data tables

# tools ------

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(stringi)
  library(exda)
  library(soucer)
  library(furrr)
  library(writexl)
  library(rstatix)
  library(trafo)
  library(psych)
  library(clustTools)
  
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
           y_lab = ifelse(format == 'numeric', 
                          exchange(variable, 
                                   dict = ptsd$var_lexicon, 
                                   value = 'axis_lab'), 
                          '% of strata'), 
           plot_title = stri_capitalize(exchange(variable, 
                                                 dict = ptsd$var_lexicon, 
                                                 value = 'label')), 
           plot_subtitle = exchange(variable, 
                                    dict = ptsd$var_lexicon, 
                                    value = 'description'), 
           types = ifelse(type == 'violin', 'wilcoxon_r', 'cramer_v'), 
           types2 = ifelse(type == 'violin', 'kruskal_etasq', 'cramer_v')) %>% 
    select(variable, type, y_lab, plot_title, plot_subtitle, types, types2)
  
  eda_globals$numeric_vars <- eda_globals$variables %>% 
    filter(type == 'violin') %>% 
    .$variable
  
  eda_globals$factor_vars <- eda_globals$variables %>% 
    filter(type != 'violin') %>% 
    .$variable
  
# launching the exploration and export scripts -----
  
  insert_msg('Launching the exploration scripts')
  
  c('./exploration scripts/missingness.R', 
    './exploration scripts/cohort.R', 
    './exploration scripts/distribution.R', 
    './exploration scripts/consistency.R', 
    './exploration scripts/bias.R') %>% 
    source_all(message = TRUE, 
               crash = TRUE)
  
# END -----
  
  insert_tail()