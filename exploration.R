# Exploratory data analysis: 
# cohort characteristic, numeric variable distribution, distribution plots, 
# principle component analysis and assessment of spontaneous clustering tendency

# tools ------

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(stringi)
  library(readODS)
  library(trafo)

  library(exda)
  library(rstatix)
  library(DescTools)
  
  library(psych)
  library(clustTools)

  library(ggrepel)
  

  library(ggvenn)
  library(ComplexUpset)
  

  library(soucer)
  library(furrr)

  insert_head()
  
  explore <- exda::explore
  var <- clustTools::var
  
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
    './exploration scripts/mental_incomplete.R', 
    './exploration scripts/cohort.R', 
    './exploration scripts/mental_comorbidity.R', 
    './exploration scripts/distribution.R', 
    './exploration scripts/consistency.R', 
    './exploration scripts/bias.R', 
    './exploration scripts/partition.R', 
    './exploration scripts/pca.R', 
    './exploration scripts/time_psycho.R') %>% 
    source_all(message = TRUE, 
               crash = TRUE)
  
  ## cached result of the power estimation analysis
  
  access_cache(cache_path = './cache/power.RData', 
               script_path = './exploration scripts/power.R', 
               message = 'Loading cached power estimation results')

  ## observation time dependency of psychometric variables
  
  c('./exploration scripts/time_psycho.R', 
    './exploration scripts/time_symptoms.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## cached results of the comparison with the Austrian general population
  
  access_cache(cache_path = './cache/aut_stats.RData', 
               script_path = './exploration scripts/sociodemographic.R', 
               message = 'Loading cached comparison with the Austrian population.')
  
# END -----
  
  insert_tail()