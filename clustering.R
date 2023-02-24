# Semi-supervised clustering. The cluster structure is tuned and trained 
# in the training subset of the cohort and subsequently validated in the 
# test subset (cluster assignment by inverse distance weighted kNN)
#
# Levels of psychometric variables are compared between the clusters
# and finally, demographic, socioeconomic, clinical and accident-related 
# measures are compared between the clusters


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
  
  insert_head()
  
  source_all('./tools/tools.R', 
             message = TRUE, 
             crash = TRUE)
  
# analysis globals -------
  
  insert_msg('Clustering globals')
  
  clust_globals <- list()
  
  ## clustering variables
  
  clust_globals$variables <- ptsd$mental_variables
  
  ## normalized, median-centered psychometric data

  clust_globals$analysis_tbl <- ptsd$dataset %>% 
    dlply('partition', as_tibble) %>% 
    map(select, ID, all_of(clust_globals$variable)) %>% 
    map(column_to_rownames, 'ID') %>% 
    map(center_data, type = 'median')
  
# clustering analysis scripts -----
  
  insert_msg('Clustering analysis scripts')
  
  c('./clustering scripts/development.R', 
    './clustering scripts/clustering.R', 
    './clustering scripts/characteristic.R', 
    './clustering scripts/background.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()