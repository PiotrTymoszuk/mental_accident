# Sensitivity analysis for the education grade

  insert_head()
  
# container -------
  
  se_education <- list()
  
# analysis data for the income strata -------
  
  insert_msg('Psychometry for the income strata')

  se_education$data <- minus_level_data(ptsd$dataset, 
                                        split_factor = 'education', 
                                        extremes = FALSE) %>% 
    set_names(c('without_primary', 
                'without_secondary', 
                'without_tertiary'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_education$clust_obj$global <- se_globals$clust_obj
  
  ## structures with exclusion of minor and of severe injuries
  
  se_education$clust_obj[names(se_education$data)] <- se_education$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_education$clust_obj$without_primary <- 
    se_education$clust_obj$without_primary %>% 
    rename(c('1' = 'neutral', 
             '2' = 'PTG', 
             '3' = 'PTS'))
  
  se_education$clust_obj$without_secondary <- 
    se_education$clust_obj$without_secondary %>% 
    rename(c('3' = 'neutral', 
             '2' = 'PTG', 
             '1' = 'PTS'))
  
  se_education$clust_obj$without_tertiary <- 
    se_education$clust_obj$without_tertiary %>% 
    rename(c('3' = 'neutral', 
             '2' = 'PTG', 
             '1' = 'PTS'))
  
# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_education$stats <- se_education$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_education$clust_size <- se_education$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the education strata to the clusters ------
  
  insert_msg('Assignment of the education strata to the clusters')
  
  se_education$assignment <- se_education$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'education')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()
  
  

  