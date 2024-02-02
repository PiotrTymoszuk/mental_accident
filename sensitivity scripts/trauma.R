# Sensitivity analysis for prior traumatic events

  insert_head()
  
# container -------
  
  se_trauma <- list()
  
# analysis data for the trauma strata -------
  
  insert_msg('Psychometry for the trauma strata')

  se_trauma$data <- minus_level_data(ptsd$dataset, 
                                     split_factor = 'traumatic_event', 
                                     extremes = FALSE) %>% 
    set_names(c('with_trauma', 
                'without_trauma'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_trauma$clust_obj$global <- se_globals$clust_obj
  
  ## structures with exclusion of the trauma event strata
  
  se_trauma$clust_obj[names(se_trauma$data)] <- se_trauma$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_trauma$clust_obj$with_trauma <- 
    se_trauma$clust_obj$with_trauma %>% 
    rename(c('2' = 'neutral', 
             '1' = 'PTG', 
             '3' = 'PTS'))
  
  se_trauma$clust_obj$without_trauma <- 
    se_trauma$clust_obj$without_trauma %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))

# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_trauma$stats <- se_trauma$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_trauma$clust_size <- se_trauma$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the traumatic events strata to the clusters ------
  
  insert_msg('Assignment of the trauma strata to the clusters')
  
  se_trauma$assignment <- se_trauma$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'traumatic_event')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()