# Sensitivity analysis for the participants with and without a prior accident

  insert_head()
  
# container -------
  
  se_accident <- list()
  
# analysis data for the prior accident strata -------
  
  insert_msg('Psychometry for the prior accident strata')

  se_accident$data <- minus_level_data(ptsd$dataset, 
                                       split_factor = 'prior_accident', 
                                       extremes = FALSE) %>% 
    set_names(c('prior_accident', 
                'without_prior_accident'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_accident$clust_obj$global <- se_globals$clust_obj
  
  ## structures with the strata
  
  se_accident$clust_obj[names(se_accident$data)] <- se_accident$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_accident$clust_obj$prior_accident <- 
    se_accident$clust_obj$prior_accident %>% 
    rename(c('1' = 'neutral', 
             '2' = 'PTG', 
             '3' = 'PTS'))
  
  se_accident$clust_obj$without_prior_accident <- 
    se_accident$clust_obj$without_prior_accident %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))

# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_accident$stats <- se_accident$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_accident$clust_size <- se_accident$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the prior accident strata to the clusters ------
  
  insert_msg('Assignment of the prior accident strata to the clusters')
  
  se_accident$assignment <- se_accident$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'prior_accident')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()