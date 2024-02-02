# Sensitivity analysis for accident year

  insert_head()
  
# container -------
  
  se_year <- list()
  
# analysis data for the accident year strata -------
  
  insert_msg('Psychometry for the accident year strata')

  se_year$data <- minus_level_data(ptsd$dataset, 
                                   split_factor = 'accident_year', 
                                   extremes = FALSE) %>% 
    set_names(c('without_2018', 
                'without_2019', 
                'without_2020'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_year$clust_obj$global <- se_globals$clust_obj
  
  ## structures with exclusion of the year strata
  
  se_year$clust_obj[names(se_year$data)] <- se_year$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_year$clust_obj$without_2018 <- 
    se_year$clust_obj$without_2018 %>% 
    rename(c('1' = 'neutral', 
             '2' = 'PTG', 
             '3' = 'PTS'))
  
  se_year$clust_obj$without_2019 <- 
    se_year$clust_obj$without_2019 %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_year$clust_obj$without_2020 <- 
    se_year$clust_obj$without_2020 %>% 
    rename(c('3' = 'neutral', 
             '2' = 'PTG', 
             '1' = 'PTS'))
  
# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_year$stats <- se_year$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_year$clust_size <- se_year$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the accident year strata to the clusters ------
  
  insert_msg('Assignment of the accident year strata to the clusters')
  
  se_year$assignment <- se_year$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'accident_year')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()