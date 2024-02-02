# Sensitivity analysis for injury severity reflected by stratified AIS 

  insert_head()
  
# container -------
  
  se_ais <- list()
  
# analysis data for the AIS strata -------
  
  insert_msg('Psychometry for the AIS strata')

  se_ais$data <- minus_level_data(ptsd$dataset, 
                                  split_factor = 'injury_sev_strata', 
                                  extremes = FALSE) %>% 
    set_names(c('without_ais1', 
                'without_ais2', 
                'without_ais3up'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_ais$clust_obj$global <- se_globals$clust_obj
  
  ## structures with exclusion of minor and of severe injuries
  
  se_ais$clust_obj[names(se_ais$data)] <- se_ais$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_ais$clust_obj$without_ais1 <- 
    se_ais$clust_obj$without_ais1 %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_ais$clust_obj$without_ais2 <- 
    se_ais$clust_obj$without_ais2 %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_ais$clust_obj$without_ais3up <- 
    se_ais$clust_obj$without_ais3up %>% 
    rename(c('1' = 'neutral', 
             '2' = 'PTG', 
             '3' = 'PTS'))
  
# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_ais$stats <- se_ais$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_ais$clust_size <- se_ais$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the injury strata to the clusters ------
  
  insert_msg('Assignment of the injury strata to the clusters')
  
  se_ais$assignment <- se_ais$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'injury_sev_strata')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()
  
  

  