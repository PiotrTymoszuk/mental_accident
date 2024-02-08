# Sensitivity analysis for the surgery status

  insert_head()
  
# container -------
  
  se_surg <- list()
  
# analysis data for the surgery strata -------
  
  insert_msg('Psychometry for the surgery strata')

  se_surg$data <- minus_level_data(ptsd$dataset, 
                                   split_factor = 'surgery_done', 
                                   extremes = FALSE) %>% 
    set_names(c('surgery', 
                'no_surgery'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_surg$clust_obj$global <- se_globals$clust_obj
  
  ## structures with the strata
  
  se_surg$clust_obj[names(se_surg$data)] <- se_surg$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_surg$clust_obj$surgery <- 
    se_surg$clust_obj$surgery %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_surg$clust_obj$no_surgery <- 
    se_surg$clust_obj$no_surgery %>% 
    rename(c('1' = 'neutral', 
             '2' = 'PTG', 
             '3' = 'PTS'))

# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_surg$stats <- se_surg$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_surg$clust_size <- se_surg$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the surgery participants to the clusters ------
  
  insert_msg('Assignment of the surgery strata to the clusters')
  
  se_surg$assignment <- se_surg$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'surgery_done')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'surgery_done')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()