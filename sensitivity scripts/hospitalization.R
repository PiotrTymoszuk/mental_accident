# Sensitivity analysis for the hospitalization status

  insert_head()
  
# container -------
  
  se_hosp <- list()
  
# analysis data for the hospitalized and non-hospitalized participants -------
  
  insert_msg('Psychometry for the hospitalization strata')

  se_hosp$data <- minus_level_data(ptsd$dataset, 
                                   split_factor = 'hospitalization', 
                                   extremes = FALSE) %>% 
    set_names(c('hospitalized', 
                'non_hospitalized'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_hosp$clust_obj$global <- se_globals$clust_obj
  
  ## structures with the strata
  
  se_hosp$clust_obj[names(se_hosp$data)] <- se_hosp$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_hosp$clust_obj$hospitalized <- 
    se_hosp$clust_obj$hospitalized %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_hosp$clust_obj$non_hospitalized <- 
    se_hosp$clust_obj$non_hospitalized %>% 
    rename(c('3' = 'neutral', 
             '1' = 'PTG', 
             '2' = 'PTS'))

# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_hosp$stats <- se_hosp$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_hosp$clust_size <- se_hosp$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the hospitalized strata to the clusters ------
  
  insert_msg('Assignment of the hospitalization strata to the clusters')
  
  se_hosp$assignment <- se_hosp$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'hospitalization')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'hospitalization')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()