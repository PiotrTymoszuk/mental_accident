# Sensitivity analysis for the genders

  insert_head()
  
# container -------
  
  se_gender <- list()
  
# analysis data for the genders -------
  
  insert_msg('Psychometry for the genders')

  se_gender$data <- minus_level_data(ptsd$dataset, 
                                     split_factor = 'sex', 
                                     extremes = FALSE) %>% 
    set_names(c('male', 
                'female'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_gender$clust_obj$global <- se_globals$clust_obj
  
  ## structures with the strata
  
  se_gender$clust_obj[names(se_gender$data)] <- se_gender$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_gender$clust_obj$male <- 
    se_gender$clust_obj$male %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_gender$clust_obj$female <- 
    se_gender$clust_obj$female %>% 
    rename(c('1' = 'neutral', 
             '3' = 'PTG', 
             '2' = 'PTS'))

# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_gender$stats <- se_gender$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_gender$clust_size <- se_gender$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the genders to the clusters ------
  
  insert_msg('Assignment of the genders to the clusters')
  
  se_gender$assignment <- se_gender$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'sex')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()