# Sensitivity analysis for household income class

  insert_head()
  
# container -------
  
  se_income <- list()
  
# analysis data for the income strata -------
  
  insert_msg('Psychometry for the income strata')

  se_income$data <- minus_level_data(ptsd$dataset, 
                                     split_factor = 'household_income_class', 
                                     extremes = FALSE) %>% 
    set_names(c('without_no_income', 
                'without_30Klo', 
                'without_30_45K', 
                'without_45Kup'))
  
# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_income$clust_obj$global <- se_globals$clust_obj
  
  ## structures with exclusion of minor and of severe injuries
  
  se_income$clust_obj[names(se_income$data)] <- se_income$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_income$clust_obj$without_no_income <- 
    se_income$clust_obj$without_no_income %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_income$clust_obj$without_30Klo <- 
    se_income$clust_obj$without_30Klo %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_income$clust_obj$without_30_45K <- 
    se_income$clust_obj$without_30_45K %>% 
    rename(c('1' = 'neutral', 
             '2' = 'PTG', 
             '3' = 'PTS'))
  
  se_income$clust_obj$without_45Kup <- 
    se_income$clust_obj$without_45Kup %>% 
    rename(c('3' = 'neutral', 
             '2' = 'PTG', 
             '1' = 'PTS'))

# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_income$stats <- se_income$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_income$clust_size <- se_income$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the income strata to the clusters ------
  
  insert_msg('Assignment of the income strata to the clusters')
  
  se_income$assignment <- se_income$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'household_income_class')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()
  
  

  