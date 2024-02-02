# Sensitivity analysis for the participants with and without 
# psychological support following the accident.
#
# Participants with a psychological support or with psychological 
# therapy/psychiatric therapy are coded as individuals with the support

  insert_head()
  
# container -------
  
  se_support <- list()
  
# analysis data for the psychological support strata -------
  
  insert_msg('Psychometry for the psychological support strata')
  
  se_support$data <- ptsd$dataset %>% 
    mutate(psych_support_post_accident = ifelse(psych_support_post_accident == 'yes' | 
                                                  psych_therapy_post_accident == 'yes', 
                                                'yes', 'no'), 
           psych_support_post_accident = factor(psych_support_post_accident, 
                                                c('no', 'yes'))) %>% 
    minus_level_data(split_factor = 'psych_support_post_accident', 
                     extremes = FALSE) %>% 
    set_names(c('support', 
                'without_support'))

# Cluster analysis objects -------
  
  insert_msg('Cluster analysis objects')
  
  ## the global solution for all participants included in the analysis
  
  se_support$clust_obj$global <- se_globals$clust_obj
  
  ## structures with the strata
  
  se_support$clust_obj[names(se_support$data)] <- se_support$data %>% 
    map(kcluster, 
        distance_method = 'cosine', 
        clust_fun = 'pam', 
        k = 3, 
        seed = 1234)
  
  ## re-naming after the key cluster characteristics
  ## and for consistency with the global cluster solution
  
  se_support$clust_obj$support <- 
    se_support$clust_obj$support %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
  se_support$clust_obj$without_support <- 
    se_support$clust_obj$without_support %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))

# Characteristic of the clustering structures --------
  
  insert_msg('Characteristic of the clustering structures')
  
  se_support$stats <- se_support$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_support$clust_size <- se_support$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Assignment of the support strata to the clusters ------
  
  insert_msg('Assignment of the support strata to the clusters')
  
  se_support$assignment <- se_support$clust_obj %>% 
    map(extract, 'assignment') %>% 
    map(set_names, c('ID', 'clust_id')) %>% 
    map(left_join, ptsd$dataset[c('ID', 'psych_support_post_accident')], 
        by = 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))
  
# END -------
  
  insert_tail()