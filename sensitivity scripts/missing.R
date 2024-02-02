# Comparing following: 
# 
# 1) the genuine mental clusters for all analyzed individuals 
#
# 2) the mental clusters trained in a merged data set consisting of the 
# participants included in the analysis and those missing some psychometric
# variables.
#
# Participants excluded from the genuine analysis: for individuals with less than
# 80% of missing values, the missing psychometric variables are imputed by 
# a 9-kNN regressor.

  insert_head()
  
# container ------
  
  se_missing <- list()
  
# Psychometric data for the included and excluded participants -------
  
  insert_msg('Psychometric data for the included excluded participants')

  se_missing$miss_data <- ptsd$cleared %>% 
    filter(ID %in% ptsd$interview_ID) %>% 
    select(ID, all_of(se_globals$variables))
  
  ## participants with more than 80% of missing answers
  ## won't be imputed
  
  se_missing$missing_fractions <- se_missing$miss_data %>% 
    column_to_rownames('ID') %>% 
    as.matrix
  
  se_missing$missing_fractions <- se_missing$missing_fractions %>% 
    is.na %>% 
    rowMeans %>% 
    compress(names_to = 'ID', 
             values_to = 'frac_missing')
  
  se_missing$low_miss_ID <- se_missing$missing_fractions %>% 
    filter(frac_missing <= 0.8) %>%
    .$ID
  
  se_missing$miss_data <- se_missing$miss_data %>% 
    filter(ID %in% se_missing$low_miss_ID) %>% 
    column_to_rownames('ID')
  
# Identifiers of participants absent from the genuine clusters -------
  
  insert_msg('IDs of the imputed participants')
  
  se_missing$impute_ID <- 
    se_missing$low_miss_ID[!se_missing$low_miss_ID %in% ptsd$complete_ID]
  
# Imputation ----------
  
  insert_msg('Imputation')
  
  se_missing$miss_data <- se_missing$miss_data %>% 
    t %>% 
    impute.knn(k = 9)
  
  se_missing$miss_data <- se_missing$miss_data$data %>% 
    t %>% 
    as.data.frame %>% 
    center_data('median')
  
# Clustering objects ------
  
  insert_msg('Clustering objects')
  
  ## for the genuine analyzed data set
  
  se_missing$clust_obj$global <- se_globals$clust_obj
  
  ## and for the merged data set: analyzed + excluded/imputed
  
  se_missing$clust_obj$merged <- 
    kcluster(se_missing$miss_data, 
             distance_method = 'cosine', 
             clust_fun = 'pam', 
             k = 3, 
             seed = 1234) %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
# characteristic of the clustering structures -------
  
  insert_msg('characteristic of the clustering structures')
  
  ## numeric stats: silhouette width, fraction misclassified,
  ## explained variance and neighborhood preservation
  
  se_missing$stats <- se_missing$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'dataset') %>% 
    relocate(dataset)
  
  ## cluster distribution 
  
  se_missing$clust_size <- se_missing$clust_obj %>% 
    map(ngroups) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100)
  
# Cluster assignment of the genuine and imputed individuals -------
  
  insert_msg('Cluster assignment')
  
  se_missing$assingment <- se_missing$clust_obj$merged %>% 
    extract('assignment') %>% 
    set_names(c('ID', 'clust_id')) %>% 
    mutate(subset = ifelse(ID %in% se_missing$impute_ID, 
                           'missing', 'complete'), 
           subset = factor(subset, c('complete', 'missing')))
  
# END -------
  
  insert_tail()