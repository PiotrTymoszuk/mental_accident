# Analysis globals for sensitivity analyses:
# 
# 1) A 'mother' clustering solution with the merged test + training subset 
# participants.
#
# 2) A 'monther' UMAP layout of psychometric variables with the merged 
# test + training subset participants.

  insert_head()
  
# container ------
  
  se_globals <- list()
  
# The global psychometric data set -------
  
  insert_msg('Global psychometric data set')
  
  se_globals$variables <- ptsd$mental_variables
  
  se_globals$data <- ptsd$dataset %>% 
    select(ID, all_of(se_globals$variables)) %>% 
    column_to_rownames('ID') %>% 
    center_data('median')
  
# The global clustering structure -------
  
  insert_msg('The global clustering structure')
  
  se_globals$clust_obj <- 
    kcluster(se_globals$data, 
             distance_method = 'cosine', 
             clust_fun = 'pam', 
             k = 3, 
             seed = 1234) %>% 
    rename(c('2' = 'neutral', 
             '3' = 'PTG', 
             '1' = 'PTS'))
  
# The global UMAP layout -----
  
  insert_msg('The global UMAP layout')
  
  se_globals$umap_obj <- se_globals$clust_obj %>% 
    components(kdim = 2, 
               red_fun = 'umap', 
               with = 'data')
  
# END ---------
  
  insert_tail()