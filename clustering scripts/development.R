# Development of the optimal clustering procedure in the training dataset

  insert_head()
  
# container list ------
  
  clust_devel <- list()
  
# globals -----
  
  insert_msg('Analysis globals')
  
  ## analysis table
  
  clust_devel$analysis_tbl <- clust_globals$analysis_tbl$training

  ## distances
  
  clust_devel$distances <- 
    c('euclidean', 'manhattan', 'cosine')

# Clustering algorithms ------
  
  insert_msg('Generating clustering objects')
  
  ## HCL, Ward D2

  clust_devel$algos[paste0('hcl_', clust_devel$distances)] <- 
    clust_devel$distances %>% 
    map(hcluster, 
        data = clust_devel$analysis_tbl, 
        k = 3,  
        hc_method = 'ward.D2')
  
  ## K-means
  
  clust_devel$algos[paste0('kmeans_', clust_devel$distances)] <-  
    clust_devel$distances %>% 
    map(kcluster, 
        data = clust_devel$analysis_tbl, 
        k = 3, 
        clust_fun = 'kmeans') 
  
  ## PAM
  
  clust_devel$algos[paste0('pam_', clust_devel$distances)] <-  
    clust_devel$distances %>% 
    map(kcluster, 
        data = clust_devel$analysis_tbl, 
        k = 3, 
        clust_fun = 'pam') 

# Clustering variances ------
  
  insert_msg('Clustering variances')
  
  clust_devel$variance <- clust_devel$algos %>% 
    map(var) %>% 
    map(~.x$frac_var) %>% 
    compress(names_to = 'method', 
             values_to = 'variance') %>% 
    mutate(variance = unlist(variance))
  
# Cross-validation ----
  
  insert_msg('Cross-validation')
  
  plan('multisession')
  
  clust_devel$cv <- clust_devel$algos %>% 
    future_map(cv, 
               nfolds = 10, 
               kNN = 7, 
               resolve_ties = TRUE, 
               simple_vote = FALSE, 
               .parallel = FALSE, 
               .options = furrr_options(seed = TRUE, 
                                        packages = c('clustTools', 
                                                     'somKernels'))) 
  
  plan('sequential')
  
  clust_devel$cv <- clust_devel$cv %>% 
    map(~.x$summary) %>% 
    compress(names_to = 'method')
    
# Common results table and result visualization ------
  
  insert_msg('Common result table and result visualization')
  
  clust_devel$result_tbl <- left_join(clust_devel$variance, 
                                      clust_devel$cv, 
                                      by = 'method') %>% 
    mutate(method_lab = stri_split_fixed(method, 
                                     pattern = '_', 
                                     simplify = TRUE)[, 1] %>% 
             toupper, 
           method_lab = paste(method_lab, 
                              stri_split_fixed(method, 
                                               pattern = '_', 
                                               simplify = TRUE)[, 2], 
                              sep = ', '), 
           cv_accuracy = 1 - mean_error)
  
  ## plot  
  
  clust_devel$result_plot <- clust_devel$result_tbl %>% 
    pivot_longer(cols = c(variance, cv_accuracy), 
                 names_to = 'statistic', 
                 values_to = 'value') %>% 
    ggplot(aes(x = value, 
               y = reorder(method_lab, value), 
               fill = statistic)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    scale_fill_manual(values = c(variance = 'steelblue2', 
                                 cv_accuracy = 'darkolivegreen4'), 
                      labels = c(variance = 'Expl. variance', 
                                 cv_accuracy = 'CV accuracy'), 
                      name = '') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Performance of clustering algorithms', 
         subtitle = paste('Training subset, k = 3 clusters, n =', 
                          nrow(clust_globals$analysis_tbl$training)), 
         x = 'Statistic value')
  
# END -----
  
  insert_tail()