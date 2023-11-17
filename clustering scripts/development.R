# Development of the optimal clustering procedure in the training dataset

  insert_head()
  
# container list ------
  
  clust_devel <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
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

# Silhouettes, fractions of misclassified, variance and neighborhood -----
  
  insert_msg('Quality stats')
  
  ## done for the entire training cohort
  
  clust_devel$train_stats <- clust_devel$algos %>% 
    map(summary) %>% 
    compress(names_to = 'method')

# Cross-validation ----
  
  insert_msg('Cross-validation')
  
  clust_devel$cv_stats <- clust_devel$algos %>% 
    future_map(cv, 
               nfolds = 10, 
               kNN = 27, 
               resolve_ties = TRUE, 
               simple_vote = FALSE, 
               .parallel = FALSE, 
               .options = furrr_options(seed = TRUE, 
                                        packages = c('clustTools', 
                                                     'somKernels'))) 
  
  clust_devel$cv_stats <- clust_devel$cv %>% 
    map(summary) %>% 
    compress(names_to = 'method') %>% 
    select(method, ends_with('mean'))
    
# Common results table and result visualization ------
  
  insert_msg('Common result table and result visualization')
  
  clust_devel$result_tbl <- 
    clust_devel[c("train_stats", "cv_stats")] %>% 
    reduce(left_join, by = 'method') %>% 
    mutate(method_lab = stri_split_fixed(method, 
                                     pattern = '_', 
                                     simplify = TRUE)[, 1] %>% 
             toupper, 
           method_lab = paste(method_lab, 
                              stri_split_fixed(method, 
                                               pattern = '_', 
                                               simplify = TRUE)[, 2], 
                              sep = ', '))
  
# Plot ------
  
  insert_msg('Plot')
  
  clust_devel$result_plot <- clust_devel$result_tbl %>% 
    pivot_longer(cols = c(frac_var, sil_width, accuracy_mean), 
                 names_to = 'statistic', 
                 values_to = 'value') %>% 
    ggplot(aes(x = value, 
               y = reorder(method_lab, value), 
               fill = reorder(statistic, value))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    scale_fill_manual(values = c(frac_var = 'steelblue2', 
                                 sil_width = 'indianred3', 
                                 accuracy_mean = 'darkolivegreen4'), 
                      labels = c(frac_var = 'Explained variance', 
                                 sil_width = 'Mean silhouette', 
                                 accuracy_mean = 'CV accuracy'), 
                      name = '') + 
    facet_grid(. ~ statistic, 
               labeller = as_labeller(c(frac_var = 'Explained variance', 
                                        sil_width = 'Mean silhouette', 
                                        accuracy_mean = 'CV accuracy')), 
               scales = 'free') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          legend.position = 'none') + 
    labs(title = 'Performance of clustering algorithms', 
         subtitle = paste('Training subset, k = 3 clusters, n =', 
                          nrow(clust_globals$analysis_tbl$training)), 
         x = 'Statistic value')
  
# END -----
  
  plan('sequential')
  
  insert_tail()