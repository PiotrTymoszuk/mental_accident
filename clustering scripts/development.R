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
  
# Average silhouette widths -------
  
  insert_msg('Average silhouette widths')
  
  clust_devel$silhouette <- clust_devel$algos %>% 
    map(silhouette) %>% 
    map(summary) %>% 
    map(filter, clust_id == 'global') %>% 
    compress(names_to = 'method') %>% 
    select(method, perc_negative, mean) %>% 
    set_names(c('method', 'perc_negative_sil', 'mean_sil'))
  
# Cross-validation ----
  
  insert_msg('Cross-validation')
  
  plan('multisession')
  
  clust_devel$cv <- clust_devel$algos %>% 
    future_map(cv, 
               nfolds = 10, 
               kNN = 27, 
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
  
  clust_devel$result_tbl <- 
    clust_devel[c("variance", "silhouette", "cv")] %>% 
    reduce(left_join, by = 'method') %>% 
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
  
# Plot ------
  
  insert_msg('Plot')
  
  clust_devel$result_plot <- clust_devel$result_tbl %>% 
    pivot_longer(cols = c(variance, mean_sil, cv_accuracy), 
                 names_to = 'statistic', 
                 values_to = 'value') %>% 
    ggplot(aes(x = value, 
               y = reorder(method_lab, value), 
               fill = reorder(statistic, value))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    scale_fill_manual(values = c(variance = 'steelblue2', 
                                 mean_sil = 'indianred3', 
                                 cv_accuracy = 'darkolivegreen4'), 
                      labels = c(variance = 'Explained variance', 
                                 mean_sil = 'Mean silhouette', 
                                 cv_accuracy = 'CV accuracy'), 
                      name = '') + 
    facet_grid(. ~ statistic, 
               labeller = as_labeller(c(variance = 'Explained variance', 
                                        mean_sil = 'Mean silhouette', 
                                        cv_accuracy = 'CV accuracy')), 
               scales = 'free') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          legend.position = 'none') + 
    labs(title = 'Performance of clustering algorithms', 
         subtitle = paste('Training subset, k = 3 clusters, n =', 
                          nrow(clust_globals$analysis_tbl$training)), 
         x = 'Statistic value')
  
# END -----
  
  insert_tail()