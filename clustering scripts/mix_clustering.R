# Assignment of the test cohort samples to clusters developed with 
# mixture modeling by 7-NN inverse distance weighted classifier
# Characteristic of the clustering structures in the training and test subset

  insert_head()
  
# container -----
  
  semi_mix <- list()
  
# Analysis globals ------
  
  insert_msg('Analysis globals')
  
  semi_mix$analysis_tbl <- clust_globals$analysis_tbl
  
# Training and predictions ------
  
  insert_msg('Training and testing')
  
  semi_mix$clust_obj$training <- clust_em$clust_obj
  
  semi_mix$clust_obj$test <- 
    predict(semi_mix$clust_obj$training, 
            newdata = semi_mix$analysis_tbl$test, 
            type = 'propagation', 
            kNN = 7, 
            resolve_ties = TRUE, 
            simple_vote = FALSE)
  
  semi_mix$clust_obj$test$clust_assignment <- 
    semi_mix$clust_obj$test$clust_assignment %>% 
    mutate(clust_id = factor(clust_id, 
                             levels(semi_mix$clust_obj$training$clust_assignment$clust_id)))
  
# Clustering variances ------
  
  insert_msg('Clustering variances')
  
  semi_mix$variance <- semi_mix$clust_obj %>% 
    map(var) %>% 
    map(~.x$frac_var) %>% 
    compress(names_to = 'partition', 
             values_to = 'variance') %>% 
    map_dfc(unlist)
  
  ## plotting the variance values
  
  semi_mix$variance_plot <- semi_mix$variance %>% 
    ggplot(aes(x = variance, 
               y = reorder(partition, variance), 
               fill = partition)) + 
    geom_bar(stat = 'identity', 
             color = 'gray20') + 
    geom_text(aes(label = signif(variance, 2)), 
              size = 2.5, 
              hjust = 1.4, 
              color = 'white') + 
    scale_fill_manual(values = c(training = 'steelblue4', 
                                 test = 'coral4'), 
                      name = 'Subset') +
    guides(fill = 'none') + 
    scale_x_continuous(limits = c(0, 1)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Semi-supervised clustering', 
         x = 'Fraction explained clustering variance')
  
# Cluster percentages --------
  
  insert_msg('Cluster percentages')
  
  semi_mix$clust_n <- semi_mix$clust_obj %>% 
    map(ngroups) %>% 
    map(arrange, desc(clust_id)) %>% 
    map(mutate, 
        perc = n/sum(n) * 100, 
        y_pos = cumsum(perc) - 0.5 * perc)
  
  ## scale labels for the plots
  
  semi_mix$clust_lab_n <- semi_mix$clust_n %>% 
    map(~map2_chr(.x[[1]], .x[[2]], 
                  paste, sep = ': n = '))
  
  ## plotting the percentages
  
  semi_mix$clust_perc_plot <- semi_mix$clust_n %>% 
    compress(names_to = 'partition') %>% 
    ggplot(aes(x = partition, 
               y = perc, 
               fill = clust_id)) + 
    geom_bar(stat = 'identity', 
             position = 'stack', 
             color = 'black') + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = '') + 
    scale_x_discrete(labels = c(paste('training\nn =', 
                                      nrow(semi_mix$analysis_tbl$training)), 
                                paste('test\nn =', 
                                      nrow(semi_mix$analysis_tbl$test)))) + 
    geom_label(aes(label = signif(perc, 2), 
                   y = y_pos), 
               size = 2.75, 
               color = 'black', 
               show.legend = FALSE) + 
    globals$common_theme + 
    theme(axis.title.x = element_blank()) + 
    labs(title = 'Mental clusters', 
         subtitle = 'Cluster size', 
         y = '% of subset')
  
# Diagnostic plots  ------
  
  insert_msg('Diagnostic plots')
  
  ## WSS and silhouette
  
  semi_mix$diagn_plots <- semi_mix$clust_obj$training %>% 
    plot(cust_theme = globals$common_theme)
  
  ## distance MDS
  
  semi_mix$dist_mds <- semi_mix$clust_obj %>% 
    map(plot, 
        'components', 
        red_fun = 'mds',
        with = 'dist', 
        kdim = 2, 
        cust_theme = globals$common_theme) %>% 
    map2(., semi_mix$clust_obj, 
         ~.x + 
           scale_fill_manual(values = globals$clust_colors, 
                             labels = .y$clust_assignment %>% 
                               label_n(clust_id, sep = ': n = ')))
  
  ## data UMAP
  
  semi_mix$data_umap <- semi_mix$clust_obj %>% 
    map(plot, 
        'components', 
        red_fun = 'umap',
        with = 'data', 
        kdim = 2, 
        random_state = 12345, 
        cust_theme = globals$common_theme) %>% 
    map2(., semi_mix$clust_obj, 
         ~.x + 
           scale_fill_manual(values = globals$clust_colors, 
                             labels = .y$clust_assignment %>% 
                               label_n(clust_id, sep = ': n = '), 
                             name = ''))
  
  ## distance heat maps
  
  semi_mix$dist_hm <- semi_mix$clust_obj %>% 
    map(plot, 
        'heat_map', 
        cust_theme = globals$common_theme) %>% 
    map(~.x + 
          theme(axis.text = element_blank(), 
                axis.text.x = element_blank(), 
                axis.title = element_blank(), 
                axis.ticks = element_blank()))
  
  ## appending the plots with more informative titles
  
  semi_mix[c("dist_mds", 
               "data_umap",
               "dist_hm")] <- semi_mix[c("dist_mds", 
                                           "data_umap",
                                           "dist_hm")] %>%
    map(~map2(.x, c('Training', 'Test'), 
              ~.x + 
                labs(title = .y, 
                     subtitle = .x$labels$subtitle %>% 
                       stri_replace(fixed = 'prediction', 
                                    replacement = '7-NN classifier') %>% 
                       stri_replace(fixed = 'Kmeans clustering', 
                                    replacement = 'EM mixture modeling'))))
  
# END ------
  
  insert_tail()