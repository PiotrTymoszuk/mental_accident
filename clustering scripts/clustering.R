# Semi-supervised clustering.
#
# The most stable clustering structure (PAM/cosine) is trained in the training
# subset and the cluster assignment predicted for the test subset 
# (7-NN, inverse distance weighted). 
# Goodness of prediction: clustering variance
#
# Diagnostic plots for the training subset and variable importance measures

  insert_head()
  
# container ------
  
  semi_clust <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  semi_clust$analysis_tbl <- clust_globals$analysis_tbl
  
# Training and predictions -----
  
  insert_msg('Devlopment of the clustering structures')
  
  ## training, renaming the clusters
  
  semi_clust$clust_obj$training <- 
    kcluster(semi_clust$analysis_tbl$training, 
             distance_method = 'cosine', 
             clust_fun = 'pam', 
             k = 3, 
             seed = 1234)
  
  semi_clust$clust_obj$training$clust_assignment <- 
    semi_clust$clust_obj$training$clust_assignment %>% 
    mutate(clust_id = car::recode(as.character(clust_id), 
                                  "'1' = 'PTSD'; 
                                  '2' = 'neutral'; 
                                  '3' = 'PTG'"), 
           clust_id = factor(clust_id, c('neutral', 'PTG', 'PTSD'))) 
  
  ## test
  
  semi_clust$clust_obj$test <- 
    predict(semi_clust$clust_obj$training, 
            newdata = semi_clust$analysis_tbl$test, 
            type = 'propagation', 
            kNN = 7, 
            resolve_ties = TRUE, 
            simple_vote = FALSE)
  
# Clustering variances ------
  
  insert_msg('Clustering variances')
  
  semi_clust$variance <- semi_clust$clust_obj %>% 
    map(var) %>% 
    map(~.x$frac_var) %>% 
    compress(names_to = 'partition', 
             values_to = 'variance') %>% 
    map_dfc(unlist)
  
  ## plotting the variance values
  
  semi_clust$variance_plot <- semi_clust$variance %>% 
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
  
# Diagnostic plots  ------
  
  insert_msg('Diagnostic plots')
  
  ## WSS and silhouette
  
  semi_clust$diagn_plots <- semi_clust$clust_obj$training %>% 
    plot(cust_theme = globals$common_theme)
  
  ## distance MDS
  
  semi_clust$dist_mds <- semi_clust$clust_obj %>% 
    map(plot, 
        'components', 
        red_fun = 'mds',
        with = 'dist', 
        kdim = 2, 
        cust_theme = globals$common_theme) %>% 
    map2(., semi_clust$clust_obj, 
         ~.x + 
           scale_fill_manual(values = globals$clust_colors, 
                             labels = .y$clust_assignment %>% 
                               label_n(clust_id, sep = ': n = ')))
  
  ## data UMAP

  semi_clust$data_umap <- semi_clust$clust_obj %>% 
    map(plot, 
        'components', 
        red_fun = 'umap',
        with = 'data', 
        kdim = 2, 
        random_state = 1234, 
        cust_theme = globals$common_theme) %>% 
    map2(., semi_clust$clust_obj, 
         ~.x + 
           scale_fill_manual(values = globals$clust_colors, 
                             labels = .y$clust_assignment %>% 
                               label_n(clust_id, sep = ': n = '), 
                             name = ''))
  
  ## distance heat maps
  
  semi_clust$dist_hm <- semi_clust$clust_obj %>% 
    map(plot, 
        'heat_map', 
        cust_theme = globals$common_theme) %>% 
    map(~.x + 
          theme(axis.text = element_blank(), 
                axis.text.x = element_blank(), 
                axis.title = element_blank(), 
                axis.ticks = element_blank()))
  
  ## appending the plots with more informative titles
  
  semi_clust[c("dist_mds", 
               "data_umap",
               "dist_hm")] <- semi_clust[c("dist_mds", 
                                           "data_umap",
                                           "dist_hm")] %>%
    map(~map2(.x, c('Training', 'Test'), 
              ~.x + 
                labs(title = .y, 
                     subtitle = .x$labels$subtitle %>% 
                       stri_replace(fixed = 'prediction', 
                                    replacement = '7-NN classifier'))))
  
# Importance of the clustering variables -------
  
  insert_msg('Importance')
  
  set.seed(1234)
  
  plan('multisession')
  
  semi_clust$importance <- 1:50 %>% 
    future_map(~impact(semi_clust$clust_obj$training, 
                       .parallel = FALSE), 
               .options = furrr_options(seed = TRUE)) %>% 
    set_names(paste0('shuffle_', 1:50))
  
  plan('sequential')
  
  ## formatting the results
  
  semi_clust$importance <- semi_clust$importance %>% 
    compress(names_to = 'shuffle')
  
  ## plotting the importance
  
  semi_clust$importance_plot <- semi_clust$importance %>% 
    filter(variable != 'data') %>% 
    ggplot(aes(x = frac_diff, 
               y = reorder(variable, frac_diff))) + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_boxplot(fill = 'steelblue', 
                 alpha = 0.25, 
                 outlier.color = NA) + 
    geom_point(shape = 16, 
               size = 1, 
               position = position_jitter(width = 0, 
                                          height = 0.1), 
               color = 'gray40') + 
    scale_y_discrete(labels = clust_globals$variables %>% 
                       exchange(dict = ptsd$var_lexicon, 
                                key = 'variable', 
                                value = 'label') %>% 
                       stri_replace(fixed = ' score', 
                                    replacement = '') %>% 
                       set_names(clust_globals$variables)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Variable importance, training subset', 
         x = '\u0394 explained variance', 
         subtitle = paste('observations: n =', 
                          nrow(model.frame(semi_clust$clust_obj$training))))

# END -------
  
  insert_tail()