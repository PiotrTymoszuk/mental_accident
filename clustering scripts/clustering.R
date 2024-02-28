# Semi-supervised clustering.
#
# The most stable clustering structure (PAM/cosine) is trained in the training
# subset and the cluster assignment predicted for the test subset 
# (27-NN, inverse distance weighted). 
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
  
  semi_clust$clust_obj$training <- 
    semi_clust$clust_obj$training %>% 
    rename(nm = c('2' = 'neutral', 
                  '3' = 'PTG', 
                  '1' = 'PTS'))
  
  ## test
  
  semi_clust$clust_obj$test <- 
    predict(semi_clust$clust_obj$training, 
            newdata = semi_clust$analysis_tbl$test, 
            type = 'propagation', 
            kNN = 27, 
            resolve_ties = TRUE, 
            simple_vote = FALSE)
  
  semi_clust$clust_obj$test$clust_assignment <- 
    semi_clust$clust_obj$test$clust_assignment %>% 
    mutate(clust_id = factor(clust_id, 
                             levels(semi_clust$clust_obj$training$clust_assignment$clust_id)))
  
# Cluster assignment ------
  
  insert_msg('Cluster assignment tables')
  
  semi_clust$assignment <- semi_clust$clust_obj %>% 
    map(~.x$clust_assignment) %>% 
    map(set_names, c('ID', 'clust_id'))
  
# Clustering variances and silhouettes ------
  
  insert_msg('Clustering variances and silhouettes')
  
  ## explained clustering variance
  ## average silhouette and percentage of observations with negative 
  ## metric values indicative of possible miss-classification
  
  semi_clust$stats <- semi_clust$clust_obj %>% 
    map(summary) %>% 
    compress(names_to = 'partition')

# Plots of variances and average silhouettes -------
  
  insert_msg('Plots of variances, silhouettes and neighborhood preservation')
  
  semi_clust$stat_plots <- 
    list(x = c('frac_var', 'sil_width', 
               'frac_misclassified', 'frac_np'), 
         y = c('Explained variance', 'Cluster separation', 
               'Misclassification', 'Neighborhood preservation'), 
         z = c('between-cluster/total SS', 
               'mean silhouette width', 
               'fraction with negative silhouette width', 
               'fraction of preserved 5-NN')) %>% 
    pmap(function(x, y, z) semi_clust$stats %>% 
           ggplot(aes(x = .data[[x]], 
                      y = ' ', 
                      fill = partition)) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    position = position_dodge(0.9)) + 
           geom_text(aes(label = signif(.data[[x]], 2)), 
                     size = 2.5, 
                     hjust = 1.4, 
                     color = 'white', 
                     position = position_dodge(0.9)) + 
           scale_fill_manual(values = c(training = 'steelblue4', 
                                        test = 'coral4'), 
                             name = 'Subset') +
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = z)) %>% 
    set_names(c('frac_var', 'sil_width', 
                'frac_misclassified', 'frac_np'))

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
  ## defining a common UMAP layout in the training subset
  
  semi_clust$train_umap <- semi_clust$clust_obj$training %>% 
    components(red_fun = 'umap', 
               with = 'data',
               kdim = 2, 
               random_state = 12345)

  semi_clust$data_umap$training <- semi_clust$train_umap %>% 
    plot(cust_theme = globals$common_theme) + 
    labs(subtitle = 'PAM, cosine distance')
    
  semi_clust$data_umap$test <- semi_clust$clust_obj$test %>% 
    plot(type = 'components', 
         red_fun = 'umap',
         with = 'data', 
         kdim = 2, 
         train_object = semi_clust$train_umap, 
         cust_theme = globals$common_theme)
    
  semi_clust$data_umap <- semi_clust$data_umap %>% 
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
  
  ## silhouette widths for single observations
  
  semi_clust$silhouette_plots <- semi_clust$clust_obj %>% 
    map(silhouette) %>% 
    map(plot, 
        fill_by = 'neighbor', 
        cust_theme = globals$common_theme) %>% 
    map(~.x + 
          theme(panel.grid.major.y = element_blank()) + 
          scale_fill_manual(values = globals$clust_colors, 
                            name = 'Nearest neighbor cluster'))
  
  ## appending the plots with more informative titles
  
  semi_clust[c("dist_mds", 
               "data_umap",
               "dist_hm", 
               "silhouette_plots")] <- semi_clust[c("dist_mds", 
                                                    "data_umap",
                                                    "dist_hm", 
                                                    "silhouette_plots")] %>%
    map(~map2(.x, c('Training', 'Test'), 
              ~.x + 
                labs(title = .y, 
                     subtitle = .x$labels$subtitle %>% 
                       stri_replace(fixed = 'prediction', 
                                    replacement = '27-NN classifier'))))
  
# Cluster distribution ------
  
  insert_msg('Cluster distribution')
  
  ## percentages of participants in the clusters
  
  semi_clust$n_numbers$data <- semi_clust$clust_obj %>% 
    map(ngroups) %>% 
    map(arrange, desc(clust_id)) %>% 
    map(mutate, 
        percent = n/sum(n) * 100, 
        n_complete = sum(n), 
        lab_pos = cumsum(percent) - 0.5 * percent) %>% 
    compress(names_to = 'partition')
  
  semi_clust$n_numbers$complete_lab <- semi_clust$clust_obj %>% 
    map(nobs) %>% 
    map_dbl(~.x$observations)
  
  semi_clust$n_numbers$complete_lab  <- 
    map2_chr(names(semi_clust$n_numbers$complete_lab), 
             semi_clust$n_numbers$complete_lab, 
             paste, sep = '\nn = ') %>% 
    set_names(names(semi_clust$n_numbers$complete_lab))
   
  ## stack plots
  
  semi_clust$n_numbers$plot <- semi_clust$n_numbers$data %>% 
    ggplot(aes(x = percent, 
               y = partition, 
               fill = clust_id)) + 
    geom_bar(stat = 'identity', 
             color = 'black') + 
    geom_label(aes(label = signif(percent, 2), 
                   x = lab_pos), 
               size = 2.75, 
               show.legend = FALSE) + 
    scale_y_discrete(labels = semi_clust$n_numbers$complete_lab) + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = '') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Cluster distribution', 
         x = '% of subset')
  
# Cosine distances between the clusters ------
  
  insert_msg('Cosine distances between the clusters')
  
  ## mean cosine distance between the clusters within each of
  ## the training and test subsets
  
  semi_clust$intra_distance$stats <- semi_clust$clust_obj %>% 
    map(cross_distance) %>% 
    map(summary)

  ## distance heat maps
 
  semi_clust$intra_distance$plots <- semi_clust$clust_obj %>% 
    map(cross_distance) %>% 
    map(plot, 
        type = 'mean', 
        cust_theme = globals$common_theme) %>% 
    map2(., c('Training', 'Test'), 
         ~.x + 
           labs(title = .y, 
                x = paste0('Mentral cluster, ', 
                           tolower(.y), 
                           ' subset'), 
                y = paste0('Mental cluster, ', 
                           tolower(.y), 
                           ' subset')))

# Cross distances between the clusters in the subsets ------
  
  insert_msg('Cross distances between the clusters in the subsets')
  
  ## cross-distances
  
  semi_clust$inter_distance$stats <- 
    cross_distance(semi_clust$clust_obj$training, 
                   semi_clust$clust_obj$test, 
                   method = 'euclidean') %>% 
    summary

  ## distance heat map
  
  semi_clust$inter_distance$plot <- 
    cross_distance(semi_clust$clust_obj$training, 
                   semi_clust$clust_obj$test) %>% 
    plot(type = 'mean', 
         cust_theme = globals$common_theme) + 
    labs(title = 'Cross-distances between the clusters', 
         subtitle = 'Cosine distance, training vs test subset', 
         x = 'training subset', 
         y = 'test subset')
  
# Importance of the clustering variables -------
  
  insert_msg('Importance')
  
  ## permutation importance stats
  
  semi_clust$importance <- 
    impact(semi_clust$clust_obj$training, 
           n_iter = 50, 
           seed = 1234, 
           .parallel = TRUE)

  ## plotting the importance
  
  semi_clust$importance_plot <- semi_clust$importance %>% 
    plot(cust_theme = globals$common_theme, 
         fill_color = 'steelblue', 
         point_size = 1) + 
    geom_vline(xintercept = 0,
               linetype = 'dashed') + 
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