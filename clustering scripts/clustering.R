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
  
  semi_clust$clust_obj$training$clust_assignment <- 
    semi_clust$clust_obj$training$clust_assignment %>% 
    mutate(clust_id = car::recode(as.character(clust_id), 
                                  "'1' = 'PTS'; 
                                  '2' = 'neutral'; 
                                  '3' = 'PTG'"), 
           clust_id = factor(clust_id, c('neutral', 'PTG', 'PTS'))) 
  
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
  
  insert_msg('Clusetr assignment tables')
  
  semi_clust$assignment <- semi_clust$clust_obj %>% 
    map(~.x$clust_assignment) %>% 
    map(set_names, c('ID', 'clust_id'))
  
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
        random_state = 12345, 
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
    map(clustTools::nobs) %>% 
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
  
  semi_clust$intra_distance$data <- semi_clust$clust_obj %>% 
    map(clust_distance, method = 'cosine')
  
  ## distance heat map
 
  semi_clust$intra_distance$plots <- 
    list(x = semi_clust$intra_distance$data, 
         y = c('Training', 'Test')) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = clust_1,
                      y = clust_2, 
                      fill = dist)) + 
           geom_tile(color = 'black') + 
           geom_text(aes(label = signif(dist, 2)), 
                     size = 2.75) + 
           scale_fill_gradient2(limits = c(0.5, 1), 
                                low = 'firebrick', 
                                mid = 'white', 
                                high = 'steelblue', 
                                midpoint = 0.75, 
                                oob = scales::squish, 
                                name = 'cosine distance') + 
           globals$common_theme + 
           theme(axis.title = element_blank()) + 
           labs(title = y, 
                subtitle = 'Cross-distances between the clusters'))
  
# Cross distances between the clusters in the subsets ------
  
  insert_msg('Cross distances between the clusters in the subsets')
  
  ## cross-distances
  
  semi_clust$inter_distance$data <- 
    semi_distance(semi_clust$clust_obj$training, 
                  semi_clust$clust_obj$test, 
                  method = 'cosine')
  
  ## distance heat map
  
  semi_clust$inter_distance$plot <- semi_clust$inter_distance$data %>% 
    ggplot(aes(x = clust_train, 
               y = clust_test, 
               fill = dist)) + 
    geom_tile(color = 'black') + 
    geom_text(aes(label = signif(dist, 2)), 
              size = 2.75) + 
    scale_fill_gradient2(limits = c(0.5, 1), 
                         low = 'firebrick', 
                         mid = 'white', 
                         high = 'steelblue', 
                         midpoint = 0.75, 
                         oob = scales::squish, 
                         name = 'cosine distance') + 
    globals$common_theme + 
    labs(title = 'Cross-distances between the clusters', 
         subtitle = 'Cosine distance, training vs test subset', 
         x = 'training subset', 
         y = 'test subset')
    
  
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