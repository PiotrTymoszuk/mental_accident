# Differences in clustering variables between the mental clusters

  insert_head()
  
# container -------
  
  feat_mix <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  feat_mix$variables <- 
    c(clust_globals$variables, 
      ptsd$mental_variables[!ptsd$mental_variables %in% clust_globals$variables], 
      'dsm5_cluster_class', 
      'dsm5_B_class', 
      'dsm5_C_class', 
      'dsm5_D_class', 
      'dsm5_E_class', 
      'rs13_total_class', 
      'phq9_total_class', 
      'phq_events_total_class', 
      'gad7_total_class')
  
  ## clustering objects 
  
  feat_mix$clust_obj <- semi_mix$clust_obj
  
  ## analysis tables: identity and normalized
  
  feat_mix$norm_tbl <- feat_mix$clust_obj %>% 
    map(model.frame) %>% 
    map(rownames_to_column, 'ID') %>% 
    map2(., feat_mix$clust_obj, 
         ~left_join(.x, 
                    set_names(.y$clust_assignment, c('ID', 'clust_id')), 
                    by = 'ID')) %>% 
    map(as_tibble) %>% 
    map(~map_dfc(.x, unname))
  
  feat_mix$ident_tbl <- ptsd$dataset %>% 
    dlply('partition', 
          select, 
          ID, 
          all_of(feat_mix$variables)) %>% 
    map2(., feat_mix$clust_obj, 
         ~left_join(.x, 
                    set_names(.y$clust_assignment, c('ID', 'clust_id')), 
                    by = 'ID')) %>% 
    map(as_tibble) %>% 
    map(~map_dfc(.x, unname))  
  
  ## test types
  
  feat_mix$test_types <- feat_mix$ident_tbl$training[feat_mix$variables] %>% 
    map_lgl(is.numeric)
  
  feat_mix$test_types <- ifelse(feat_mix$test_types, 
                                  'kruskal_eta', 
                                  'cramer_v')
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  feat_mix$desc_stats <- feat_mix$ident_tbl %>% 
    map(explore, 
        variables = feat_mix$variables, 
        split_factor = 'clust_id', 
        what = 'table', 
        pub_styled = TRUE)
  
# Testing for differences -----
  
  insert_msg('Testing for differences between the clusters')
  
  feat_mix$test <- feat_mix$ident_tbl %>% 
    map(compare_variables, 
        variables = feat_mix$variables, 
        split_factor = 'clust_id', 
        what = 'eff_size', 
        types =  feat_mix$test_types, 
        ci = FALSE, 
        exact = FALSE, 
        pub_styled = TRUE)
  
  ## separate result data frames for the numeric and categorical variables
  
  feat_mix$test <- feat_mix$test %>% 
    map(~list(clust_fct = filter(.x, variable %in% ptsd$mental_variables), 
              symptoms = filter(.x, !variable %in% clust_globals$variables)) %>% 
          map(format_fct_test)) %>% 
    transpose

# Significant factors ------
  
  insert_msg('Significant factors')

  feat_mix$top_factors <- feat_mix$test %>% 
    map(~map(.x, filter, p_value < 0.05) %>% 
          map(~.x$variable) %>% 
          reduce(intersect))
  
# Single plots ------
  
  insert_msg('Plots for single variables')
  
  ## for the clustering factors
  
  feat_mix$plots$clust_fct <- 
    list(data = feat_mix$ident_tbl, 
         test_data = feat_mix$test$clust_fct) %>% 
    pmap(plot_fct, 
         factor = 'clust_id', 
         fill_scale = scale_fill_manual(values = globals$clust_colors, 
                                        name = 'Cluster'))
  
  ## for mental health problem symptoms
  
  feat_mix$plots$symptoms <- 
    list(data = feat_mix$ident_tbl, 
         test_data = feat_mix$test$symptoms) %>% 
    pmap(plot_fct, 
         factor = 'clust_id', 
         fill_scale = scale_fill_manual(values = c('cornsilk', 
                                                   'coral3', 
                                                   'coral4')))
  
  ## appending the titles with the subset information
  
  feat_mix$plots$clust_fct$training <- 
    feat_mix$plots$clust_fct$training %>% 
    map(~.x + 
          labs(title = paste0(.x$labels$title, ', training')))
  
  feat_mix$plots$clust_fct$test <- 
    feat_mix$plots$clust_fct$test %>% 
    map(~.x + 
          labs(title = paste0(.x$labels$title, ', test')))
  
  feat_mix$plots$symptoms$training <- 
    feat_mix$plots$symptoms$training %>% 
    map(~.x + 
          labs(title = paste0(.x$labels$title, ', training')))
  
  feat_mix$plots$symptoms$test <- 
    feat_mix$plots$symptoms$test %>% 
    map(~.x + 
          labs(title = paste0(.x$labels$title, ', test')))

# Clustering of the clustering factors -------
  
  insert_msg('Clustering of the variables')
  
  ## for visualization
  
  feat_mix$var_clust_obj <- feat_mix$clust_obj$training %>% 
    model.frame %>% 
    t %>% 
    as.data.frame %>% 
    kcluster(distance_method = 'cosine', 
             clust_fun = 'pam', 
             k = 3)
  
  ## appending the test results and plotting order
  
  feat_mix$test$clust_fct <- feat_mix$test$clust_fct %>% 
    map(inner_join, 
        set_names(feat_mix$var_clust_obj$clust_assignment, 
                  c('variable', 'var_clust')), 
        by = 'variable')
  
  feat_mix$plot_order <- feat_mix$test$clust_fct %>% 
    map(arrange, 
        var_clust, 
        eff_size) %>% 
    map(~.$variable)
  
# Heat maps of the clustering factors -------
  
  insert_msg('Heat maps')
  
  feat_mix$clust_hm <- 
    list(x_object = feat_mix$clust_obj, 
         plot_title = c('Training', 'Test'), 
         plot_subtitle = feat_mix$norm_tbl %>% 
           map(label_n, clust_id, sep = ': n = ') %>% 
           map(paste, collapse = ', ')) %>% 
    pmap(plot_clust_hm, 
         y_object = feat_mix$var_clust_obj, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'black', 
                                high = 'firebrick', 
                                midpoint = 0, 
                                limits = c(-4, 4), 
                                oob = scales::squish, 
                                name = 'Z-score') + 
           scale_y_discrete(#limits = feat_mix$plot_order$training, 
                            labels = clust_globals$variables %>% 
                              exchange(dict = ptsd$var_lexicon, 
                                       key = 'variable', 
                                       value = 'label') %>% 
                              stri_replace(fixed = ' score', 
                                           replacement = '') %>% 
                              set_names(clust_globals$variables)))
  
# Ribbon plots ------
  
  insert_msg('Ribbon plots')

  feat_mix$ribbon_plots <- 
    list(data = feat_mix$norm_tbl, 
         plot_title = c('Training', 'Test'), 
         plot_subtitle = feat_mix$norm_tbl %>% 
           map(label_n, clust_id, sep = ': n = ') %>% 
           map(paste, collapse = ', ')) %>% 
    pmap(draw_stat_panel, 
         variables = clust_globals$variables, 
         split_factor = 'clust_id',
         stat = 'mean', 
         err_stat = '2se',
         form = 'line', 
         cust_theme = globals$common_theme, 
         x_lab = 'mean Z-score, 2 \u00D7 SEM') %>% 
    map(~.x + 
          geom_vline(xintercept = 0, 
                     linetype = 'dashed') + 
          scale_fill_manual(values = globals$clust_colors, 
                            name = 'Cluster') + 
          scale_color_manual(values = globals$clust_colors, 
                             name = 'Cluster') + 
          scale_y_discrete(limits = feat_mix$plot_order$training, 
                           labels = clust_globals$variables %>% 
                             exchange(dict = ptsd$var_lexicon, 
                                      key = 'variable', 
                                      value = 'label') %>% 
                             stri_replace(fixed = ' score', 
                                          replacement = '') %>% 
                             set_names(clust_globals$variables)) + 
          theme(axis.title.y = element_blank()))
  
# PTSD cluster positivity --------
  
  insert_msg('PTSD cluster positivity')
  
  feat_mix$ptsd_clust_plots <- 
    list(data = feat_mix$ident_tbl, 
         test_data = feat_mix$test$symptoms, 
         plot_title = paste0('PTSD symptoms, ', 
                             c('training', 'test')), 
         plot_subtitle = feat_mix$norm_tbl %>% 
           map(label_n, clust_id, sep = ': n = ') %>% 
           map(paste, collapse = ', ')) %>% 
    pmap(plot_ptsd_freq, 
         split_factor = 'clust_id', 
         fill_scale = scale_fill_manual(values = globals$clust_colors, 
                                        name = 'Cluster'), 
         color_scale = scale_color_manual(values = globals$clust_colors, 
                                          name = 'Cluster')) %>% 
    map2(., 
         feat_mix$norm_tbl %>% 
           map(label_n, clust_id, sep = ': n = ') %>% 
           map(paste, collapse = ', '), 
         ~.x + 
           labs(plot_subtitle = .y))
  
# Significance testing results for numeric clustering factors -------
  
  insert_msg('Table for numeric clustering factors')
  
  feat_mix$result_tbl <- 
    map2(map(feat_mix$desc_stats, 
             reduce, left_join, by = 'variable') %>% 
           map(set_names, c('variable', names(feat_mix$desc_stats[[1]]))), 
         map(feat_mix$test$clust_fct, 
             ~.x[c('variable', 'significance', 'eff_size')]), 
         right_join, by = 'variable') %>% 
    map(format_summ_tbl) %>% 
    map(~map_dfc(.x, 
                 stri_replace_all, 
                 regex = '\\nn\\s{1}=\\s{1}\\d+$', 
                 replacement = '')) %>% 
    map(set_names, 
        c('Variable', 'Neutral cluster', 'PTG cluster', 'PTB cluster', 
          'Significance', 
          'Effect size'))
  
  feat_mix$result_tbl <- 
    map2(feat_mix$result_tbl, 
         feat_mix$norm_tbl %>% 
           map(count, clust_id) %>% 
           map(~cbind('Participants, n', 
                      reduce(.x$n, cbind), 
                      NA, NA)) %>% 
           map(as_tibble) %>% 
           map(set_names, names(feat_mix$result_tbl[[1]])), 
         ~rbind(.y, .x))
  
# END ------
  
  insert_tail()