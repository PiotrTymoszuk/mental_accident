# Differences in clustering variables between the mental clusters

  insert_head()
  
# container -------
  
  feat_clust <- list()
  
# Parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  feat_clust$variables <- 
    c(clust_globals$variables, 
      ptsd$mental_variables[!ptsd$mental_variables %in% clust_globals$variables])
  
  ## clustering objects 
  
  feat_clust$clust_obj <- semi_clust$clust_obj
  
  ## analysis tables: identity and normalized
  
  feat_clust$norm_tbl <- feat_clust$clust_obj %>% 
    map(model.frame) %>% 
    map(rownames_to_column, 'ID') %>% 
    map2(., feat_clust$clust_obj, 
         ~left_join(.x, 
                    set_names(.y$clust_assignment, c('ID', 'clust_id')), 
                    by = 'ID')) %>% 
    map(as_tibble)
  
  feat_clust$ident_tbl <- ptsd$dataset %>% 
    blast(partition) %>% 
    map(select, 
        ID, all_of(feat_clust$variables)) %>% 
    map2(., feat_clust$clust_obj, 
         ~left_join(.x, 
                    set_names(.y$clust_assignment, c('ID', 'clust_id')), 
                    by = 'ID')) %>% 
    map(as_tibble)

# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  feat_clust$desc_stats <- feat_clust$ident_tbl %>% 
    map(explore, 
               variables = feat_clust$variables, 
               split_factor = 'clust_id', 
               what = 'table', 
               pub_styled = TRUE)
  
# Testing for differences -----
  
  insert_msg('Testing for differences between the clusters')
  
  feat_clust$test <- feat_clust$ident_tbl %>% 
    future_map(compare_variables, 
               variables = feat_clust$variables, 
               split_factor = 'clust_id', 
               what = 'eff_size', 
               types =  'kruskal_etasq', 
               ci = FALSE, 
               exact = FALSE, 
               pub_styled = TRUE, 
               adj_method = 'BH', 
               .options = furrr_options(seed = TRUE)) %>% 
    map(format_fct_test)

# Significant factors ------
  
  insert_msg('Significant factors')

  feat_clust$top_factors <- feat_clust$test %>% 
    map(filter, p_adjusted < 0.05) %>% 
    map(~.x$variable) %>% 
    reduce(intersect)

# Single plots ------
  
  insert_msg('Plots for single variables')
  
  ## for the clustering factors
  
  feat_clust$plots <- 
    list(data = feat_clust$ident_tbl, 
         test_data = feat_clust$test) %>% 
    pmap(plot_fct, 
         factor = 'clust_id', 
         fill_scale = scale_fill_manual(values = globals$clust_colors, 
                                        name = 'Cluster'))

  
  ## appending the titles with the subset information
  
  for(i in names(feat_clust$test)) {
    
    feat_clust$plots[[i]] <- 
      feat_clust$plots$clust_fct[[i]] %>% 
      map(~.x + 
            labs(title = paste(.x$labels$title, i, sep = ', ')))
    
  }

# Clustering of the clustering factors -------
  
  insert_msg('Clustering of the variables')
  
  ## for visualization
  
  feat_clust$var_clust_obj <- feat_clust$clust_obj$training %>% 
    model.frame %>% 
    t %>% 
    as.data.frame %>% 
    kcluster(distance_method = 'cosine', 
             clust_fun = 'pam', 
             k = 3)
  
  ## appending the test results and plotting order
  
  feat_clust$test <- feat_clust$test %>% 
    map(inner_join, 
        set_names(feat_clust$var_clust_obj$clust_assignment, 
                  c('variable', 'var_clust')), 
        by = 'variable')

# Heat maps of the clustering factors -------
  
  insert_msg('Heat maps')
  
  feat_clust$clust_hm <- 
    list(x_object = feat_clust$clust_obj, 
         plot_title = c('Training', 'Test'), 
         plot_subtitle = feat_clust$norm_tbl %>% 
           map(label_n, clust_id, sep = ': n = ') %>% 
           map(paste, collapse = ', ')) %>% 
    pmap(plot_clust_hm, 
         y_object = feat_clust$var_clust_obj, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'black', 
                                high = 'firebrick', 
                                midpoint = 0, 
                                limits = c(-4, 4), 
                                oob = scales::squish, 
                                name = 'Z-score') + 
           scale_y_discrete(#limits = feat_clust$plot_order$training, 
                            labels = clust_globals$variables %>% 
                              exchange(dict = ptsd$var_lexicon, 
                                       key = 'variable', 
                                       value = 'label') %>% 
                              stri_replace(fixed = ' score', 
                                           replacement = '') %>% 
                              set_names(clust_globals$variables)))
  
# Ribbon plots ------
  
  insert_msg('Ribbon plots')
  
  ## y axis labels, significant effects are highlighted in bold
  
  feat_clust$ribbon_labs <- feat_clust$test %>% 
    map(mutate, 
        label = ifelse(variable %in% names(psych_labeller), 
                       psych_labeller[variable], 
                       exchange(variable, 
                                dict = ptsd$var_lexicon)), 
        label = stri_replace(label, 
                             fixed = ' score', 
                             replacement = ''), 
        label = ifelse(stri_detect(label, fixed = 'sum'), 
                       label, 
                       stri_replace(label, 
                                    regex = '(EUROHIS-QOL\\s{1}8\\s{1})|(PTGI\\s{1})|(PCL-5\\s{1})', 
                                    replacement = '')), 
        label = ifelse(p_adjusted < 0.05, 
                       paste0('<b>', label, '</b>'), 
                       label)) %>% 
    map(select, 
        variable, label)
  
  ## ribbon plots

  feat_clust$ribbon_plots <- 
    list(data = feat_clust$norm_tbl, 
         plot_title = c('Training', 'Test'), 
         plot_subtitle = feat_clust$norm_tbl %>% 
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
    map2(., feat_clust$ribbon_labs, 
         ~.x + 
          geom_vline(xintercept = 0, 
                     linetype = 'dashed') + 
          scale_fill_manual(values = globals$clust_colors, 
                            name = 'Cluster') + 
          scale_color_manual(values = globals$clust_colors, 
                             name = 'Cluster') + 
          scale_y_discrete(labels = function(x) exchange(x, dict = .y)) + 
          theme(axis.title.y = element_blank(), 
                axis.text.y = element_markdown()))
  
# Significance testing results for numeric clustering factors -------
  
  insert_msg('Table for numeric clustering factors')
  
  feat_clust$result_tbl <- 
    map2(map(feat_clust$desc_stats, 
             reduce, left_join, by = 'variable') %>% 
           map(set_names, 
               c('variable', names(feat_clust$desc_stats[[1]]))), 
         map(feat_clust$test, 
             ~.x[c('variable', 'significance', 'eff_size')]), 
         right_join, by = 'variable') %>% 
    map(format_summ_tbl) %>% 
    map(~map_dfc(.x, 
                 stri_replace_all, 
                 regex = '\\nn\\s{1}=\\s{1}\\d+$', 
                 replacement = '')) %>% 
    map(set_names, 
        c('Variable', 'Neutral cluster', 'PTG cluster', 'PTS cluster', 
          'Significance', 
          'Effect size'))
  
  feat_clust$result_tbl <- 
    map2(feat_clust$result_tbl, 
         feat_clust$norm_tbl %>% 
           map(count, clust_id) %>% 
           map(~cbind('Participants, n', 
                      reduce(.x$n, cbind), 
                      NA, NA)) %>% 
           map(as_tibble) %>% 
           map(set_names, names(feat_clust$result_tbl[[1]])), 
         ~rbind(.y, .x))
  
# END ------
  
  plan('sequential')
  
  insert_tail()