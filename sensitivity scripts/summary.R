# Plots and result tables for the sensitivity analysis 

  insert_head()
  
# container ------
  
  se_summary <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## expression for calling objects of interest
  
  se_summary$object_expr <- 
    expr(list(missing = se_missing, 
              sex = se_gender, 
              education = se_education, 
              household_income_class = se_income, 
              traumatic_event = se_trauma, 
              prior_accident = se_accident, 
              accident_year = se_year, 
              injury_sev_strata = se_ais, 
              psych_support_post_accident = se_support))
  
  ## lexicon of the splitting factors
  
  se_summary$split_lexicon <- ptsd$var_lexicon %>% 
    filter(variable %in% names(eval(se_summary$object_expr))) %>% 
    select(variable, label, axis_lab) %>% 
    rbind(tibble(variable = c('global', 'missing'), 
                 label = c('entire analysis cohort', 'incomplete observations'), 
                 axis_lab = c(NA, NA))) %>% 
    mutate(facet_label = stri_replace_all(label, 
                                          fixed = ' ', 
                                          replacement = '\n'))
  
  ## lexicon for the data sets
  
  se_summary$dataset_lexicon <- 
    c(global = 'analysis cohort', 
      merged = 'with missing observations', 
      without_ais1 = 'without AIS1', 
      without_ais2 = 'without AIS2', 
      without_ais3up = 'without AIS3+', 
      without_2018 = 'without 2018 accidents', 
      without_2019 = 'without 2019 accidents', 
      without_2020 = 'without 2020 accidents', 
      male = 'male only', 
      female = 'female only', 
      with_trauma = 'only prior traumatic events', 
      without_trauma = 'without prior traumatic events', 
      prior_accident = 'only prior accidents', 
      without_prior_accident = 'without prior accident', 
      support = 'only psych. support', 
      without_support = 'without psych. support', 
      without_primary = 'without primary', 
      without_secondary = 'without secondary', 
      without_tertiary = 'without tertiary', 
      without_no_income = 'without no income', 
      without_30Klo = 'without < 30K', 
      without_30_45K = 'without 30K - 45K', 
      without_45Kup = 'without > 45K') %>% 
    compress(names_to = 'variable', 
             values_to = 'label')
  
  ## a dummy clustering object grouping the mental health variables
  
  se_summary$var_clust_obj <- feat_clust$var_clust_obj
  
  ## variable lexicon
  
  se_summary$var_lexicon <- ptsd$var_lexicon %>% 
    select(variable, label) %>% 
    mutate(label = stri_replace(label, regex = '\\s{1}score$', replacement = ''))
  
# Data frames with cluster performance stats --------
  
  insert_msg('Common stats')
  
  se_summary$stats <- se_summary$object_expr %>% 
    eval %>% 
    map(~.x$stats) %>% 
    compress(names_to = 'split_factor') %>% 
    filter(!duplicated(dataset)) %>% 
    mutate(split_factor = ifelse(dataset == 'global', 'global', split_factor))
  
  ## deltas of the clustering variance, silhouette width
  ## and neighborhood preservation, expressing as percentages
  
  se_summary$stat_deltas <- se_summary$stats %>% 
    select(dataset, split_factor, sil_width, frac_var, frac_np) %>% 
    mutate(sil_width = sil_width - se_summary$stats$sil_width[1], 
           frac_var = frac_var - se_summary$stats$frac_var[1], 
           frac_np = frac_np - se_summary$stats$frac_np[1]) %>% 
    mutate(sil_width = 100 * sil_width/se_summary$stats$sil_width[1], 
           frac_var = 100 * frac_var/se_summary$stats$frac_var[1],
           frac_np = 100 * frac_np/se_summary$stats$frac_np[1])
  
# Numbers of observations in the data sets -------
  
  insert_msg('N numbers')
  
  se_summary$n_numbers <- se_summary$object_expr %>% 
    eval %>% 
    map(~.x$data) %>% 
    map(map_dbl, nrow) %>% 
    compact
  
  se_summary$n_numbers$missing <- 
    c('merged' = nrow(se_missing$miss_data))
  
  se_summary$n_numbers$global <- 
    c('global' = nrow(se_globals$data))
  
  se_summary$n_numbers <- se_summary$n_numbers %>% 
    map(compress,
        names_to = 'dataset', 
        values_to = 'n_total') %>% 
    compress(names_to = 'split_factor') %>% 
    mutate(axis_label = exchange(dataset, 
                                 se_summary$dataset_lexicon), 
           axis_label = paste(axis_label, n_total, 
                              sep = '\nn = '))
    
  
# Data frames with the cluster sizes -------
  
  insert_msg('Data frames with the cluster sizes')
  
  se_summary$clust_size <- se_summary$object_expr %>% 
    eval %>% 
    map(~.x$clust_size) %>% 
    map(~.x[names(.x) != 'global']) %>% 
    map(compress, names_to = 'dataset') %>% 
    compress(names_to = 'split_factor')
  
  se_summary$clust_size <- se_missing$clust_size$global %>% 
    mutate(split_factor = 'global', 
           dataset = 'global') %>% 
    rbind(se_summary$clust_size)
  
# Plots of deltas of performance stats -------
  
  insert_msg('Plots of deltas of performance stats')
  
  se_summary$delta_plots <- 
    list(x = c('sil_width', 'frac_var', 'frac_np'), 
         y = c('Cluster separation', 
               'Explained clustering variance', 
               'Neighorhood preservation'), 
         z = paste('\u0394', 
                   c('mean silhouette width', 
                     'fraction of explained variance', 
                     'neighborhood preservation')), 
         v = map2(c('mean silhouette width', 
                    'fraction of explained variance', 
                    'neighborhood preservation'), 
                  se_summary$stats[1, c("sil_width", "frac_var", "frac_np")], 
                  ~paste(.x, signif(.y, 2), sep = ' = '))) %>% 
    pmap(function(x, y, z, v) se_summary$stat_deltas %>% 
           filter(dataset != 'global') %>% 
           mutate(split_factor = factor(split_factor, 
                                        names(eval(se_summary$object_expr)))) %>% 
           ggplot(aes(x = .data[[x]], 
                      y = reorder(dataset, .data[[x]]), 
                      fill = factor(sign(.data[[x]])))) + 
           geom_vline(xintercept = 0, 
                      linetype = 'dashed') + 
           geom_bar(stat = 'identity', 
                    color = 'black') + 
           facet_grid(split_factor ~ ., 
                      space = 'free', 
                      scales = 'free', 
                      labeller = as_labeller(set_names(se_summary$split_lexicon$facet_label, 
                                                       se_summary$split_lexicon$variable))) + 
           scale_fill_manual(labels = c('-1' = 'worse', 
                                        '0' = 'analysis cohort', 
                                        '1' = 'better'), 
                             values = c('-1' = 'steelblue', 
                                        '0' = 'gray60', 
                                        '1' = 'coral3'), 
                             name = 'Performance\nvs analysis cohort') + 
           scale_y_discrete(labels = function(x) exchange(x, 
                                                          se_summary$n_numbers, 
                                                          key = 'dataset', 
                                                          value = 'axis_label')) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank(), 
                 strip.text.y = element_text(angle = 0,
                                             hjust = 0)) + 
           labs(title = y, 
                subtitle = paste0('Analysis cohort: n = ', 
                                 nrow(se_globals$data), 
                                 ', ', v), 
                x = paste0(z, ', % of analysis cohort value'))) %>% 
    set_names(c('sil_width', 'frac_var', 'frac_np'))
  
# Stack plots of cluster sizes -------
  
  insert_msg('Stack plots of cluster sizes')
  
  se_summary$size_plot <- se_summary$clust_size %>% 
    mutate(split_factor = factor(split_factor, 
                                 c('global', 
                                   names(eval(se_summary$object_expr))))) %>% 
    ggplot(aes(x = percent, 
               y = reorder(dataset, percent), 
               fill = clust_id)) + 
    geom_bar(color = 'black', 
             stat = 'identity') + 
    facet_grid(split_factor ~ ., 
               space = 'free', 
               scales = 'free', 
               labeller = as_labeller(set_names(se_summary$split_lexicon$facet_label, 
                                                se_summary$split_lexicon$variable))) + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = 'Mental health cluster') + 
    scale_y_discrete(labels = function(x) exchange(x, 
                                                   se_summary$n_numbers, 
                                                   key = 'dataset', 
                                                   value = 'axis_label')) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          strip.text.y = element_text(angle = 0,
                                      hjust = 0)) + 
    labs(title = 'Cluster distribution', 
         x = '% of observations')
  
# Clustering objects for plotting -------
  
  insert_msg('Clustering objects for plotting')
  
  se_summary$clust_obj$global <- se_globals$clust_obj
  
  se_summary$clust_obj <- se_summary$object_expr %>% 
    eval %>% 
    map(~.x$clust_obj) %>% 
    map(~.x[names(.x) != 'global']) %>% 
    reduce(c) %>% 
    c(se_summary$clust_obj, .)
  
# UMAP layouts -------
  
  insert_msg('UMAP layouts')
  
  se_summary$umap_plots <- se_summary$clust_obj %>% 
    future_map(plot, 
               type = 'components', 
               with = 'data', 
               kdim = 2, 
               red_fun = 'umap', 
               train_object = se_globals$umap_obj, 
               cust_theme = globals$common_theme, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(~.x +
          scale_fill_manual(values = globals$clust_colors, 
                            name = 'Mental health cluster'))
  
  ## plot titles
  
  se_summary$umap_plots <- 
    list(x = se_summary$umap_plots, 
         y = exchange(names(se_summary$umap_plots), 
                      se_summary$dataset_lexicon)) %>% 
    pmap(function(x, y) x + 
           labs(title = stri_capitalize(y)))
  
# Heat maps of levels of the clustering factors -----

  insert_msg('Heat maps of the clustering factors')
  
  se_summary$hm_plots <- 
    list(x_object = se_summary$clust_obj, 
         plot_title = exchange(names(se_summary$clust_obj), 
                               se_summary$dataset_lexicon) %>% 
           stri_capitalize) %>% 
    pmap(plot_clust_hm, 
         y_object = se_summary$var_clust_obj, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          labs(subtitle = .x$labels$tag) + 
          theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                strip.background.y = element_blank(), 
                strip.text.y = element_blank(), 
                plot.tag = element_blank()) + 
          scale_fill_gradient2(low = 'steelblue', 
                               mid = 'black', 
                               high = 'firebrick', 
                               midpoint = 0, 
                               limits = c(-4, 4), 
                               oob = scales::squish, 
                               name = 'Z-score') + 
          scale_y_discrete(labels = function(x) exchange(x, se_summary$var_lexicon )))
  
# END ------
  
  plan('sequential')
  
  insert_tail()