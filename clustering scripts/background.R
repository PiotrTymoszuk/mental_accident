# Differences in levels of demographic, socioeconomic and clinical factors 
# as well as variables related to the accident between the clusters
#
# No splitting into the test and training subset!

  insert_head()
  
# container -------
  
  clust_bcg <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## mental disorder symptoms
  
  clust_bcg$mental_variables <- 
    c('dsm5_cluster_class', 
      'dsm5_B_class', 
      'dsm5_C_class', 
      'dsm5_D_class', 
      'dsm5_E_class', 
      'unwilling_flashback', 
      'flashback_frequency', 
      'rs13_total_class', 
      'phq9_total_class', 
      'gad7_total_class', 
      'phqd_panic_total_class', 
      'phq_events_total_class')
  
  ## background demographic and clinical variables
  
  clust_bcg$bcg_variables <- ptsd$var_lexicon %>% 
    filter(type %in% c('characteristic')) %>% 
    .$variable
  
  clust_bcg$bcg_variables <- 
    clust_bcg$bcg_variables[!clust_bcg$bcg_variables %in% clust_bcg$mental_variables]
  
  ## explanatory variables 

  clust_bcg$variables <- 
    c(clust_bcg$bcg_variables, 
      clust_bcg$mental_variables)

  ## cluster assignment schemes
  
  clust_bcg$clust_assignment <- semi_clust$assignment
  
  ## analysis table, appending with the cluster assignment scheme
  
  clust_bcg$analysis_tbl <- ptsd$dataset %>% 
    blast(partition) %>% 
    map(select, 
        ID, 
        all_of(clust_bcg$variables)) %>% 
    map(as_tibble)
  
  clust_bcg$analysis_tbl <- 
    map2_dfr(clust_bcg$analysis_tbl, 
             clust_bcg$clust_assignment, 
             right_join, by = 'ID')
  
  ## test types
  
  clust_bcg$test_types <- 
    clust_bcg$analysis_tbl[clust_bcg$variables] %>% 
    map_lgl(is.numeric) %>% 
    ifelse('kruskal_eta', 'cramer_v')

# Descriptive stats ------  
  
  insert_msg('Descriptive stats')
  
  clust_bcg$desc_stats <- clust_bcg$analysis_tbl %>% 
    explore(split_factor = 'clust_id', 
            variables = clust_bcg$variables, 
            what = 'table', 
            pub_styled = TRUE)

# Testing ------
  
  insert_msg('Testing for differences between the clusters')
  
  clust_bcg$test <- clust_bcg$analysis_tbl %>% 
    compare_variables(variables = clust_bcg$variables, 
                      split_factor = 'clust_id', 
                      what = 'eff_size', 
                      types = clust_bcg$test_types, 
                      ci = FALSE, 
                      exact = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    format_fct_test %>% 
    mutate(y_lab = '% of cluster')

# Significant factors ------
  
  insert_msg('Significant factors')
  
  ## significant in both subsets
  
  clust_bcg$top_factors <- clust_bcg$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable

# Plots --------
  
  insert_msg('Plots for single variables')
  
  clust_bcg$plots <- 
    plot_fct(data = clust_bcg$analysis_tbl, 
             test_data = clust_bcg$test, 
             factor = 'clust_id', 
             fill_scale = scale_fill_brewer(palette = 'Reds'))

# Injured body parts -------
  
  insert_msg('Injured body parts')
  
  clust_bcg$body_part_plots <- 
    plot_body_freq(data = clust_bcg$analysis_tbl, 
                   test_data = clust_bcg$test, 
                   split_factor = 'clust_id', 
                   fill_scale = scale_fill_manual(values = globals$clust_colors, 
                                                  name = 'Cluster'), 
                   color_scale = scale_color_manual(values = globals$clust_colors, 
                                                    name = 'Cluster')) +
    labs(subtitle = clust_bcg$analysis_tbl %>% 
           label_n(clust_id, sep = ': n = ') %>% 
           paste(collapse = ', '), 
         x = '% of cluster')

# Summary result table ------
  
  insert_msg('Key differences between the clusters, table')
  
  clust_bcg$n_numbers <- clust_bcg$analysis_tbl %>% 
    count(clust_id)
  
  clust_bcg$n_numbers <- 
    tibble(variable = 'Participants, n', 
           !!as.character(clust_bcg$n_numbers$clust_id[1]) := clust_bcg$n_numbers$n[1], 
           !!as.character(clust_bcg$n_numbers$clust_id[2]) := clust_bcg$n_numbers$n[2], 
           !!as.character(clust_bcg$n_numbers$clust_id[3]) := clust_bcg$n_numbers$n[3])

  clust_bcg$result_tbl <- 
    right_join(clust_bcg$desc_stats %>% 
                reduce(left_join, by= 'variable') %>% 
                set_names(c('variable', names(clust_bcg$desc_stats))), 
              clust_bcg$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_summ_tbl %>% 
    full_rbind(clust_bcg$n_numbers , .) %>% 
    map_dfc(stri_replace, 
            regex = '\nn\\s{1}=\\s{1}\\d+', 
            replacement = '') %>% 
    set_names( c('Variable', 'Neutral cluster', 'PTG cluster', 'PTS cluster', 
                 'Significance', 'Effect size')) %>% 
    mutate(source_var = c('n_number', clust_bcg$variables)) %>% 
    relocate(source_var)
  
# Bar plots for frequencies of PTSD symptoms ------
  
  insert_msg('PTSD symptoms')
  
  ## plotting variables and their axis labs
  
  clust_bcg$ptsd_symptoms$variables <- 
    c('unwilling_flashback', 
      'dsm5_cluster_class', 
      'dsm5_B_class', 
      'dsm5_C_class', 
      'dsm5_D_class', 
      'dsm5_E_class')
  
  clust_bcg$ptsd_symptoms$ax_labs <- 
    symptom_ax_labs(test_data = clust_bcg$test, 
                    variables = clust_bcg$ptsd_symptoms$variables)
  
  ## the plot
  
  clust_bcg$ptsd_symptoms$plot <- 
    plot_ptsd_freq(data = clust_bcg$analysis_tbl, 
                   test_data = clust_bcg$test, 
                   variables = clust_bcg$ptsd_symptoms$variables, 
                   split_factor = 'clust_id', 
                   plot_title = 'PTSD symptoms, PCL-5 domains', 
                   plot_subtitle = clust_bcg$analysis_tbl %>% 
                     label_n(clust_id, sep = ': ') %>% 
                     paste(collapse = ', '), 
                   x_lab = '% of cluster', 
                   ptsd_labs = FALSE, 
                   fill_scale = scale_fill_manual(values = globals$clust_colors, 
                                                  name = 'Cluster'), 
                   color_scale = scale_color_manual(values = globals$clust_colors, 
                                                    name = 'Cluster')) + 
    scale_y_discrete(limits = rev(clust_bcg$ptsd_symptoms$variables), 
                     labels = clust_bcg$ptsd_symptoms$ax_labs) + 
    theme(axis.text.y = element_markdown())
  
# Bar plots for other mental symptoms -------
  
  insert_msg('Bar plots for other mental symptoms')
  
  ## variables and axis labels
  
  clust_bcg$mental_symptoms$variables <- 
    c('phq9_total_class', 
      'gad7_total_class', 
      'phqd_panic_total_class', 
      'phq_events_total_class')
  
  clust_bcg$mental_symptoms$ax_labs <- 
    symptom_ax_labs(test_data = clust_bcg$test, 
                    variables = clust_bcg$mental_symptoms$variables) 

  ## the plot
  
  clust_bcg$mental_symptoms$plot <- 
    plot_ptsd_freq(data = clust_bcg$analysis_tbl, 
                   test_data = clust_bcg$test, 
                   variables = clust_bcg$mental_symptoms$variables, 
                   split_factor = 'clust_id', 
                   plot_title = 'Mental disorder symptoms', 
                   plot_subtitle = clust_bcg$analysis_tbl %>% 
                     label_n(clust_id, sep = ': ') %>% 
                     paste(collapse = ', '), 
                   x_lab = '% of cluster', 
                   ptsd_labs = FALSE, 
                   fill_scale = scale_fill_manual(values = globals$clust_colors, 
                                                  name = 'Cluster'), 
                   color_scale = scale_color_manual(values = globals$clust_colors, 
                                                    name = 'Cluster')) + 
    scale_y_discrete(limits = rev(clust_bcg$mental_symptoms$variables), 
                     labels = clust_bcg$mental_symptoms$ax_labs) + 
    theme(axis.text.y = element_markdown())
  
# Post-hoc test for frequency of sport types --------
  
  insert_msg('Post hoc test for differences in sport types')
  
  clust_bcg$sport_post_hoc <- 
    pairwise_chisq_test(clust_bcg$analysis_tbl, 
                        variable = 'sport_type', 
                        split_factor = 'clust_id') %>% 
    mutate(significant = ifelse(p_adjusted < 0.05, 'yes', 'no'), 
           post_lab = paste(eff_size, significance, sep = ', '), 
           post_lab = paste(sport_type, post_lab, sep = '<br>'), 
           post_lab = ifelse(significant == 'yes', 
                             paste0('<b>', post_lab, '</b>'), 
                             post_lab))
  
# Bar plot for the sport types --------
  
  insert_msg('Bar plot for the sport types')
  
  ## fill labels with total n numbers

  clust_bcg$sport_fill_labs <- clust_bcg$analysis_tbl$clust_id %>% 
    table %>% 
    map2_chr(names(.), ., 
             paste, sep = ': n = ') %>% 
    set_names(levels(clust_bcg$analysis_tbl$clust_id))
    
  ## plotting data
  
  clust_bcg$sport_panel_data <- clust_bcg$analysis_tbl %>% 
    count(clust_id, sport_type, .drop = FALSE) %>% 
    group_by(clust_id) %>% 
    mutate(n_cluster = sum(n), 
           perc_cluster = n/n_cluster * 100) %>% 
    ungroup %>% 
    mutate(season = ifelse(sport_type %in% c('hiking', 
                                             'biking', 
                                             'climbing', 
                                             'air sport', 
                                             'mountaineering', 
                                             'water sport'), 
                           'summer', 'winter'), 
           season = factor(season, c('winter', 'summer')))
  
  ## bar plot
  
  clust_bcg$sport_type_panel <- clust_bcg$sport_panel_data %>% 
    ggplot(aes(x = perc_cluster, 
               y = sport_type, 
               fill = clust_id)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    geom_text(aes(label = signif(perc_cluster, 2), 
                  color = clust_id), 
              size = 2.5, 
              show.legend = FALSE, 
              position = position_dodge(0.9), 
              hjust = -0.4) + 
    facet_grid(season ~ ., 
               space = 'free', 
               scales = 'free') + 
    scale_y_discrete(labels = set_names(clust_bcg$sport_post_hoc$post_lab, 
                                        clust_bcg$sport_post_hoc$sport_type)) + 
    scale_fill_manual(values = globals$clust_colors, 
                      labels = clust_bcg$sport_fill_labs, 
                      name = '') + 
    scale_color_manual(values = globals$clust_colors, 
                       name = '') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_markdown()) + 
    labs(title = 'Sport type', 
         subtitle = filter(clust_bcg$test, variable == 'sport_type')$plot_cap, 
         x = '% of cluster')

# END ------
  
  plan('sequential')
  
  insert_tail()