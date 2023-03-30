# Differences in levels of demographic, socioeconomic and clinical factors 
# as well as variables related to the accident between the clusters

  insert_head()
  
# container -------
  
  clust_bcg <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## explanatory variables 
  
  clust_bcg$variables <- ptsd$var_lexicon %>% 
    filter(type %in% c('characteristic')) %>% 
    .$variable

  ## cluster assignment schemes
  
  clust_bcg$clust_assignment <- semi_clust$clust_obj %>% 
    map(~.x$clust_assignment) %>% 
    map(set_names, c('ID', 'clust_id'))
  
  ## analysis table, splitting into the training and test subset
  ## appending with the clust assignment scheme
  
  clust_bcg$analysis_tbl <- ptsd$dataset %>% 
    dlply('partition', 
          select, 
          ID, 
          all_of(clust_bcg$variables)) %>% 
    map(as_tibble)
  
  clust_bcg$analysis_tbl <- 
    map2(clust_bcg$analysis_tbl, 
         clust_bcg$clust_assignment, 
         right_join, by = 'ID')
  
  ## test types
  
  clust_bcg$test_types <- 
    clust_bcg$analysis_tbl$training[clust_bcg$variables] %>% 
    map_lgl(is.numeric) %>% 
    ifelse('kruskal_eta', 'cramer_v')

# Descriptive stats ------  
  
  insert_msg('Descriptive stats')
  
  clust_bcg$desc_stats <- clust_bcg$analysis_tbl %>% 
    future_map(explore, 
               split_factor = 'clust_id', 
               variables = clust_bcg$variables, 
               what = 'table', 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE))
  
# Testing ------
  
  insert_msg('Testing for differences between the clusters')
  
  ## all variables are categorical: chi-squared test with Cramer V effect size
  
  clust_bcg$test <- clust_bcg$analysis_tbl %>% 
    future_map(compare_variables, 
               variables = clust_bcg$variables, 
               split_factor = 'clust_id', 
               what = 'eff_size', 
               types = clust_bcg$test_types, 
               ci = FALSE, 
               exact = FALSE, 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(format_fct_test) %>% 
    map(mutate, 
        y_lab = '% of cluster')

# Significant factors ------
  
  insert_msg('Significant factors')
  
  ## significant and nearly-significant in both subsets
  
  clust_bcg$top_factors <- clust_bcg$test %>% 
    map(filter, p_value < 0.05) %>% 
    map(~.x$variable) %>% 
    reduce(intersect)
  
# Plots --------
  
  insert_msg('Plots for single variables')
  
  clust_bcg$plots <- 
    list(data = clust_bcg$analysis_tbl, 
         test_data = clust_bcg$test) %>% 
    future_pmap(plot_fct, 
                factor = 'clust_id', 
                fill_scale = scale_fill_brewer(palette = 'Reds'), 
                .options = furrr_options(seed = TRUE))
  
  ## appending the plot titles with the subset information
  
  clust_bcg$plots$training <- clust_bcg$plots$training %>% 
    map(~.x + 
          labs(title = paste0(.x$labels$title, ', training')))
  
  clust_bcg$plots$test <- clust_bcg$plots$test %>% 
    map(~.x + 
          labs(title = paste0(.x$labels$title, ', test')))
  
# Injured body parts -------
  
  insert_msg('Injured body parts')
  
  clust_bcg$body_part_plots <- 
    list(data = clust_bcg$analysis_tbl, 
         test_data = clust_bcg$test) %>% 
    future_pmap(plot_body_freq, 
                split_factor = 'clust_id', 
                fill_scale = scale_fill_manual(values = globals$clust_colors, 
                                               name = 'Cluster'), 
                color_scale = scale_color_manual(values = globals$clust_colors, 
                                                 name = 'Cluster'), 
                .options = furrr_options(seed = TRUE))
  
  clust_bcg$body_part_plots <- 
    list(x = clust_bcg$body_part_plots, 
         y =  clust_bcg$analysis_tbl %>% 
           map(label_n, clust_id, sep = ': n = ') %>% 
           map(paste, collapse = ', '), 
         z = c('Injured body regions, training', 
               'Injured body regions, test')) %>% 
    pmap(function(x, y, z) x + 
           labs(title = z, 
                subtitle = y, 
                x = '% of cluster'))
  
# Key differences between the clusters in a table ------
  
  insert_msg('Key differences between the clusters, table')
  
  clust_bcg$n_numbers <- semi_clust$clust_obj %>% 
    map(ngroups) %>% 
    map(~tibble(variable = 'Participants, n', 
                neutral = .x$n[1], 
                PTG = .x$n[2], 
                PTB = .x$n[3]))
  
  clust_bcg$result_tbl <- 
    map2(clust_bcg$desc_stats %>% 
           map(reduce, left_join, by = 'variable') %>% 
           map(set_names, c('variable', names(clust_bcg$desc_stats[[1]]))), 
         clust_bcg$test %>% 
           map(filter, p_value < 0.05) %>% 
           map(select, variable, significance, eff_size), 
         right_join, by = 'variable') %>% 
    map(format_summ_tbl) %>% 
    map2(., clust_bcg$n_numbers, 
         ~full_rbind(.y, .x)) %>% 
    map(~map_dfc(.x, function(x) stri_replace(x, 
                                              regex = '\nn\\s{1}=\\s{1}\\d+', 
                                              replacement = ''))) %>% 
    map(set_names, 
        c('Variable', 'Neutral cluster', 'PTG cluster', 'PTB cluster', 
          'Significance', 'Effect size'))
  
# END ------
  
  plan('sequential')
  
  insert_tail()