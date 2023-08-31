# comparison of the training and test datasets

  insert_head()

# container ------

  partition <- list()

# parallel backend ------

  insert_msg('Parallel backend')
  
  plan('multisession')
  
# descriptive stats -------
  
  insert_msg('Descriptive stats')

  partition$desc_stats <- ptsd$dataset %>% 
    explore(variables = eda_globals$variables$variable, 
            split_factor = 'partition', 
            what = 'table', 
            pub_styled = TRUE)
  
# testing -------
  
  insert_msg('Testing for differences by data partition')
  
  ## Mann-Whitney or Chi-squared test
  ## multiple variables are not normally distributed
  ##
  ## FDR correction
  
  partition$test <- 
    compare_variables(ptsd$dataset, 
                      variables = eda_globals$variables$variable, 
                      split_factor = 'partition', 
                      what = 'eff_size', 
                      types = eda_globals$variables$types, 
                      ci = FALSE, 
                      exact = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH', 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('ptsd', 'eda_globals'))) %>% 
    format_fct_test
  
# Significant and near significant differences -----
  
  insert_msg('Significant and near significant differences')
  
  partition$top_factors <- partition$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# A ready to use table with significant and near significant differences -------
  
  insert_msg('Near- and significant difference table')
  
  partition$result_tbl <- 
    list(partition$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(partition$desc_stats))), 
         partition$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% partition$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'Training', 'Test', 
                'Significance', 'Effect size'))
  
# Plots ------
  
  insert_msg('Plots')
  
  excl$plots <- plot_fct(ptsd$dataset, 
                         test_data = partition$test, 
                         factor = 'partition', 
                         fill_scale = scale_fill_brewer('Greens'))
  
# END ------
  
  plan('sequential')
