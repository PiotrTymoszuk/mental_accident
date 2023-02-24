# Effects of prior traumatic events

  insert_head()

# container -----

  trauma <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  ## variables
  
  trauma$variables <- fct_globals$variables$variable
  
  trauma$variables <- trauma$variables[trauma$variables != 'traumatic_event']
  
  ## analysis tables
  
  trauma$analysis_tbl <- ptsd$dataset %>% 
    select(ID, 
           traumatic_event , 
           all_of(trauma$variables)) %>% 
    filter(!is.na(traumatic_event)) %>% 
    mutate(traumatic_event  = car::recode(traumatic_event , 
                                            "'no' = 'no trauma'; 
                                            'yes' = 'trauma'"), 
           traumatic_event  = factor(traumatic_event , 
                                       c('no trauma', 
                                         'trauma')))
  
  ## statistical test type
  
  trauma$test_type <- 
    ptsd$dataset[trauma$variables] %>% 
    map_lgl(is.numeric)
  
  trauma$test_type <- ifelse(trauma$test_type, 
                             'wilcoxon_r', 'cramer_v')
  
# Descriptive statistic ------
  
  insert_msg('Descriptive statistics')
  
  trauma$desc_stats <- trauma$analysis_tbl %>% 
    explore(variables = trauma$variables, 
            split_factor = 'traumatic_event', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the trauma modes ------
  
  insert_msg('Testing for differences between the trauma strata')
  
  trauma$test <- trauma$analysis_tbl %>%
    compare_variables(variables = trauma$variables, 
                      split_factor = 'traumatic_event', 
                      what = 'eff_size', 
                      types = trauma$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('trauma'))) %>% 
    format_fct_test
  
# Significant differences -------
  
  insert_msg('Significant differences')
  
  trauma$top_factors <- trauma$test %>% 
    filter(p_value < 0.05) %>% 
    .$variable
  
# Single variable plots ------
  
  insert_msg('Single variable plots')
  
  trauma$plots <- 
    plot_fct(data = trauma$analysis_tbl, 
             test_data = trauma$test, 
             factor = 'traumatic_event', 
             fill_scale = scale_fill_brewer(palette = 'YlGn'))
  
# Specific plots: PTSD, PTG and QoL scores ------
  
  insert_msg('Plot panels')
  
  trauma$panels <- 
    plot_fct_panels(data =  trauma$analysis_tbl, 
                    test_data = trauma$test, 
                    factor = 'traumatic_event', 
                    fill_scale = scale_fill_brewer(palette = 'YlGn', 
                                                   name = ''))
  
# Percentages of PTSD cluster-positive individuals ------
  
  insert_msg('Percentages of PTSD cluster positivity')
  
  trauma$ptsd_clust_plot <- 
    plot_ptsd_freq(trauma$analysis_tbl, 
                   test_data = trauma$test, 
                   split_factor = 'traumatic_event', 
                   x_lab = '% of trauma strata', 
                   fill_scale = scale_fill_brewer(palette = 'YlGn', 
                                                  labels =  trauma$analysis_tbl %>% 
                                                    label_n(traumatic_event ), 
                                                  name = ''), 
                   color_scale = scale_color_brewer(palette = 'YlGn', 
                                                    labels =  trauma$analysis_tbl %>% 
                                                      label_n(traumatic_event ), 
                                                    name = ''))

# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  trauma$result_tbl <- 
    list(trauma$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(trauma$desc_stats))), 
         trauma$test[c('variable', 'significance', 'eff_size')]) %>% 
   # map(filter, variable %in% trauma$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'No prior traumatic event', 'Prior traumatic event', 
                'Significance', 'Effect size'))
  
# END -----
  
  insert_tail()