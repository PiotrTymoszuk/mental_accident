# Effects of rescue (self/partner vs professional)

  insert_head()

# container -----

  rescue <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  ## variables
  
  rescue$variables <- fct_globals$variables$variable
  
  rescue$variables <- rescue$variables[rescue$variables != 'accident_rescue']
  
  ## analysis tables
  
  rescue$analysis_tbl <- ptsd$dataset %>% 
    select(ID, 
           accident_rescue, 
           all_of(rescue$variables)) %>% 
    filter(!is.na(accident_rescue))
  
  ## statistical test type
  
  rescue$test_type <- 
    ptsd$dataset[rescue$variables] %>% 
    map_lgl(is.numeric)
  
  rescue$test_type <- ifelse(rescue$test_type, 
                             'wilcoxon_r', 'cramer_v')
  
# Descriptive statistic ------
  
  insert_msg('Descriptive statistics')
  
  rescue$desc_stats <- rescue$analysis_tbl %>% 
    explore(variables = rescue$variables, 
            split_factor = 'accident_rescue', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the rescue modes ------
  
  insert_msg('Testing for differences between the rescue strata')
  
  rescue$test <- rescue$analysis_tbl %>%
    compare_variables(variables = rescue$variables, 
                      split_factor = 'accident_rescue', 
                      what = 'eff_size', 
                      types = rescue$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('rescue'))) %>% 
    format_fct_test
  
# Significant differences -------
  
  insert_msg('Significant differences')
  
  rescue$top_factors <- rescue$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# Single variable plots ------
  
  insert_msg('Single variable plots')
  
  rescue$plots <- 
    plot_fct(data = rescue$analysis_tbl, 
             test_data = rescue$test, 
             factor = 'accident_rescue', 
             fill_scale = scale_fill_brewer(palette = 'Purples'))
  
# Specific plots: PTSD, PTG and QoL scores ------
  
  insert_msg('Plot panels')
  
  rescue$panels <- 
    plot_fct_panels(data =  rescue$analysis_tbl, 
                    test_data = rescue$test, 
                    factor = 'accident_rescue', 
                    fill_scale = scale_fill_brewer(palette = 'Purples', 
                                                   name = 'Rescue'))
  
# Percentages of PTSD cluster-positive individuals ------
  
  insert_msg('Percentages of PTSD cluster positivity')
  
  rescue$ptsd_clust_plot <- 
    plot_ptsd_freq(rescue$analysis_tbl, 
                   test_data = rescue$test, 
                   split_factor = 'accident_rescue', 
                   x_lab = '% of rescue strata', 
                   fill_scale = scale_fill_brewer(palette = 'Purples', 
                                                  labels =  rescue$analysis_tbl %>% 
                                                    label_n(accident_rescue), 
                                                  name = ''), 
                   color_scale = scale_color_brewer(palette = 'Purples', 
                                                    labels =  rescue$analysis_tbl %>% 
                                                      label_n(accident_rescue), 
                                                    name = ''))

# Body part injuries -------
  
  insert_msg('Percentages of body part injuries')
  
  rescue$body_part_plot <- 
    plot_body_freq(rescue$analysis_tbl, 
                   test_data = rescue$test, 
                   split_factor = 'accident_rescue', 
                   x_lab = '% of rescue mode strata', 
                   fill_scale = scale_fill_brewer(palette = 'Purples', 
                                                  labels = rescue$analysis_tbl %>% 
                                                    label_n(accident_rescue), 
                                                  name = 'Rescue'), 
                   color_scale = scale_color_brewer(palette = 'Purples', 
                                                    labels = rescue$analysis_tbl %>% 
                                                      label_n(accident_rescue), 
                                                    name = 'Rescue'))
  
# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  rescue$result_tbl <- 
    list(rescue$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(rescue$desc_stats))), 
         rescue$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% rescue$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 
                'Self rescue', 
                'Partner/third party rescue', 
                'Professional rescue', 
                'Significance',
                'Effect size'))
  
# END -----
  
  insert_tail()