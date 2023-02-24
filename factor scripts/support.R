# Effects of psychological support after the accident

  insert_head()
  
# container -----
  
  support <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  support$variables <- fct_globals$variables$variable
  
  support$variables <- 
    support$variables[support$variables != 'psych_support_post_accident']
  
  ## analysis table
  
  support$analysis_tbl <- ptsd$dataset %>% 
    select(ID, psych_support_post_accident, 
           all_of(support$variables)) %>% 
    filter(!is.na(psych_support_post_accident)) %>% 
    mutate(psych_support_post_accident = car::recode(psych_support_post_accident, 
                                                     "'no' = 'no support'; 
                                                     'yes' = 'support'"))

  ## statistical test type
  
  support$test_type <- 
    ptsd$dataset[support$variables] %>% 
    map_lgl(is.numeric)
  
  support$test_type <- ifelse(support$test_type, 
                              'wilcoxon_r', 'cramer_v')
  
# Descriptive stats ------
  
  insert_msg('Descriptive statistics')
  
  support$desc_stats <- support$analysis_tbl %>% 
    explore(variables = support$variables, 
            split_factor = 'psych_support_post_accident', 
            what = 'table', 
            pub_styled = TRUE)
  
# Serial testing for differences between the support strata ------
  
  insert_msg('Testing for differences between the psych. support strata')
  
  support$test <- support$analysis_tbl %>%
    compare_variables(variables = support$variables, 
                      split_factor = 'psych_support_post_accident', 
                      what = 'eff_size', 
                      types = support$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('support'))) %>% 
    format_fct_test
  
# Significant differences -------
  
  insert_msg('Significant differences')
  
  support$top_factors <- support$test %>% 
    filter(p_value < 0.05) %>% 
    .$variable
  
# Plots for single variables -------
  
  insert_msg('Plots for single variables')
  
  support$plots <- 
    plot_fct(data = support$analysis_tbl, 
             test_data = support$test, 
             factor = 'psych_support_post_accident', 
             fill_scale = scale_fill_brewer(palette = 'Greens'))
  
# Plot panels for QoL, PTSD and PTG ------
  
  insert_msg('Plot panels for QoL, PTG and PTSD')
  
  support$panels <- 
    plot_fct_panels(data = support$analysis_tbl, 
                    test_data = support$test, 
                    factor = 'psych_support_post_accident', 
                    fill_scale = scale_fill_brewer(palette = 'Greens', 
                                                   name = ''))
  
# PTSD clusters ---------
  
  insert_msg('PTSD clusters')
  
  support$ptsd_clust_plot <- 
    plot_ptsd_freq(support$analysis_tbl, 
                   test_data = support$test, 
                   split_factor = 'psych_support_post_accident', 
                   x_lab = '% of support strata', 
                   fill_scale = scale_fill_brewer(palette = 'Greens', 
                                                  labels = support$analysis_tbl %>% 
                                                    label_n(psych_support_post_accident), 
                                                  name = ''), 
                   color_scale = scale_color_brewer(palette = 'Greens', 
                                                    labels = support$analysis_tbl %>% 
                                                      label_n(psych_support_post_accident), 
                                                    name = ''))

# Body part injuries -------
  
  insert_msg('Percentages of body part injuries')
  
  support$body_part_plot <- 
    plot_body_freq(support$analysis_tbl, 
                   test_data = support$test, 
                   split_factor = 'psych_support_post_accident', 
                   x_lab = '% of support strata', 
                   fill_scale = scale_fill_brewer(palette = 'Greens', 
                                                  labels = support$analysis_tbl %>% 
                                                    label_n(psych_support_post_accident), 
                                                  name = ''), 
                   color_scale = scale_color_brewer(palette = 'Greens', 
                                                    labels = support$analysis_tbl %>% 
                                                      label_n(psych_support_post_accident), 
                                                    name = ''))

# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  support$result_tbl <- 
    list(support$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(support$desc_stats))), 
         support$test[c('variable', 'significance', 'eff_size')]) %>% 
    #map(filter, variable %in% support$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'No support', 'Psychological support', 
                'significance', 'Effect size'))
  
# END ------
  
  insert_tail()