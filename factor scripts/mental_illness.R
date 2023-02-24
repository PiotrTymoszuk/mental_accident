# Effects of mental illness

  insert_head()
  
# container ------
  
  mental <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  mental$variables <- fct_globals$variables$variable
  
  mental$variables <- 
    mental$variables[mental$variables != 'psych_comorbidity']
  
  ## analysis table
  
  mental$analysis_tbl <- ptsd$dataset %>% 
    select(ID, psych_comorbidity, 
           all_of(mental$variables)) %>% 
    mutate(psych_comorbidity = car::recode(as.character(psych_comorbidity), 
                                           "'no' = 'no mental illness'; 
                                           'yes' = 'mental illness'"), 
           psych_comorbidity = factor(psych_comorbidity, 
                                      c('no mental illness', 
                                        'mental illness')))
  
  ## statistical test type
  
  mental$test_type <- 
    mental$analysis_tbl[mental$variables] %>% 
    map_lgl(is.numeric)
  
  mental$test_type <- ifelse(mental$test_type, 
                             'wilcoxon_r', 'cramer_v')

# Descriptive statistic ------
  
  insert_msg('Descriptive statistics')
  
  mental$desc_stats <-  mental$analysis_tbl %>% 
    explore(variables = mental$variables, 
            split_factor = 'psych_comorbidity', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the mental illness strata ------
  
  insert_msg('Testing for differences between the mental illness strata')
  
  mental$test <-  mental$analysis_tbl %>%
    compare_variables(variables = mental$variables, 
                      split_factor = 'psych_comorbidity', 
                      what = 'eff_size', 
                      types = mental$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('mental'))) %>% 
    format_fct_test
  
# Significant and near significant differences -------
  
  insert_msg('Significant and near significant differences')
  
  mental$top_factors <- mental$test %>% 
    filter(p_value < 0.05) %>% 
    .$variable
  
# Single variable plots ------
  
  insert_msg('Single variable plots')
  
  mental$plots <- 
    plot_fct(data =  mental$analysis_tbl, 
             test_data = mental$test, 
             factor = 'psych_comorbidity', 
             fill_scale = scale_fill_brewer(palette = 'Greys'))
  
# Specific plots: PTSD, PTG and QoL scores ------
  
  insert_msg('Plot panels')
  
  mental$panels <- 
    plot_fct_panels(data =  mental$analysis_tbl, 
                    test_data = mental$test, 
                    factor = 'psych_comorbidity', 
                    fill_scale = scale_fill_brewer(palette = 'Greys', 
                                                   name = ''))
  
# Percentages of PTSD cluster-positive individuals ------
  
  insert_msg('Percentages of PTSD cluster positivity')
  
  mental$ptsd_clust_plot <- 
    plot_ptsd_freq(mental$analysis_tbl, 
                   test_data = mental$test, 
                   split_factor = 'psych_comorbidity', 
                   plot_title = 'PCL-5 DSM-5 clusters', 
                   x_lab =  '% of mental illness strata', 
                   fill_scale = scale_fill_brewer(palette = 'Greys', 
                                                  labels = mental$analysis_tbl %>% 
                                                    label_n(psych_comorbidity), 
                                                  name = ''), 
                   color_scale = scale_color_brewer(palette = 'Greys', 
                                                    labels = mental$analysis_tbl %>% 
                                                      label_n(psych_comorbidity), 
                                                    name = ''))
  
# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  mental$result_tbl <- 
    list(mental$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(mental$desc_stats))), 
         mental$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% mental$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'No mental illness', 'Mental illness', 
                'Significance', 'Effect size'))
  
# END -----
  
  insert_tail()