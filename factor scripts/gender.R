# Effects of gender

  insert_head()
  
# container -----
  
  gender <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  gender$variables <- fct_globals$variables$variable
  
  gender$variables <- gender$variables[gender$variables != 'sex']

  ## statistical test type
  
  gender$test_type <- 
    ptsd$dataset[gender$variables] %>% 
    map_lgl(is.numeric)
  
  gender$test_type <- ifelse(gender$test_type, 
                             'wilcoxon_r', 'cramer_v')
  
# Descriptive statistic ------
  
  insert_msg('Descriptive statistics')
  
  gender$desc_stats <- ptsd$dataset %>% 
    explore(variables = gender$variables, 
            split_factor = 'sex', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the genders ------
  
  insert_msg('Testing for differences between the gender strata')
  
  gender$test <- ptsd$dataset %>%
    compare_variables(variables = gender$variables, 
                      split_factor = 'sex', 
                      what = 'eff_size', 
                      types = gender$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      adj_method = 'BH', 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('ptsd'))) %>% 
    format_fct_test
  
# Significant and near significant differences -------
  
  insert_msg('Significant and near significant differences')
  
  gender$top_factors <- gender$test %>% 
    filter(p_adjusted < 0.1) %>% 
    .$variable
  
# Single variable plots ------
  
  insert_msg('Single variable plots')
  
  gender$plots <- 
    plot_fct(data = ptsd$dataset, 
             test_data = gender$test, 
             factor = 'sex', 
             fill_scale = scale_fill_brewer(palette = 'Oranges'))
  
# Specific plots: PTSD, PTG and QoL scores ------
  
  insert_msg('Plot panels')
  
  gender$panels <- 
    plot_fct_panels(data = ptsd$dataset, 
                    test_data = gender$test, 
                    factor = 'sex', 
                    fill_scale = scale_fill_brewer(palette = 'Oranges'))
  
# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  gender$result_tbl <- 
    list(gender$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(gender$desc_stats))), 
         gender$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% gender$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'Female', 'Male', 
                'Significance (FDR)', 'Effect size'))
  
# END -----
  
  insert_tail()