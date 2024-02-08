# Effects of the observation time (survey - admission) of psychometric variable 
# values.

  insert_tail()
  
# container ------
  
  time_psych <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## psychometric variables
  
  time_psych$variables <- ptsd$mental_variables
  
  ## analysis table, bi-year stratification
  
  time_psych$analysis_tbl <- ptsd$dataset %>% 
    select(ID, obs_time, all_of(time_psych$variables)) %>% 
    mutate(time_class = cut(obs_time, 
                            c(-Inf, 1:2 * 730, Inf),
                            c('6-24 months', '25-48 months', '49+ months')))
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  time_psych$stats <- time_psych$analysis_tbl %>% 
    explore(split_factor = 'time_class', 
            variables = time_psych$variables, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(time_psych$analysis_tbl$time_class)))
  
# Testing for differences with Kruskal-Wallis test ------
  
  insert_msg('Testing for differences with Kruskal-Wallis test')
  
  time_psych$test <- time_psych$analysis_tbl %>% 
    compare_variables(variables = time_psych$variables, 
                      split_factor = 'time_class', 
                      what = 'eff_size', 
                      types = 'kruskal_etasq', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots -------
  
  insert_msg('Plots')
  
  time_psych$plots <- 
    list(variable = time_psych$test$variable, 
         plot_title = exchange(time_psych$test$variable, 
                               dict = ptsd$var_lexicon) %>% 
           stri_capitalize, 
         plot_subtitle = time_psych$test$plot_cap) %>% 
    pmap(plot_variable, 
         time_psych$analysis_tbl, 
         split_factor = 'time_class', 
         type = 'violin', 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE, 
         x_lab = 'Admission to survey time', 
         y_lab = 'score') %>% 
    map(~.x + 
          scale_fill_manual(values = c('cornsilk', 
                                       'coral2', 
                                       'coral4'))) %>% 
    set_names(time_psych$variables)
  
# END ------
  
  insert_tail()