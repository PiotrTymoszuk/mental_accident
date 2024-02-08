# Difference in frequency of symptoms of mental disorders in bi-annual 
# classes of the admission - survey time (observation time)

  insert_head()
  
# container -------
  
  time_sympt <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## binary variables of mental disorder symptoms
  
  time_sympt$variables <- ptsd$dataset %>% 
    select(ends_with('class')) %>% 
    select(starts_with('dsm'), 
           starts_with('rs13'), 
           starts_with('phq'), 
           starts_with('gad7')) %>% 
    names
  
  ## variable lexicon 
  
  time_sympt$var_lexicon <- ptsd$var_lexicon %>% 
    filter(variable %in% time_sympt$variables) %>% 
    select(variable, label) %>% 
    rbind(tibble(variable = c('phq2_total_class', 
                              'phq8_total_class'), 
                 label = paste('clinically relevant depression symptoms', 
                               c('(PHQ-2 \u22653)', 
                                 '(PHQ-8)'))))
  
  ## analysis table
  
  time_sympt$analysis_tbl <- ptsd$dataset %>% 
    select(ID, 
           obs_time, 
           all_of(time_sympt$variables)) %>% 
    mutate(time_class = cut(obs_time, 
                            c(-Inf, 1:2 * 730, Inf),
                            c('6-24 months', '25-48 months', '49+ months')))
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  time_sympt$stats <- time_sympt$analysis_tbl %>% 
    explore(split_factor = 'time_class', 
            variables = time_sympt$variables, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(time_sympt$analysis_tbl$time_class)))
  
# testing for time-dependent differences with chi-squared test -----
  
  insert_msg('Testing for differences')
  
  time_sympt$test <- time_sympt$analysis_tbl %>% 
    compare_variables(variables = time_sympt$variables,
                      split_factor = 'time_class', 
                      what = 'eff_size', 
                      types = 'cramer_v', 
                      ci = FALSE, 
                      exact = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))

# Stack plots -----
  
  insert_msg('Stack plots')
  
  time_sympt$plots <- 
    list(variable = time_sympt$test$variable, 
         plot_title = exchange(time_sympt$test$variable, 
                               dict = time_sympt$var_lexicon) %>% 
           stri_capitalize, 
         plot_subtitle = time_sympt$test$plot_cap) %>% 
    pmap(plot_variable, 
         time_sympt$analysis_tbl, 
         split_factor = 'time_class', 
         type = 'stack', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_lab = 'Admission to survey time', 
         y_lab = '% of strata', 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 
                                       'coral2', 
                                       'coral4', 
                                       'gray60'), 
                            name = '')) %>% 
    set_names(time_sympt$variables)
  
# END -----
  
  insert_tail()