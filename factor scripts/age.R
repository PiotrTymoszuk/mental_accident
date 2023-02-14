# Analysis for age classes 
# (young adults: <= 30, middle-aged: <= 65, elderly: > 65)

  insert_head()
  
# container -----
  
  age <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  age$variables <- fct_globals$variables$variable
  
  age$variables <- age$variables[age$variables != 'age']
  
  ## analysis table
  
  age$analysis_tbl <- ptsd$dataset %>% 
    mutate(age = cut(age, 
                     c(-Inf, 30, 65, Inf), 
                     c('young', 'middle-aged', 'elderly'))) %>% 
    select(ID, age, all_of(age$variables))
  
  ## statistical test type
  
  age$test_type <- 
    age$analysis_tbl[age$variables] %>% 
    map_lgl(is.numeric)
  
  age$test_type <- ifelse(age$test_type, 
                          'kruskal_eta', 'cramer_v')
  
# Descriptive statistic ------
  
  insert_msg('Descriptive statistics')
  
  age$desc_stats <- age$analysis_tbl %>% 
    explore(variables = age$variables, 
            split_factor = 'age', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the age groups ------
  
  insert_msg('Testing for differences between the age strata')
  
  age$test <- age$analysis_tbl %>%
    compare_variables(variables = age$variables, 
                      split_factor = 'age', 
                      what = 'eff_size', 
                      types = age$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      adj_method = 'BH', 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('age'))) %>% 
    format_fct_test
  
# Significant and near significant differences -------
  
  insert_msg('Significant and near significant differences')
  
  age$top_factors <- age$test %>% 
    filter(p_adjusted < 0.1) %>% 
    .$variable

# Single variable plots ------
  
  insert_msg('Single variable plots')
  
  age$plots <- 
    plot_fct(data = age$analysis_tbl, 
             test_data = age$test, 
             factor = 'age', 
             fill_scale = scale_fill_brewer(palette = 'Blues'))
  
# Specific plots: PTSD, PTG and QoL scores ------
  
  insert_msg('Plot panels')
  
  ## variables
  
  age$panels <- 
    plot_fct_panels(data = age$analysis_tbl, 
                    test_data = age$test, 
                    factor = 'age', 
                    fill_scale = scale_fill_brewer(palette = 'Blues'))

# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  age$result_tbl <- 
    list(age$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(age$desc_stats))), 
         age$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% age$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'Young adult', 'Middle-aged', 
                'Elderly', 'significance (FDR)', 'Effect size'))
  
# END ------
  
  insert_tail()