# Comparison of the individuals included in the analysis 
# and those excluded by any reason

  insert_head()
  
# container ------
  
  excl <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## an analysis table containing the study variables for the excluded
  ## and included individuals
  
  excl$analysis_tbl <- 
    list(included = ptsd$dataset, 
         excluded = ptsd$cleared %>% 
           filter(!ID %in% ptsd$dataset$ID)) %>% 
    map(select, 
        ID, 
        all_of(eda_globals$variables$variable)) %>% 
    compress(names_to = 'analysis_status') %>% 
    mutate(analysis_status = factor(analysis_status, 
                                    c('included', 'excluded')))
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  excl$desc_stats <- excl$analysis_tbl %>% 
    explore(variables = eda_globals$variables$variable, 
            split_factor = 'analysis_status', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the analysis groups ------
  
  insert_msg('Testing for differences by analysis status')
  
  ## Mann-Whitney or Chi-squared test
  ## multiple variables are not normally distributed
  ##
  ## working with safely(): some variables are so missing in the excluded 
  ## set that no testing is possible
  ##
  ## FDR correction
  
  excl$test <- 
    list(variables = eda_globals$variables$variable, 
         types = eda_globals$variables$types) %>% 
    future_pmap(safely(compare_variables), 
                excl$analysis_tbl, 
                split_factor = 'analysis_status', 
                what = 'eff_size', 
                ci = FALSE, 
                exact = FALSE, 
                pub_styled = TRUE, 
                .options = furrr_options(seed = TRUE)) %>% 
    map_dfr(~.x$result) %>% 
    re_adjust %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '), 
           y_lab = ifelse(test == 'Chi-squared test', 
                          '% of strata', 
                          exchange(variable, 
                                   dict = ptsd$var_lexicon, 
                                   key = 'variable', 
                                   label = 'axis_lab')), 
           plot_type = ifelse(test == 'Chi-squared test', 
                              'stack', 
                              'violin'))
  
# Significant and near significant differences -----
  
  insert_msg('Significant and near significant differences')
  
  excl$top_factors <- excl$test %>% 
    filter(p_value < 0.05) %>% 
    .$variable
  
# A ready to use table with significant and near significant differences -------
  
  insert_msg('Near- and significant difference table')
  
  excl$result_tbl <- 
    list(excl$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(excl$desc_stats))), 
         excl$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% excl$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'Included', 'Excluded', 
                'Significance (FDR)', 'Effect size'))
  
# Plots ------
  
  insert_msg('Plots')
  
  excl$plots <- 
    list(variable = excl$test$variable, 
         plot_title = excl$test$variable %>% 
           exchange(dict = ptsd$var_lexicon, 
                    key = 'variable', 
                    label = 'label') %>% 
           stri_capitalize, 
         plot_subtitle = excl$test$plot_cap, 
         y_lab = excl$test$y_lab, 
         type = excl$test$plot_type) %>% 
    pmap(plot_variable, 
         excl$analysis_tbl, 
         split_factor = 'analysis_status', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_lab = 'Analysis status', 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_brewer()) %>% 
    set_names(excl$test$variable)
  
# END -------
  
  plan('sequential')
  
  insert_tail()