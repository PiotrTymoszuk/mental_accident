# Comparison of the individuals included in the analysis 
# and those excluded
#
# Two comparisons are made: 
# (1) invited vs included in the analysis
# (2) excluded due to data missingness vs included in the analysis

  insert_head()
  
# container ------
  
  excl <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables: no psychometric features
  
  excl$variables <- eda_globals$variables %>% 
    filter(!variable %in% ptsd$mental_variables)
  
  ## analysis tables
  
  excl$analysis_tbl$no_response <- 
    list(included = ptsd$dataset, 
         excluded = ptsd$cleared %>% 
           filter(!ID %in% ptsd$interview_ID))
  
  excl$analysis_tbl$missing <- 
    list(included = ptsd$dataset, 
         excluded = ptsd$cleared %>% 
           filter(ID %in% ptsd$interview_ID) %>% 
           filter(!ID %in% ptsd$dataset$ID))
  
  excl$analysis_tbl <- excl$analysis_tbl %>% 
    map(map, 
        select, 
        ID, 
        all_of(eda_globals$variables$variable)) %>% 
    map(compress, 
        names_to = 'analysis_status') %>% 
    map(mutate, 
        analysis_status = factor(analysis_status, 
                                 c('included', 'excluded')), 
        accident_injured_persons = ifelse(accident_injured_persons == 'no information', 
                                          NA, as.character(accident_injured_persons)), 
        accident_injured_persons = factor(accident_injured_persons, 
                                          levels(ptsd$dataset$accident_injured_persons)), 
        accident_injured_persons = droplevels(accident_injured_persons))

# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  excl$desc_stats <- excl$analysis_tbl %>% 
    future_map(explore, 
               variables =   excl$variables$variable, 
               split_factor = 'analysis_status', 
               what = 'table', 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, 
        c('variable', levels(excl$analysis_tbl[[1]]$analysis_status)))
  
# Testing for differences between the analysis groups ------
  
  insert_msg('Testing for differences by analysis status')
  
  ## Mann-Whitney or Chi-squared test
  ## multiple variables are not normally distributed
  ##
  ## working with safely(): some variables are completely missing in the 
  ## excluded dataset that no testing is possible

  excl$test <- excl$analysis_tbl %>% 
    map(function(data) list(variables =   excl$variables$variable, 
                            types =   excl$variables$types) %>% 
          future_pmap(safely(compare_variables), 
                      data, 
                      split_factor = 'analysis_status', 
                      what = 'eff_size', 
                      ci = FALSE, 
                      exact = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH', 
                      .options = furrr_options(seed = TRUE)) %>% 
          map_dfr(~.x$result) %>% 
          format_fct_test) %>% 
    map(filter, !is.na(p_value))
  
# Significant differences -----
  
  insert_msg('Significant differences')
  
  excl$top_factors <- excl$test %>% 
    map(filter, p_adjusted < 0.05) %>% 
    map(~.x$variable)
  
# A ready to use table with significant and near significant differences -------
  
  insert_msg('Near- and significant difference table')
  
  excl$result_tbl <- 
    map2(excl$desc_stats, 
         map(excl$test, 
             ~.x[c('variable', 'significance', 'eff_size')]), 
         left_join, by = 'variable') %>% 
    map2(excl$top_factors, 
         ~filter(.x, variable %in% .y)) %>% 
    map(format_summ_tbl) %>% 
    map2(list(c('Variable', 'Included', 'No response', 
                'Significance', 'Effect size'), 
              c('Variable', 'Included', 'Incomplete variables', 
                'Significance', 'Effect size')), 
         set_names)

# Plots ------
  
  insert_msg('Plots')
  
  excl$plots <- 
    list(data = excl$analysis_tbl, 
         test_data = excl$test) %>% 
    pmap(plot_fct, 
         factor = 'analysis_status', 
         fill_scale = scale_fill_brewer())

# END -------
  
  plan('sequential')
  
  insert_tail()