# Effects of somatic accident consequences

  insert_head()

# container -----

  conseq <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  ## variables
  
  conseq$variables <- fct_globals$variables$variable
  
  conseq$variables <- conseq$variables[conseq$variables != 'accident_aftermath']
  
  ## analysis tables
  
  conseq$analysis_tbl <- ptsd$dataset %>% 
    select(ID, 
           accident_aftermath, 
           all_of(conseq$variables)) %>% 
    filter(!is.na(accident_aftermath)) %>% 
    mutate(accident_aftermath = car::recode(accident_aftermath, 
                                            "'no' = 'no conseqences'; 
                                            'yes' = 'conseqences'"), 
           accident_aftermath = factor(accident_aftermath, 
                                       c('no conseqences', 
                                         'conseqences')))
  
  ## statistical test type
  
  conseq$test_type <- 
    ptsd$dataset[conseq$variables] %>% 
    map_lgl(is.numeric)
  
  conseq$test_type <- ifelse(conseq$test_type, 
                             'wilcoxon_r', 'cramer_v')
  
# Descriptive statistic ------
  
  insert_msg('Descriptive statistics')
  
  conseq$desc_stats <- conseq$analysis_tbl %>% 
    explore(variables = conseq$variables, 
            split_factor = 'accident_aftermath', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the conseq modes ------
  
  insert_msg('Testing for differences between the conseq strata')
  
  conseq$test <- conseq$analysis_tbl %>%
    compare_variables(variables = conseq$variables, 
                      split_factor = 'accident_aftermath', 
                      what = 'eff_size', 
                      types = conseq$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('conseq'))) %>% 
    format_fct_test
  
# Significant and near significant differences -------
  
  insert_msg('Significant differences')
  
  conseq$top_factors <- conseq$test %>% 
    filter(p_value < 0.05) %>% 
    .$variable
  
# Single variable plots ------
  
  insert_msg('Single variable plots')
  
  conseq$plots <- 
    plot_fct(data = conseq$analysis_tbl, 
             test_data = conseq$test, 
             factor = 'accident_aftermath', 
             fill_scale = scale_fill_brewer(palette = 'PuRd'))
  
# Specific plots: PTSD, PTG and QoL scores ------
  
  insert_msg('Plot panels')
  
  conseq$panels <- 
    plot_fct_panels(data =  conseq$analysis_tbl, 
                    test_data = conseq$test, 
                    factor = 'accident_aftermath', 
                    fill_scale = scale_fill_brewer(palette = 'PuRd', 
                                                   name = ''))
  
# Percentages of PTSD cluster-positive individuals ------
  
  insert_msg('Percentages of PTSD cluster positivity')

  conseq$ptsd_clust_plot <- 
    plot_ptsd_freq(conseq$analysis_tbl, 
                   test_data = conseq$test, 
                   split_factor = 'accident_aftermath', 
                   x_lab = '% of consequences strata', 
                   fill_scale = scale_fill_brewer(palette = 'PuRd', 
                                                  labels = conseq$analysis_tbl %>% 
                                                    label_n(accident_aftermath), 
                                                  name = ''), 
                   color_scale = scale_color_brewer(palette = 'PuRd', 
                                                    labels = conseq$analysis_tbl %>% 
                                                      label_n(accident_aftermath), 
                                                    name = ''))
  
# Body part injuries -------
  
  insert_msg('Percentages of body part injuries')

  conseq$body_part_plot <- 
    plot_body_freq(conseq$analysis_tbl, 
                   test_data = conseq$test, 
                   split_factor = 'accident_aftermath', 
                   x_lab = '% of consequences strata', 
                   fill_scale =  scale_fill_brewer(palette = 'PuRd', 
                                                   labels = conseq$analysis_tbl %>% 
                                                     label_n(accident_aftermath), 
                                                   name = ''), 
                   color_scale = scale_color_brewer(palette = 'PuRd', 
                                                    labels = conseq$analysis_tbl %>% 
                                                      label_n(accident_aftermath), 
                                                    name = ''))
  
# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  conseq$result_tbl <- 
    list(conseq$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(conseq$desc_stats))), 
         conseq$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% conseq$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'No somatic consequences', 'Somatic consequences', 
                'Significance', 'Effect size'))
  
# END -----
  
  insert_tail()