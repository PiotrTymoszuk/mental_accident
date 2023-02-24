# Effects of injury severity (AIS strata)

  insert_head()
  
# container ------
  
  sever <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  sever$variables <- fct_globals$variables$variable
  
  sever$variables <- sever$variables[sever$variables != 'injury_sev_strata']
  
  ## analysis table
  
  sever$analysis_tbl <- ptsd$dataset %>% 
    select(ID, injury_sev_strata, 
           all_of(sever$variables)) %>% 
    filter(!is.na(injury_sev_strata)) %>% 
    mutate(injury_sev_strata = paste('AIS', as.character(injury_sev_strata)), 
           injury_sev_strata = factor(injury_sev_strata))

  ## statistical test type
  
  sever$test_type <- 
    sever$analysis_tbl[sever$variables] %>% 
    map_lgl(is.numeric)
  
  sever$test_type <- ifelse(sever$test_type, 
                            'kruskal_eta', 'cramer_v')

# Descriptive stats ------

  insert_msg('Descriptive statistics')
  
  sever$desc_stats <- sever$analysis_tbl %>% 
    explore(variables = sever$variables, 
            split_factor = 'injury_sev_strata', 
            what = 'table', 
            pub_styled = TRUE)
  
# Serial testing for differences between the severity strata ------
  
  insert_msg('Testing for differences between the injury severity strata')
  
  sever$test <- sever$analysis_tbl %>%
    compare_variables(variables = sever$variables, 
                      split_factor = 'injury_sev_strata', 
                      what = 'eff_size', 
                      types = sever$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      .parallel = TRUE, 
                      .paropts = furrr_options(seed = TRUE, 
                                               globals = c('sever'))) %>% 
    format_fct_test
  
# Significant differences -------
  
  insert_msg('Significant differences')
  
  sever$top_factors <- sever$test %>% 
    filter(p_value < 0.05) %>% 
    .$variable
  
# Plots for single variables -------
  
  insert_msg('Plots for single variables')
  
  sever$plots <- 
    plot_fct(data = sever$analysis_tbl, 
             test_data = sever$test, 
             factor = 'injury_sev_strata', 
             fill_scale = scale_fill_brewer(palette = 'Reds'))
  
# Plot panels for QoL, PTSD and PTG ------
  
  insert_msg('Plot panels for QoL, PTG and PTSD')
  
  sever$panels <- 
    plot_fct_panels(data = sever$analysis_tbl, 
                    test_data = sever$test, 
                    factor = 'injury_sev_strata', 
                    fill_scale = scale_fill_brewer(palette = 'Reds'))
  
# PTSD clusters ---------
  
  insert_msg('PTSD clusters')
  
  sever$ptsd_clust_plot <- 
    plot_ptsd_freq(sever$analysis_tbl, 
                   test_data = sever$test, 
                   split_factor = 'injury_sev_strata', 
                   x_lab = '% of AIS strata', 
                   fill_scale = scale_fill_brewer(palette = 'Reds', 
                                                  labels = sever$analysis_tbl %>% 
                                                    label_n(injury_sev_strata, 
                                                            sep = ': n ='), 
                                                  name = ''), 
                   color_scale = scale_color_brewer(palette = 'Reds', 
                                                    labels = sever$analysis_tbl %>% 
                                                      label_n(injury_sev_strata, 
                                                              sep = ': n ='), 
                                                    name = ''))
  
# Body part injuries -------
  
  insert_msg('Percentages of body part injuries')
  
  sever$body_part_plot <- 
    plot_body_freq(sever$analysis_tbl, 
                   test_data = sever$test, 
                   split_factor = 'injury_sev_strata', 
                   x_lab = '% of AIS strata', 
                   fill_scale = scale_fill_brewer(palette = 'Reds', 
                                                  labels = sever$analysis_tbl %>% 
                                                    label_n(injury_sev_strata, 
                                                            sep = ': n ='), 
                                                  name = ''),
                   color_scale = scale_color_brewer(palette = 'Reds', 
                                                    labels = sever$analysis_tbl %>% 
                                                      label_n(injury_sev_strata, 
                                                              sep = ': n ='), 
                                                    name = ''))

# Ready-to-use result table -----
  
  insert_msg('Ready-to-use result table')
  
  sever$result_tbl <- 
    list(sever$desc_stats %>% 
           reduce(left_join, by = 'variable') %>% 
           set_names(c('variable', names(sever$desc_stats))), 
         sever$test[c('variable', 'significance', 'eff_size')]) %>% 
    map(filter, variable %in% sever$top_factors) %>% 
    reduce(left_join, by = 'variable') %>% 
    format_summ_tbl %>% 
    set_names(c('Variable', 'AIS 1', 'AIS 2', 
                'AIS 3+', 'significance', 'Effect size'))
  
# END -----
  
  insert_tail()