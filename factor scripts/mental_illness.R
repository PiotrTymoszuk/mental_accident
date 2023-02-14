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
  
  ## statistical test type
  
  mental$test_type <- 
    ptsd$dataset[mental$variables] %>% 
    map_lgl(is.numeric)
  
  mental$test_type <- ifelse(mental$test_type, 
                             'wilcoxon_r', 'cramer_v')

# Descriptive statistic ------
  
  insert_msg('Descriptive statistics')
  
  mental$desc_stats <- ptsd$dataset %>% 
    explore(variables = mental$variables, 
            split_factor = 'psych_comorbidity', 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for differences between the mental illness strata ------
  
  insert_msg('Testing for differences between the mental illness strata')
  
  mental$test <- ptsd$dataset %>%
    compare_variables(variables = mental$variables, 
                      split_factor = 'psych_comorbidity', 
                      what = 'eff_size', 
                      types = mental$test_type, 
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
  
  mental$top_factors <- mental$test %>% 
    filter(p_adjusted < 0.1) %>% 
    .$variable
  
# Single variable plots ------
  
  insert_msg('Single variable plots')
  
  mental$plots <- 
    plot_fct(data = ptsd$dataset, 
             test_data = mental$test, 
             factor = 'psych_comorbidity', 
             fill_scale = scale_fill_brewer(palette = 'Greys'))
  
# Specific plots: PTSD, PTG and QoL scores ------
  
  insert_msg('Plot panels')
  
  mental$panels <- 
    plot_fct_panels(data = ptsd$dataset, 
                    test_data = mental$test, 
                    factor = 'psych_comorbidity', 
                    fill_scale = scale_fill_brewer(palette = 'Greys'))
  
# Percentages of PTSD cluster-positive individuals ------
  
  insert_msg('Percentages of PTSD cluster positivity')
  
  ## variables and frequencies
  
  mental$ptsd_cluster$variables <- 
    c('dsm5_B_class', 'dsm5_C_class', 'dsm5_D_class', 'dsm5_E_class')
  
  mental$ptsd_cluster$frequency <- ptsd$dataset %>% 
    dlply('psych_comorbidity', 
          select, 
          all_of(mental$ptsd_cluster$variables)) %>% 
    map(count_binary) %>% 
    compress(names_to = 'psych_comorbidity')
  
  ## labels with effect sizes and significance
  
  mental$ptsd_cluster$ax_labs <- mental$test %>% 
    filter(variable %in% mental$ptsd_cluster$variables) %>% 
    mutate(ax_lab = variable %>% 
             stri_extract(regex = 'B|C|D|E') %>% 
             paste(., plot_cap, sep = '\n'))
  
  mental$ptsd_cluster$ax_labs <- 
    set_names(mental$ptsd_cluster$ax_labs$ax_lab, 
              mental$ptsd_cluster$ax_labs$variable)

  ## plot
  
  mental$ptsd_cluster$plot <- 
    plot_freq_bars(data = mental$ptsd_cluster$frequency, 
                   freq_var = 'percent', 
                   cat_var = 'variable', 
                   split_factor = 'psych_comorbidity', 
                   plot_title = 'PCL-5 DSM-5 clusters', 
                   x_lab = '% of mental illness strata', 
                   fill_scale = scale_fill_brewer(palette = 'Greys', 
                                                  labels = ptsd$dataset %>% 
                                                    label_n(psych_comorbidity), 
                                                  name = 'Mental illness'), 
                   show_freqs = TRUE, 
                   position = position_dodge(0.9), 
                   size = 2.5, 
                   hjust = -0.4) + 
    scale_y_discrete(limits = rev(mental$ptsd_cluster$variables), 
                     labels = mental$ptsd_cluster$ax_labs)
  
  
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
                'Significance (FDR)', 'Effect size'))
  
# END -----
  
  insert_tail()