# Cohort characteristic in a tabular form
# 

  insert_head()
  
# container list ------
  
  cohort <- list()

# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  cohort$desc_stats <- ptsd$dataset %>% 
    explore(variables = eda_globals$variables$variable, 
            what = 'table', 
            pub_styled = TRUE)
  
# ready to use tables: demographic, accident and mental health features -----
 
  insert_msg('Ready-to-use tables')
  
  ## variable categories 
  
  cohort$var_class <- 
    list(demo = c('obs_time', 
                  'age', 
                  'age_class', 
                  'sex', 
                  'residence_alpine_region', 
                  'education', 
                  'employment_status', 
                  'sport_profession', 
                  'trauma_risk_profession', 
                  'household_income_class', 
                  'smoking_status', 
                  'cage_total_class', 
                  'somatic_comorbidity', 
                  'psych_comorbidity', 
                  'traumatic_event', 
                  'prime_trauma_event', 
                  'prime_trauma_event_past'), 
         accident = c('prior_accident', 
                      'sport_type', 
                      'accident_alone', 
                      'accident_culprit', 
                      'accident_injured_persons', 
                      'accident_rescue', 
                      'injury_sev_strata', 
                      'hospitalization', 
                      'surgery_done', 
                      'psych_support_post_accident', 
                      'psych_support_need', 
                      'accident_aftermath', 
                      'same_sport_type_post_accident', 
                      'caution_post_accident', 
                      'flashback_frequency'), 
         mental = c(## gneral measures of mental health and QoL
                    'gad7_total', 
                    'gad7_total_class', 
                    'phq9_total', 
                    'phq9_total_class', 
                    'phq_events_total', 
                    'phq_events_total_class', 
                    'eurohis_total', 
                    'soc9l_total', 
                    'rs13_total', 
                    'rs13_total_class', 
                    ## accident-specific measures
                    'ptgi_total', 
                    'dsm5_total', 
                    'dsm5_cluster_class'))
  
  ## splitting the main table
  
  cohort$result_tbl <- cohort$var_class %>% 
    map(~mutate(cohort$desc_stats, 
                variable = factor(variable, .x)) %>% 
          filter(!is.na(variable))) %>% 
    map(arrange, variable) %>% 
    map(format_summ_tbl) %>% 
    map(mutate, 
        variable = stri_capitalize(variable)) %>% 
    map(set_names, c('Variable', 'Statistic'))
  
  ## the demographic and mental health tables contain
  ## only the complete  sets of information
  
  cohort$result_tbl[c("demo", "mental")] <- 
    cohort$result_tbl[c("demo", "mental")] %>% 
    map(~rbind(tibble(Variable = 'Participants, n', 
                      Statistic = nrow(ptsd$dataset)), 
               .x)) %>% 
    map(~map_dfc(.x, 
                 stri_replace, 
                 regex = '\\nn\\s{1}=\\s{1}\\d+$', 
                 replacement = ''))
  
# Plotting the injury distribution -------  
  
  insert_msg('Plotting injury distribution')
  
  ## counting injuries by the body part
  
  cohort$injury$variables <- 
    c('injury_head', 
      'injury_face', 
      'injury_neck', 
      'injury_chest', 
      'injury_abdomen', 
      'injury_spine', 
      'injury_upper_limbs', 
      'injury_lower_limbs', 
      'injury_external_other')
  
  cohort$injury$frequency <- 
    ptsd$dataset[cohort$injury$variables] %>% 
    count_binary
  
  ## plotting
  
  cohort$injury$plot <- 
    plot_freq_bars(data = cohort$injury$frequency, 
                   freq_var = 'percent', 
                   cat_var = 'variable', 
                   plot_title = 'Injured body regions', 
                   plot_subtitle = paste('n =', 
                                         cohort$injury$frequency$n_total[[1]]), 
                   x_lab = '% of cohort', 
                   bar_color = 'steelblue4', 
                   show_freqs = TRUE, 
                   size = 2.5, 
                   color = 'steelblue4', 
                   hjust = -0.4) + 
    scale_y_discrete(limits = rev(cohort$injury$variables), 
                    labels = exchange(rev(cohort$injury$variables), 
                                      dict = ptsd$var_lexicon, 
                                      key = 'variable',
                                      value = 'label') %>% 
                      stri_replace(regex = '\\s{1}injury$', 
                                   replacement = ''))

# Plotting mental health features -------
  
  insert_msg('Plotting mental health features')
  
  ## with radar plots for the EUROHIS, PCL DSM5 and PTGI scales
  
  ## plotting variables
  
  cohort$mental_details$variables <- 
    list(eurohis = c('eurohis_qol', 
                     'eurohis_health', 
                     'eurohis_energy', 
                     'eurohis_finances', 
                     'eurohis_activity', 
                     'eurohis_selfesteem', 
                     'eurohis_relationship', 
                     'eurohis_housing'), 
         dsm = c('dsm5_B', 
                 'dsm5_C', 
                 'dsm5_D', 
                 'dsm5_E'), 
         ptgi = c('ptgi_fctI', 
                  'ptgi_fctII', 
                  'ptgi_fctIII', 
                  'ptgi_fctIV', 
                  'ptgi_fctV'))
  
  ## plots
  
  cohort$mental_details$plots <- 
    list(variables = cohort$mental_details$variables, 
         plot_title = c('EUROHIS QOL domains', 
                        'PCL-5 DSM-5 clusters', 
                        'PTGI factors'), 
         fill = c('darkolivegreen3', 
                  'coral3', 
                  'steelblue')) %>% 
    pmap(draw_violin_panel, 
         data = ptsd$dataset, 
         distr_geom = 'violin', 
         point_alpha = 0.25, 
         point_hjitter = 0.1, 
         point_wjitter = 0.05, 
         point_size = 1, 
         cust_theme = globals$common_theme, 
         x_lab = 'Score', 
         plot_subtitle = paste('n =', nrow(ptsd$dataset))) %>% 
    map2(., cohort$mental_details$variables, 
         ~.x + 
           scale_y_discrete(limits = rev(.y), 
                            labels = exchange(rev(.y), 
                                              dict = ptsd$var_lexicon, 
                                              key = 'variable', 
                                              label = 'label') %>% 
                              stri_replace(regex = '\\s{1}score$', 
                                           replacement = '') %>% 
                              stri_extract(regex = '\\w+$')))
  
# Plotting percentages of positivity in the PTSD clusters ------
  
  insert_msg('Percentages positive in the PTSD clusters')
  
  ## variables 
  
  cohort$ptsd_cluster$variables <- 
    c('dsm5_B_class', 
      'dsm5_C_class',
      'dsm5_D_class', 
      'dsm5_E_class')
  
  ## frequencies
  
  cohort$ptsd_cluster$frequency <- 
    ptsd$dataset[cohort$ptsd_cluster$variables] %>% 
    count_binary
  
  ## plot
  
  cohort$ptsd_cluster$plot <- 
    plot_freq_bars(data = cohort$ptsd_cluster$frequency, 
                   freq_var = 'percent', 
                   cat_var = 'variable', 
                   plot_title = 'PCL-5 DSM-5 clusters', 
                   x_lab = '% of cohort', 
                   bar_color = 'coral3', 
                   show_freqs = TRUE, 
                   size = 2.5, 
                   color = 'coral3', 
                   hjust = -0.4, 
                   plot_subtitle = paste('n =', 
                                         cohort$ptsd_cluster$frequency$n_total[[1]])) + 
    scale_y_discrete(limits = rev(cohort$ptsd_cluster$variables), 
                     labels = function(x) stri_extract(x, regex = 'B|C|D|E'))
  
# END -----
  
  insert_tail()
