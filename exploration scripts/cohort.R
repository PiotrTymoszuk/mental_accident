# Cohort characteristic in a tabular form
# and plots for selected mental health measures

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
                  #'somatic_comorbidity', 
                  'somatic_comorbidity_type', 
                  'traumatic_number', 
                  'prime_trauma_event', 
                  'prime_trauma_event_past', 
                  'psych_comorbidity'), 
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
         plot_title = c('Quality of life, EUROHIS QOL domains', 
                        'PTSD, PCL-5 domains', 
                        'Post-traumatic growth, PTGI factors'), 
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
         plot_subtitle = paste('n =', nrow(ptsd$dataset)), 
         scale = 'width') %>% 
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
  
# Plotting QoL, stack plot -------
  
  insert_msg('Plotting QoL, stack plot')
  
  cohort$qol_details$data <- 
    cohort$mental_details$variables$eurohis %>% 
    set_names(cohort$mental_details$variables$eurohis) %>% 
    map(~count(ptsd$dataset, .data[[.x]])) %>% 
    map(set_names, c('score', 'n')) %>% 
    map(mutate, 
        n_total = sum(n), 
        perc = n/sum(n) * 100) %>% 
    compress(names_to = 'factor') %>% 
    mutate(factor = factor(factor, 
                           rev(cohort$mental_details$variables$eurohis)),
           score = factor(score, 5:1))
  
  cohort$qol_details$plot <- 
    cohort$qol_details$data %>% 
    ggplot(aes(x = perc, 
               y = factor, 
               fill = factor(score))) + 
    geom_bar(position = 'stack', 
             stat = 'identity', 
             color = 'black') + 
    scale_fill_brewer(palette = 'Blues', 
                      direction = -1, 
                      name = 'QoL') + 
    scale_y_discrete(labels = cohort$mental_details$variables$eurohis %>% 
                       rev %>% 
                       exchange(dict = ptsd$var_lexicon) %>% 
                       stri_replace(regex = '\\s{1}score$', 
                                    replacement = '') %>% 
                       stri_extract(regex = '\\w+$')) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Quality of life, EUROHIS QOL domains', 
         subtitle = paste('n = ', cohort$qol_details$data$n_total[[1]]), 
         x = '% of participants')
  
# Plotting percentages of positivity in the PTSD domains ------
  
  insert_msg('Percentages positive in the PTSD domains')
  
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
                   plot_title = 'PCL-5 DSM-5 domains', 
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
  
# Overlap of the DSM domain positivity ---------
  
  insert_msg('Overlap of the DSM domain positivity')
  
  ## variables and analysis data frame
  
  cohort$ptsd_overlap$variables <- 
    c('dsm5_B_class', 
      'dsm5_C_class',
      'dsm5_D_class', 
      'dsm5_E_class')
  
  cohort$ptsd_overlap$data <- ptsd$dataset %>% 
    select(all_of(cohort$ptsd_overlap$variables)) %>% 
    map_dfc(~.x == 'positive') %>% 
    set_names(c('B', 'C', 'D', 'E'))

  ## Venn plot
  
  cohort$ptsd_overlap$venn_plot <- cohort$ptsd_overlap$data %>% 
    ggvenn(fill_color = c('coral1', 'steelblue1', 'plum4', 'darkolivegreen4'), 
           set_name_size = 2.75, 
           text_size = 2.75) + 
    labs(title = 'Overlap between PCL-5 PTSD domain symptoms', 
         subtitle = '% of participants positive for at least one domain') + 
    theme(plot.title = element_text(size = 8, face = 'bold'), 
          plot.subtitle = globals$common_text)
  
  ## Upset plot, based on an example from:
  ## https://github.com/krassowski/complex-upset/issues/133
  
  cohort$ptsd_overlap$upset_plot <- 
    upset(cohort$ptsd_overlap$data, 
          intersect = c('B', 'C', 'D', 'E'), 
          name = 'Exclusive overlap between the PCL-5 domains', 
          mode = 'exclusive_intersection', 
          keep_empty_groups = FALSE, 
          min_degree = 1, 
          wrap = TRUE, 
          width_ratio = 1/3, 
          sort_intersections_by = 'degree', 
          sort_intersections = 'ascending', 
          stripes = upset_stripes(colors = NA), 
          encode_sets = FALSE, 
          themes = upset_modify_themes(list('intersections_matrix' = theme(axis.title = globals$common_text))),  
          base_annotations = list(
            'Intersection size' = intersection_size(text_mapping = aes(label = signif(!!get_size_mode('exclusive_intersection')/nrow(ptsd$dataset) * 100, 2)), 
                                                    fill = 'darkolivegreen4', 
                                                    color = 'black', 
                                                    text = list(size = 2.75)) + 
              ylab('Overlap, % of cohort') + 
              scale_y_continuous(labels = scales::percent_format(scale = 100/nrow(ptsd$dataset), 
                                                                 suffix = ''), 
                                 breaks = seq(0, 3.5, by = 0.5)/100 * 
                                   nrow(ptsd$dataset))  +
              globals$common_theme + 
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(), 
                    axis.line.x = element_blank(), 
                    axis.ticks.x = element_blank())), 
          set_sizes = upset_set_size(filter_intersections = TRUE, 
                                     position = 'right', 
                                     geom = geom_bar(aes(y = after_stat(count/nrow(ptsd$dataset) * 100)), 
                                                     stat = 'count', 
                                                     fill = 'coral3', 
                                                     color = 'black'))  + 
            geom_text(aes(y = after_stat(count/nrow(ptsd$dataset) * 100 - 1), 
                          label = after_stat(signif(count/nrow(ptsd$dataset) * 100, 2))), 
                      stat = 'count', 
                      color = 'white', 
                      size = 2.75) + 
            ylab('Domain-positive, % of cohort') + 
            globals$common_theme + 
            theme(axis.title.y = element_blank(), 
                  axis.text.y = element_blank(), 
                  axis.line.y = element_blank(), 
                  axis.ticks.y = element_blank())) + 
    labs(title = 'PTSD symptoms, PCL-5 domains', 
         subtitle = paste('n =', nrow(ptsd$dataset))) + 
    theme(plot.title = element_text(size = 8, face = 'bold'), 
          plot.subtitle = globals$common_text)
  
# END -----
  
  insert_tail()
