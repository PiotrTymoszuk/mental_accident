# Forest plots of point estimates of symptoms of mental disorders in the cohort
# and publications or the German mental health surveillance system

  insert_head()
  
# container -------
  
  lit_plots <- list()
  
# analysis globals: labels, lexicons and colors ------
  
  insert_msg('Analysis globals')
  
  ## data set colors and labels
  
  lit_plots$dataset_colors <- 
    c(cohort = 'coral3', 
      cohort_symptoms = 'coral3', 
      desurv = 'steelblue', 
      koenen_2017 = 'steelblue', 
      darvez_bornos_2008_trauma = 'steelblue', 
      hauffa_2011 = 'steelblue', 
      kessler_2017 = 'steelblue', 
      kilpatrick_2013 = 'steelblue', 
      darvez_bornos_2008_ptsd = 'steelblue')
  
# Estimates of depression and anxiety symptoms -------
  
  insert_msg('Depression and anxiety')
  
  ## point estimates
  
  lit_plots$depr_anx_stats <- 
    list(cohort = lit_data$stats %>% 
           filter(variable %in% c('gad7_total_class', 'phq9_total_class'), 
                  level == 'positive') %>% 
           mutate(disorder = ifelse(stri_detect(variable, fixed = 'gad'), 
                                    'anxiety', 'depression')), 
         desurv = lit_desurv$stats %>% 
           mutate(disorder = c('anxiety', 'depression'))) %>% 
    map(select, disorder, variable, percent, lower_ci, upper_ci) %>% 
    compress(names_to = 'source') %>% 
    mutate(plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'))
  
  ## Forest plots
  
  lit_plots$depr_anx_plot <- lit_plots$depr_anx_stats %>% 
    ggplot(aes(x = percent, 
               y = source, 
               color = source)) + 
    facet_grid(disorder ~ ., 
               scales = 'free', 
               space = 'free') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0) + 
    geom_point(shape = 16, 
               size = 2) + 
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.5,
              vjust = -1.2) + 
    scale_color_manual(values = lit_plots$dataset_colors, 
                       labels = lit_plots$dataset_labels) + 
    scale_y_discrete(labels = c(cohort = 'own cohort\nGAD-7 and PHQ-9', 
                                desurv = 'German mental\nhealth monitoring\nGAD-2 and PHQ-2')) + 
    guides(color = 'none') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Frequency of depression and anxiety', 
         x = 'estimated % of sample, 95% CI')
  
# Traumatic events -------
  
  insert_msg('Traumatic events')
  
  ## common stats
  
  lit_plots$trauma_stats <- lit_data$stats %>% 
    filter(variable == 'traumatic_event', 
           level == 'yes') %>% 
    mutate(plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'))
  
  # Forest plots
  
  lit_plots$trauma_plots <- lit_plots$trauma_stats %>% 
    ggplot(aes(x = percent, 
               y = reorder(source, percent), 
               color = source)) + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0) + 
    geom_point(shape = 16,
               size = 2) + 
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.5,
              vjust = -1.2) + 
    scale_color_manual(values = lit_plots$dataset_colors) + 
    scale_y_discrete(labels = c('cohort' = 'own cohort\nDIA-X', 
                                'koenen_2017' = 'Koenen 2017\nCIDI', 
                                'kessler_2017' = 'Kessler 2017\nCIDI', 
                                'darvez_bornos_2008_trauma' = 'Darvez-Bornos 2008\nCIDI', 
                                'hauffa_2011' = 'Hauffa 2011\nCIDI', 
                                'kilpatrick_2013' = 'Kilpatrick 2013\nDSM-5 criteria')) + 
    scale_x_continuous(limits = c(0, 100)) + 
    guides(color = 'none') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Lifetime frequency of traumatic events', 
         x = 'estimated % of sample, 95% CI')
  
# Point prevalence of PTSD --------
  
  insert_msg('PTSD')
  
  ## common stats
  
  lit_plots$ptsd_stats <- lit_data$stats %>% 
    filter(stri_detect(variable, fixed = 'ptsd') | 
             variable %in% c('dsm5_cluster_class', 'dsm5_all_class')) %>% 
    filter(level == 'positive') %>% 
    mutate(source = ifelse(variable == 'dsm5_cluster_class', 
                           'cohort_symptoms', source), 
           plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'))
  
  ## Forest plots
  
  lit_plots$ptsd_plot <- lit_plots$ptsd_stats %>% 
    ggplot(aes(x = percent, 
               y = reorder(source, percent), 
               color = source)) + 
    geom_vline(xintercept = 0,
               linetype = 'dashed') +
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0) + 
    geom_point(shape = 16, 
               size = 2) + 
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.5, 
              vjust = -1.2) +
    scale_color_manual(values = lit_plots$dataset_colors) + 
    scale_y_discrete(labels = c('cohort_symptoms' = 'own cohort\nat least PCL-5 domain+', 
                                'cohort' = 'own cohort\nall PCL-5 domains+', 
                                'kilpatrick_2013' = 'Kilpatrick 2013\nDSM-5 criteria', 
                                'koenen_2017' = 'Koenen 2017\nCIDI', 
                                'darvez_bornos_2008_ptsd' = 'Darvez-Bornoz 2008\nCIDI', 
                                'hauffa_2011' = 'Hauffa 2011\nPDS')) + 
    guides(color = 'none') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Frequency of PTSD', 
         x = 'estimated % of sample, 95% CI')
  
# END ------
  
  insert_tail()