# Forest plots of point estimates of symptoms of mental disorders in the cohort
# and publications.
# Population estimates of anxiety and depression: paper by Hummer and colleagues, 
# because they use the same toolset.

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
      darvez_bornos_2008_ptsd = 'steelblue', 
      microcensus = 'steelblue', 
      mikutta_2022 = 'steelblue', 
      leonard_2021 = 'steelblue', 
      chernova_2021 = 'steelblue', 
      leppert_2008 = 'steelblue', 
      humer_2022 = 'steelblue')
  
  ## studies with alpine accidents and with general population
  
  lit_plots$studies$alpine <- c('cohort', 'mikutta_2022', 'leonard_2021')
  
  lit_plots$studies$general <- 
    names(lit_data$data)[!names(lit_data$data) %in% lit_plots$studies$alpine] %>% 
    c('cohort', .)
  
  lit_plots$studies <- lit_plots$studies[c("general", "alpine")]

# Estimates of depression and anxiety symptoms, Humer 2022 -------
  
  insert_msg('Depression and anxiety, German monitoring')
  
  ## point estimates
  
  lit_plots$depr_anx_stats <- lit_data$stats %>% 
    filter(variable %in% c('gad7_total_class', 
                           'phq9_total_class'), 
           level == 'positive') %>% 
    mutate(plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'), 
           variable = car::recode(variable, 
                                  "'phq9_total_class' = 'PHQ-9'; 
                                  'phq2_total_class' = 'PHQ-2'; 
                                  'gad7_total_class' = 'GAD-7'; 
                                  'gad2_total_class' = 'GAD-2'"), 
           disorder = ifelse(variable == 'GAD-7', 
                             'anxiety', 'depression'), 
           disorder = factor(disorder, c('anxiety', 'depression')))
  
  ## Forest plots: separate plots for depression and anxiety
  
  lit_plots$depr_anx_plot <- lit_plots$depr_anx_stats %>% 
    ggplot(aes(x = percent, 
               y = source, 
               color = source)) + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0,
                   position = position_dodge(0.9)) + 
    geom_point(shape = 16, 
               size = 2, 
               position = position_dodge(0.9)) + 
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.2,
              vjust = -1.2, 
              position = position_dodge(0.9)) + 
    facet_grid(disorder ~ ., 
               labeller = as_labeller(c(anxiety = 'Anxiety, GAD-7 \u2265 10', 
                                        depression = 'Depression, PHQ-9 \u2265 10'))) + 
    scale_y_discrete(labels = c(cohort = 'own cohort', 
                                humer_2022 = 'Austrian estimates\nHumer 2022')) + 
    scale_color_manual(values = lit_plots$dataset_colors, 
                       labels = c(cohort = 'own cohort', 
                                  humer_2022 = 'Austrian estimates\nHumer 2022'), 
                       name = '') + 
    guides(color = 'none') +
    globals$common_theme + 
    theme(legend.title = element_blank(), 
          axis.title.y = element_blank()) + 
    labs(title = 'Frequency of anxious and depressive symptoms', 
         x = 'estimated % of sample, 95% CI')
  
# Traumatic events: stats -------
  
  insert_msg('Stats for traumatic events')
  
  lit_plots$trauma_stats <- lit_data$stats %>% 
    filter(variable == 'traumatic_event', 
           level == 'yes') %>% 
    mutate(plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'))

# Traumatic events: Forest plots -------
  
  insert_msg('Traumatic events: Forest plots')
  
  ## Forest plots for comparison with the general population
  ## and for comparison with mountain rescuers and avalanche victims
  
  lit_plots$trauma_plots <- lit_plots$studies %>% 
    map(~filter(lit_plots$trauma_stats, 
                source %in% .x)) %>% 
    list(x = ., 
         y = paste('Traumatic events,', 
                   c('population estimates', 
                     'alpine sport/accidents')), 
         z = list(c('cohort' = 'own cohort\nDIA-X', 
                    'koenen_2017' = 'Koenen 2017\nCIDI', 
                    'kessler_2017' = 'Kessler 2017\nCIDI', 
                    'darvez_bornos_2008_trauma' = 'Darves-Bornos 2008\nCIDI', 
                    'hauffa_2011' = 'Hauffa 2011\nCIDI', 
                    'kilpatrick_2013' = 'Kilpatrick 2013\nDSM-5 criteria'), 
                  c('cohort' = 'own cohort\nDIA-X', 
                    'mikutta_2022' = 'Mikutta 2022\nmountain-specific\nchecklist'))) %>% 
    pmap(function(x, y, z) x %>% 
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
           scale_y_discrete(labels = z) + 
           scale_x_continuous(limits = c(0, 100)) + 
           guides(color = 'none') + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = 'estimated % of sample, 95% CI'))
  
# Prevalence of manifest PTSD: stats -------
  
  insert_msg('PTSD prevalence, stats')
  
  lit_plots$ptsd_stats <- lit_data$stats %>% 
    filter(stri_detect(variable, fixed = 'ptsd') | 
             variable == 'dsm5_all_class') %>% 
    filter(level == 'positive') %>% 
    mutate(plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'))
  
# Prevalence of manifest PTSD: Forest plots --------
  
  insert_msg('Manifest PTSD: Forest plots')

  ## separate Forest plots for the population and alpine accident estimates
  
  lit_plots$ptsd_plots <- lit_plots$studies %>% 
    map(~filter(lit_plots$ptsd_stats, 
                source %in% .x)) %>% 
    list(x = ., 
         y = paste('Manifest PTSD,', 
                   c('population estimates', 
                     'alpine sport/accidents')), 
         z = list(c('cohort' = 'own cohort\nall PCL-5 domains+', 
                    'kilpatrick_2013' = 'Kilpatrick 2013\nDSM-5 criteria', 
                    'koenen_2017' = 'Koenen 2017\nCIDI', 
                    'darvez_bornos_2008_ptsd' = 'Darves-Bornoz 2008\nCIDI', 
                    'hauffa_2011' = 'Hauffa 2011\nPDS'), 
                  c('cohort' = 'own cohort\nall PCL-5 domains+', 
                    'mikutta_2022' = 'Mikutta 2022\nall PCL-5 domains+', 
                    'leonard_2021' = 'L\u00E9onard 2021\nIES-R'))) %>% 
    pmap(function(x, y, z) x %>% 
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
           scale_y_discrete(labels = z) + 
           guides(color = 'none') + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = 'estimated % of sample, 95% CI'))
  
# Depressive symptoms in the cohort and Austrian population, PHQ-8 ------
  
  insert_msg('Depressive symptoms in the cohort and Austria')
  
  ## stats to be plotted
  
  lit_plots$aut_stats <- 
    list(cohort = lit_data$stats, 
         desurv = lit_aut$stats) %>% 
    map_dfr(filter, 
            variable == 'phq8_total_class', 
            level != 'none') %>% 
    mutate(plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'), 
           level = factor(level, 
                          rev(c('none', 
                                'mild', 
                                'moderate', 
                                'moderately severe', 
                                'severe'))))
  
  ## Forest plot
  
  lit_plots$aut_plot <- lit_plots$aut_stats %>% 
    ggplot(aes(x = percent, 
               y = level, 
               color = source)) + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0, 
                   position = position_dodge(0.9)) + 
    geom_point(shape = 16, 
               size = 2, 
               position = position_dodge(0.9)) + 
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.2,
              vjust = -1.2, 
              position = position_dodge(0.9)) + 
    scale_color_manual(values = lit_plots$dataset_colors, 
                       labels = c(cohort = 'own cohort', 
                                  microcensus = 'Austrian\nmicrocensus 2019'), 
                       name = '') + 
    scale_y_discrete(labels = c('none' = 'none', 
                                'mild' = 'mild\nPHQ-8 5 - 9', 
                                'moderate' = 'moderate\nPHQ-8 10 - 14', 
                                'moderately severe' = 'moderately severe\nPHQ-8 15 - 20', 
                                'severe' = 'severe\nPHQ-8 21+')) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Frequency of depressive symptoms', 
         x = 'estimated % of sample, 95% CI')
  
# Plots for resilience estimates --------
  
  insert_msg('Plots for resilience estimates')
  
  ## stats, indicating studies with the mountain sport context
  
  lit_plots$rs_stats <- lit_data$rs_stats %>% 
    mutate(context = ifelse(source %in% lit_plots$studies$alpine, 
                            'alpine', 'general'), 
           plot_lab = paste(signif(mean, 2), signif(sd, 2), 
                            sep = ', SD: '), 
           axis_lab = car::recode(source, 
                                  "'cohort' = 'own cohort'; 
                                  'chernova_2021' = 'Chernova 2021'; 
                                  'leppert_2008' = 'Leppert 2008'; 
                                  'mikutta_2022' = 'Mikutta 2022'"), 
           axis_lab = paste(axis_lab, n_total, sep = '\nn = '))
  
  ## Forest plot: mean with SD
  
  lit_plots$rs_plot <- lit_plots$rs_stats %>% 
    ggplot(aes(x = mean, 
               y = reorder(axis_lab, mean), 
               color = source)) + 
    geom_errorbarh(aes(xmin = mean - sd, 
                       xmax = mean + sd), 
                   height = 0) + 
    geom_point(shape = 16, 
               size = 2) + 
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.5, 
              vjust = -1.2) +
    facet_grid(context ~ ., 
               scales = 'free', 
               space = 'free') + 
    scale_color_manual(values = lit_plots$dataset_colors) + 
    guides(color = 'none') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Resilience, RS13', 
         x = 'mean RS13 score, SD')
  
# PCL-5 domains in the cohort and in the report by Mikutta et al. --------
  
  insert_msg('Frequency of PTSD B, C, D and E symptoms')
  
  ## plotting table
  
  lit_plots$ptsd_symptom_stats <- lit_data$stats %>% 
    filter(source %in%  c('cohort', 'mikutta_2022'), 
           level == 'positive') %>% 
    filter(variable %in% c('dsm5_all_class', 
                           'dsm5_cluster_class', 
                           'dsm5_B_class', 
                           'dsm5_C_class', 
                           'dsm5_D_class', 
                           'dsm5_E_class')) %>% 
    mutate(plot_lab = paste0(signif(percent, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'), 
           var_lab = car::recode(variable, 
                                 "'dsm5_all_class' = 'all domains+'; 
                                 'dsm5_cluster_class' = 'at least one domain+'; 
                                 'dsm5_B_class' = 'domain B+'; 
                                 'dsm5_C_class' = 'domain C+'; 
                                 'dsm5_D_class' = 'domain D+'; 
                                 'dsm5_E_class' = 'domain E+'"),
           facet = ifelse(variable %in% c('dsm5_all_class', 'dsm5_cluster_class'), 
                          'symptoms', 'domains'), 
           facet = factor(facet, c('symptoms', 'domains')))
  
  ## Forest plots
  
  lit_plots$ptsd_symptom_plot <- lit_plots$ptsd_symptom_stats %>% 
    ggplot(aes(x = percent, 
               y = var_lab, 
               color = source)) +
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0, 
                   position = position_dodge(0.9)) + 
    geom_point(shape = 16, 
               size = 2, 
               position = position_dodge(0.9)) + 
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.3,
              vjust = -1.2, 
              position = position_dodge(0.9)) + 
    facet_grid(facet ~ ., 
               scales = 'free', 
               space = 'free') +
    scale_color_manual(values = lit_plots$dataset_colors, 
                       labels = c(cohort = paste('own cohort\nn =', 
                                                 nrow(lit_data$data$cohort)), 
                                  mikutta_2022 = paste('Mikutta 2022\nn =', 
                                                       nrow(lit_data$data$mikutta_2022))), 
                       name = '') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          strip.background = element_blank(), 
          strip.text = element_blank()) + 
    labs(title = 'PTSD, alpine sport/accidents', 
         x = '% of cohort, 95% CI')
  
# END ------
  
  insert_tail()