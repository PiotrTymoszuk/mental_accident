# Report figures 

  insert_head()
  
# container list ------
  
  figures <- list()
  
# Figure 1: flow diagram --------
  
  insert_msg('Figure 1: flow diagram')
  
  figures$consort <- 
    plot_grid(ggdraw() + 
                draw_image('./aux files/consort.png')) %>% 
    as_figure(label = 'figure_1_flow_diagram', 
              ref_name = 'consort', 
              caption = paste('Flow diagram of the analysis', 
                              'inclusion process.'), 
              w = 120, 
              h = 3966/2856 * 120)
  
# Figure 2: psychometry scores in the clusters -------
  
  insert_msg('Figure 2: psychometry scoring in the clusters')
  
  ## upper panel: stress, anxiety, depression, somatization, panic
  ## resilience and sense-of coherence
  
  figures$clust_psych$upper <- feat_clust$ribbon_plots
  
  for(i in names(figures$clust_psych$upper)) {
    
    figures$clust_psych$upper[[i]]$data <- 
      figures$clust_psych$upper[[i]]$data %>% 
      filter(variable %in% c('gad7_total', 
                             'phq9_total', 
                             'phq_events_total', 
                             'phqd_panic_total', 
                             'rs13_total', 
                             'soc9l_total'))
    
  }
  
  figures$clust_psych$upper <- figures$clust_psych$upper %>% 
    map2(., 
         paste0('Major mental health readouts, ', 
                c('training', 'test')), 
         ~.x + 
           scale_y_discrete(limits = rev(c('gad7_total', 
                                           'phq9_total', 
                                           'phq_events_total', 
                                           'phqd_panic_total', 
                                           'soc9l_total', 
                                           'rs13_total')), 
                            labels = psych_labeller) + 
           labs(title = .y))
  
  ## second panel
  
  figures$clust_psych$qol <- feat_clust$ribbon_plots
  
  for(i in names(figures$clust_psych$qol)) {
    
    figures$clust_psych$qol[[i]]$data <- 
      figures$clust_psych$qol[[i]]$data %>% 
      filter(stri_detect(variable, fixed = 'eurohis'))
    
  }
  
  figures$clust_psych$qol <- figures$clust_psych$qol %>% 
    map2(., 
         paste0('Quality of life, EUROHIS-QOL 8, ', 
                c('training', 'test')), 
         ~.x + 
           scale_y_discrete(limits = rev(c('eurohis_total', 
                                           'eurohis_qol', 
                                           'eurohis_selfesteem', 
                                           'eurohis_energy', 
                                           'eurohis_activity', 
                                           'eurohis_health', 
                                           'eurohis_relationship', 
                                           'eurohis_finances', 
                                           'eurohis_housing')), 
                            labels = function(x) exchange(x, 
                                                          dict = ptsd$var_lexicon, 
                                                          key = 'variable', 
                                                          value = 'label') %>% 
                              stri_replace(regex = '\\s{1}score$', 
                                           replacement = '') %>% 
                              stri_replace(fixed = 'EUROHIS-QOL 8 ', 
                                           replacement = '') %>% 
                              stri_replace(fixed = 'EUROHIS-QOL 8', 
                                           replacement = 'sum score')) + 
           labs(title = .y))
  
  ## third panel: post-traumatic growth
  
  figures$clust_psych$ptg <- feat_clust$ribbon_plots
  
  for(i in names(figures$clust_psych$ptg)) {
    
    figures$clust_psych$ptg[[i]]$data <- 
      figures$clust_psych$ptg[[i]]$data %>% 
      filter(stri_detect(variable, fixed = 'ptgi'))
    
  }
  
  figures$clust_psych$ptg <- figures$clust_psych$ptg %>% 
    map2(., 
         paste0('Post-traumatic growth, PTGI, ', 
                c('training', 'test')), 
         ~.x + 
           scale_y_discrete(limits = rev(c('ptgi_total', 
                                           'ptgi_fctI', 
                                           'ptgi_fctII', 
                                           'ptgi_fctIII', 
                                           'ptgi_fctIV', 
                                           'ptgi_fctV')), 
                            labels = function(x) exchange(x, 
                                                          dict = ptsd$var_lexicon, 
                                                          key = 'variable', 
                                                          value = 'label') %>% 
                              stri_replace(regex = '\\s{1}score$', 
                                           replacement = '') %>% 
                              stri_replace(fixed = 'PTGI ', 
                                           replacement = '') %>% 
                              stri_replace(fixed = 'PTGI', 
                                           replacement = 'sum score')) + 
           labs(title = .y))
  
  ## bottom panel: PTSD
  
  figures$clust_psych$ptsd <- feat_clust$ribbon_plots
  
  for(i in names(figures$clust_psych$ptsd)) {
    
    figures$clust_psych$ptsd[[i]]$data <- 
      figures$clust_psych$ptsd[[i]]$data %>% 
      filter(stri_detect(variable, fixed = 'dsm5'))
    
  }
  
  figures$clust_psych$ptsd <- figures$clust_psych$ptsd %>% 
    map2(., 
         paste0('PTSD, PCL-5 DSM-5, ', 
                c('training', 'test')), 
         ~.x + 
           scale_y_discrete(limits = rev(c('dsm5_total', 
                                           'dsm5_B', 
                                           'dsm5_C', 
                                           'dsm5_D', 
                                           'dsm5_E')), 
                            labels = function(x) exchange(x, 
                                                          dict = ptsd$var_lexicon, 
                                                          key = 'variable', 
                                                          value = 'label') %>% 
                              stri_replace(regex = '\\s{1}score$', 
                                           replacement = '') %>% 
                              stri_replace(fixed = 'PCL-5 DSM-5 ', 
                                           replacement = '') %>% 
                              stri_replace(fixed = 'PCL-5 DSM-5', 
                                           replacement = 'sum score')) + 
           labs(title = .y))
  
  ## the entire figure
  
  figures$clust_psych <- figures$clust_psych %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          scale_x_continuous(limits = c(-1.7, 1.7), 
                             breaks = seq(-1.5, 1.5, by = 0.5))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              rel_heights = c(6, 9, 6, 5) + 3.75, 
              labels = c('A', '', 
                         'B', '', 
                         'C', '', 
                         'D', ''), 
              label_size = 10) %>% 
    plot_grid(get_legend(feat_clust$ribbon_plots[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.95, 0.05)) %>%
    as_figure(label = 'figure_2_cluster_psych_scores', 
              ref_name = 'clust_psych', 
              caption = paste('Scores of psychometry readouts', 
                              'in the mental clusters.'), 
              w = 180, 
              h = 230)
  
# Figure 3: mental health problem symptoms ------  
  
  insert_msg('Figure 3: Mental health problems')
  
  ## upper panel: major symptoms
  
  figures$clust_symptoms$upper <- feat_clust$plots$symptoms %>% 
    map(~.x[c('gad7_total_class', 
              'phq9_total_class', 
              'phq_events_total_class', 
              'rs13_total_class')]) %>% 
    transpose %>% 
    map2(., 
         c('Anxiety', 'Depression', 'Somatization', 'Resilience'), 
         function(x, y) plot_grid(plotlist = map2(x, 
                                                  paste(y, 
                                                        c('training', 'test'), 
                                                        sep = ', '), 
                                                  ~.x + 
                                                    labs(title = .y) + 
                                                    theme(legend.position = 'none')), 
                                  ncol = 2, 
                                  align = 'hv') %>% 
           plot_grid(get_legend(x[[1]] + 
                                  theme(legend.position = 'bottom')), 
                     nrow = 2, 
                     rel_heights = c(0.85, 0.15))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10)
  
  ## bottom panel: PTSD
  
  figures$clust_symptoms$bottom <- feat_clust$ptsd_clust_plots %>% 
    map(~.x + 
          expand_limits(x = 42) + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(feat_clust$ptsd_clust_plots[[1]] +
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15))
  
  ## the entire figure
  
  figures$clust_symptoms <- 
    plot_grid(figures$clust_symptoms$upper, 
              figures$clust_symptoms$bottom, 
              nrow = 2, 
              rel_heights = c(1.5, 1), 
              labels = c('', 'E'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_3_mental_symptoms_clusters', 
              ref_name = 'clust_symptoms', 
              caption = paste('Signs of mental health problems', 
                              'in the mental clusters.'), 
              w = 180, 
              h = 220)
  
# Figure 4: demographic for the cluster assignment ------
  
  insert_msg('Figure 4: demographic and clinical factors')

  figures$clinic_factors <- clust_bcg$plots %>% 
    map(~.x[c('sex', 
              'household_income_class', 
              'somatic_comorbidity', 
              'psych_comorbidity')]) %>% 
    transpose %>% 
    map2(., 
         c('Sex', 
           'Income/year', 
           'Somatic illness', 
           'Mental illness'), 
         function(x, y) plot_grid(plotlist = map2(x, 
                                                  paste(y, 
                                                        c('training', 'test'), 
                                                        sep = ', '),  
                                                 ~.x + 
                                                   labs(title = .y) + 
                                                   theme(legend.position = 'none', 
                                                         plot.margin = ggplot2::margin(l = 2, 
                                                                                       r = 2, 
                                                                                       t = 3, 
                                                                                       b = 3, 
                                                                                       unit = 'mm'))), 
                                  ncol = 2, 
                                  align = 'hv') %>% 
           plot_grid(get_legend(x[[1]] + 
                                  theme(legend.position = 'bottom') + 
                                  scale_fill_brewer(palette = 'Reds', 
                                                    labels = function(x) x %>% 
                                                      stri_replace_all(regex = '000\\s{1}', 
                                                                       replacement = 'K') %>% 
                                                      stri_replace(fixed = 'no income', 
                                                                   replacement = 'none') %>% 
                                                      stri_replace(fixed = 'EUR', 
                                                                   replacement = '')) + 
                                  theme(legend.position = 'bottom')), 
                     nrow = 2, 
                     rel_heights = c(0.85, 0.15))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_4_cluster_demo_clinical_factors', 
              ref_name = 'clinic_factors', 
              caption = paste('Demographic and clinical factors differing', 
                              'between the mental clusters', 
                              'of accident victims.'), 
              w = 180, 
              h = 140)
  
# Figure 5: injury severity and clusters ------
  
  insert_msg('Figure 5: injury severity and body parts')
  
  ## upper panel: injury severity and hospitalization
  
  figures$injury$upper <- clust_bcg$plots %>% 
    map(~.x[c('injury_sev_strata', 
              'hospitalization', 
              'surgery_done')]) %>% 
    transpose %>% 
    map(function(x) plot_grid(plotlist = map(x, 
                                             ~.x + 
                                               labs(title = stri_replace(.x$labels$title, 
                                                                         fixed = ' class', 
                                                                         replacement = '')) + 
                                               theme(legend.position = 'none', 
                                                     plot.margin = ggplot2::margin(l = 2, 
                                                                                   r = 2, 
                                                                                   t = 3, 
                                                                                   b = 3, 
                                                                                   unit = 'mm'))), 
                              ncol = 2, 
                              align = 'hv') %>% 
          plot_grid(get_legend(x[[1]] + 
                                 theme(legend.position = 'bottom')), 
                    nrow = 2, 
                    rel_heights = c(0.85, 0.15))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10)
  
  ## bottom panel: injury location
  
  figures$injury$bottom <- clust_bcg$body_part_plots %>% 
    map(~.x + 
          expand_limits(x = 65) + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(clust_bcg$body_part_plots[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.82, 0.08))
  
  ## the entire figure
  
  figures$injury <- plot_grid(figures$injury$upper, 
                              figures$injury$bottom, 
                              nrow = 2, 
                              rel_heights = c(1.2, 1), 
                              labels = c('', 'D'), 
                              label_size = 10) %>% 
    as_figure(label = 'figure_5_injury', 
              ref_name = 'injury', 
              caption = paste('Injury severity and location', 
                              'in the mental clusters.'), 
              w = 180,
              h = 230)
  
# Figure 6: accident aftermath ------
  
  insert_msg('Figure 6: Accident aftermath')
  
  figures$aftermath <- clust_bcg$plots %>% 
    map(~.x[c('accident_aftermath', 
              'flashback_frequency', 
              'caution_post_accident', 
              'psych_support_need', 
              'psych_support_post_accident')]) %>% 
    transpose %>% 
    map2(., c('Consequences', 
              'Flashbacks', 
              'Behavior', 
              'Support need', 
              'Support'), 
         function(x, y) plot_grid(plotlist = map2(x, 
                                                  paste(y, 
                                                        c('training', 'test'), 
                                                        sep = ', '), 
                                                  ~.x + 
                                                    labs(title = .y) + 
                                                    theme(legend.position = 'none', 
                                                          plot.margin = ggplot2::margin(l = 2, 
                                                                                        r = 2, 
                                                                                        t = 3, 
                                                                                        b = 3, 
                                                                                        unit = 'mm'))), 
                                  ncol = 2, 
                                  align = 'hv') %>% 
           plot_grid(get_legend(x[[1]] + 
                                  theme(legend.position = 'bottom')), 
                     nrow = 2, 
                     rel_heights = c(0.85, 0.15))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_6_accident_consequences', 
              ref_name = 'aftermath', 
              caption = paste('Consequences of the accident', 
                              'in the mental clusters.'), 
              w = 180, 
              h = 230)
  
# Figure 7: mental cluster classification, early predictors -------
  
  insert_msg('Figure 7: Mental cluster classification, cRF, early preds')
  
  figures$early_crf <- pam_early$confusion_plots %>% 
    map2(., 
         paste('Cluster classification,', 
               c('training', 'test')), 
         ~.x + 
           theme(legend.position = 'none') + 
           labs(title = .y)) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              align = 'hv') %>% 
    plot_grid(pam_early$importance_plot + 
                theme(plot.title.position = 'plot', 
                      plot.title = element_text(hjust = 0.3)), 
              ncol = 2, 
              rel_widths = c(0.9, 1.1), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_7_rf_early_classifier', 
              ref_name = 'early_crf', 
              caption = paste('Assignment of accident victims to', 
                              'the mental clusters based on', 
                              'explanatory factors available', 
                              'during acute medical management', 
                              'of the accident.'), 
              w = 180, 
              h = 150)  

# Saving the figures on the disc -------
  
  insert_msg('Saving the figures on the disc')

  figures %>% 
   walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail
  
  