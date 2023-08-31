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
              h = 4156/2856 * 120)
  
# Figure 2: psychometry scores in the clusters -------
  
  insert_msg('Figure 2: psychometry scoring in the clusters')
  
  ## upper panel: stress, anxiety, depression, somatic symptoms, panic
  ## resilience and sense-of coherence
  ##
  ## second panel: quality of life
  ##
  ## third panel: post-traumatic growth
  ##
  ## bottom panel: PTSD
  
  figures$clust_psych <- 
    list(variables = list(c('gad7_total', 
                            'phq9_total', 
                            'phq_events_total', 
                            'phqd_panic_total', 
                            'soc9l_total', 
                            'rs13_total'), 
                          c('eurohis_total', 
                            'eurohis_qol', 
                            'eurohis_selfesteem', 
                            'eurohis_energy', 
                            'eurohis_activity', 
                            'eurohis_health', 
                            'eurohis_relationship', 
                            'eurohis_finances', 
                            'eurohis_housing'), 
                          c('ptgi_total', 
                            'ptgi_fctI', 
                            'ptgi_fctII', 
                            'ptgi_fctIII', 
                            'ptgi_fctIV', 
                            'ptgi_fctV'), 
                          c('dsm5_total', 
                            'dsm5_B', 
                            'dsm5_C', 
                            'dsm5_D', 
                            'dsm5_E')), 
         title_prefix = c('Major mental health readouts', 
                          'Quality of life, EUROHIS-QOL 8', 
                          'Post-traumatic growth, PTGI', 
                          'PTSD, PCL-5')) %>% 
    pmap(split_ribbon, 
         plot_lst = feat_clust$ribbon_plots, 
         lex_lst = feat_clust$ribbon_labs)

  ## the entire figure
  
  figures$clust_psych <- figures$clust_psych %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          scale_x_continuous(limits = c(-1.6, 1.6), 
                             breaks = seq(-1.5, 1.5, by = 0.5))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              rel_heights = c(6, 8, 6, 5) + 3.75, 
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
              caption = paste('Scores of mental disorder symptoms,', 
                              'sense of coherence, resilience,', 
                              'quality of life, post-traumatic growth, and', 
                              'post-traumatic stress disorder in', 
                              'the mental clusters.'), 
              w = 180, 
              h = 230)
  
# Figure 3: mental health problem symptoms ------  
  
  insert_msg('Figure 3: Mental health problems')
  
  ## upper panel: major symptoms and PTSD
  
  figures$clust_symptoms$upper <- 
    plot_grid(plot_grid(clust_bcg$mental_symptoms$plot + 
                          theme(legend.position = 'none') + 
                          expand_limits(x = 16), 
                        get_legend(clust_bcg$mental_symptoms$plot + 
                                     theme(legend.position = 'bottom')),
                        nrow = 2, 
                        rel_heights = c(0.73, 0.27)), 
              clust_bcg$ptsd_symptoms$plot + 
                theme(legend.position = 'none') + 
                expand_limits(x = 64), 
              ncol = 2, 
              rel_widths = c(1, 1.05))
  
  ## bottom panel: resilience classes and frequency of flashbacks
  
  figures$clust_symptoms$bottom <- 
    clust_bcg$plots[c("rs13_total_class", "flashback_frequency")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr')
  
  ## the entire figures
  
  figures$clust_symptoms <- 
    plot_grid(figures$clust_symptoms$upper, 
              figures$clust_symptoms$bottom, 
              nrow = 2, 
              rel_heights = c(2, 1), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_3_mental_symptoms_clusters', 
              ref_name = 'clust_symptoms', 
              caption = paste('Symptoms of mental disorders and resilience', 
                              'classes in the mental clusters.'), 
              w = 180, 
              h = 210)
  
# Figure 4: demographics and medical history of the clusters ------
  
  insert_msg('Figure 4: demographic and clinical factors')
  
  ## plots with significant effects and variables of theoretical relevance for
  ## the cluster assignment

  figures$clinic_factors <- 
    list(x = clust_bcg$plots[c("age", "sex", "education", 
                               "household_income_class", 
                               "trauma_risk_profession", 
                               "traumatic_number", 
                               "somatic_comorbidity", 
                               "psych_comorbidity")], 
         y = c('none', rep('right', 7))) %>% 
    pmap(function(x, y) x + 
           theme(legend.position = y))
  
  figures$clinic_factors$household_income_class <- 
    figures$clinic_factors$household_income_class + 
    scale_fill_brewer(palette = 'Reds', 
                      labels = c('none', 
                                 '<30K', 
                                 '30-45K', 
                                 '\u226545K'))
  
  figures$clinic_factors$age <- figures$clinic_factors$age + 
    labs(y = 'age, years')
  
  figures$clinic_factors$education <- 
    figures$clinic_factors$education + 
    scale_fill_brewer(palette = 'Reds', 
                      labels = c('primary', 'secondary', 'tertiary'))
    
  figures$clinic_factors <- figures$clinic_factors %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_4_cluster_demo_clinic',
              ref_name = 'clinic_factors', 
              caption = paste('Sociodemographic and medical history', 
                              'characteristic of the mental clusters', 
                              'of accident victims.'), 
              w = 180, 
              h = 220)
  
# Figure 5: accident, injury severity and body parts ------
  
  insert_msg('Figure 5: accident, injury severity and body parts')
  
  ## top panel: accident culprit and rescue
  
  figures$injury$top <- 
    clust_bcg$plots[c("accident_culprit", "accident_rescue")]
  
  figures$injury$top$accident_rescue <- 
    figures$injury$top$accident_rescue + 
    scale_fill_brewer(palette = 'Reds', 
                      labels = c('self', 'comrade', 'professional'))
  
  figures$injury$top <- figures$injury$top %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', ''), 
              label_size = 10)
  
  ## left panel: injury severity, hospitalization and surgery
  
  figures$injury$left <- 
    clust_bcg$plots[c("injury_sev_strata", 
                      "hospitalization", 
                      "surgery_done")] %>% 
    map(~.x + 
          theme(legend.position = 'right'))
  
  figures$injury$left$injury_sev_strata <- 
    figures$injury$left$injury_sev_strata + 
    labs(title = 'Injury severity, AIS')
  
  figures$injury$left <- figures$injury$left %>% 
    plot_grid(plotlist = ., 
              nrow = 3,
              align = 'hv', 
              axis = 'tblr')
  
  ## right panel: injured body parts
  
  figures$injury$right <- clust_bcg$body_part_plots + 
    theme(legend.position = 'bottom') + 
    expand_limits(x = 53)
  
  ## the entire figure
  
  figures$injury <- 
    plot_grid(figures$injury$left, 
              figures$injury$right, 
              ncol = 2, 
              rel_widths = c(1, 1.25), 
              labels = c('B', 'C'), 
              label_size = 10) %>% 
    plot_grid(figures$injury$top, 
              ., 
              nrow = 2, 
              rel_heights = c(1, 3)) %>% 
    as_figure(label = 'figure_5_injury', 
              ref_name = 'injury', 
              caption = paste('Accident culprits, accident rescue,', 
                              'injury severity', 
                              'and injured body parts in the mental clusters.'), 
              w = 180,
              h = 220)

# Figure 6: accident aftermath ------
  
  insert_msg('Figure 6: Accident aftermath')
  
  figures$aftermath <- 
    clust_bcg$plots[c("psych_support_post_accident", "psych_support_need", 
                      "accident_aftermath", "caution_post_accident", 
                      "same_sport_type_post_accident")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_6_accident_consequences', 
              ref_name = 'aftermath', 
              caption = paste('Psychological support and consequences', 
                              'of the accident in the mental clusters.'), 
              w = 180, 
              h = 180)
    
# Figure 7: mental cluster classification, early predictors -------
  
  insert_msg('Figure 7: Mental cluster classification, cRF, early preds')
  
  ## upper panel: overall performance stats
  
  figures$early_class$upper <- 
    map2(early_class$kappa_bs_plots[c("train", "test")], 
         paste('Overall model performance,', 
               c('training', 'test')), 
         ~.x + 
           expand_limits(x = 1, y = 2) +
           labs(title = .y) + 
           theme(legend.position = 'none')) %>% 
    c(list(get_legend(early_class$kappa_bs_plots[[1]]  +
                        guides(fill = 'none')))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              rel_widths = c(1, 1, 0.2), 
              align = 'hv', 
              axis = 'tblr')
  
  ## bottom panel: ROC curves
  
  figures$early_class$bottom <- 
    map2(early_class$roc_plots$pts[c("train", "test")], 
         paste('PTS cluster detection,', 
               c('training', 'test')), 
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr')
  
  ## the entire figure
  
  figures$early_class <- 
    plot_grid(figures$early_class$upper, 
              figures$early_class$bottom, 
              nrow = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_7_early_classifier', 
              ref_name = 'early_class', 
              caption = paste('Assignment of accident victims to', 
                              'the mental clusters based on', 
                              'explanatory factors available', 
                              'during acute medical management', 
                              'of the accident.'), 
              w = 180, 
              h = 180)

# Saving the figures on the disc -------
  
  insert_msg('Saving the figures on the disc')

  figures %>% 
   walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail
  
  