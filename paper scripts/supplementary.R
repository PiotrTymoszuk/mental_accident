# supplementary figures

  insert_head()
  
# container ------
  
  suppl_figures <- list()
  
# Figure S1: injured parts and mental health details -----
  
  insert_msg('Figure S1: injured parts and mental health, cohort')
  
  suppl_figures$parts_mental_cohort <- 
    list(cohort$injury$plot, 
         cohort$mental_details$plots$eurohis, 
         cohort$mental_details$plots$dsm, 
         cohort$ptsd_cluster$plot, 
         cohort$mental_details$plots$ptgi) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', 'B', 'C', '', 'D'), 
              label_size = 10, 
              rel_heights = c(1.4, 1, 1.1)) %>% 
    as_figure(label = 'figure_s1_parts_mental_cohort', 
              ref_name = 'parts_mental_cohort', 
              caption = paste('Injured body regions and detailed scoring', 
                              'of quality of life, post-traumatic syndrome', 
                              'disorder and post-traumatic growth', 
                              'in the study cohort.'), 
              w = 180, 
              h = 210)
  
# Figure S2: mental features associated with participant's age ------
  
  insert_msg('Figure S2: age and mental health')
  
  ## upper panel: panic, resilience and sense of coherence
  
  suppl_figures$age_mental$upper <- age$plots[c("phqd_panic_total", 
                                                "rs13_total", 
                                                "soc9l_total")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  ## bottom panel: panels of plots for QoL and PTG
  
  suppl_figures$age_mental$bottom <- 
    plot_grid(age$panels$eurohis + 
                theme(legend.position = 'none', 
                      plot.subtitle = element_blank()), 
              plot_grid(age$panels$ptgi + 
                          theme(legend.position = 'none', 
                                plot.subtitle = element_blank()), 
                        get_legend(age$panels[[1]] + 
                                     scale_fill_brewer(labels = age$analysis_tbl %>% 
                                                         label_n(age), 
                                                       palette = 'Blues') + 
                                     theme(legend.position = 'bottom')), 
                        nrow = 2, 
                        rel_heights = c(0.68, 0.32)), 
              ncol = 2)
  
  ## the entire figure
  
  suppl_figures$age_mental <- 
    plot_grid(suppl_figures$age_mental$upper, 
              ggdraw(), 
              suppl_figures$age_mental$bottom, 
              nrow = 3, 
              rel_heights = c(1, 0.1, 2.2)) %>% 
    as_figure(label = 'figure_s2_age_mental_health', 
              ref_name = 'age_mental', 
              caption = paste("Association of measures of mental health", 
                              "after the accident with participant's age"), 
              w = 180, 
              h = 210)
  
# Figure S3: mental health features associated with gender ------
  
  insert_msg('Figures S3: mental health and gender')
  
  suppl_figures$gender_mental <- gender$panels[c("dsm", "ptgi")] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(gender$panels[[1]] + 
                           scale_fill_brewer(labels = ptsd$dataset %>% 
                                               label_n(sex), 
                                             palette = 'Oranges') + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_s3_gender_mental', 
              ref_name = 'gender_mental', 
              caption = paste(paste("Association of measures of mental health", 
                                    "after the accident with", 
                                    "participant's gender")), 
              w = 180, 
              h = 100)
  
# Figure S4: basic mental health features associated with mental illness -----
  
  insert_msg('Figure S4: basic mental health features in mental illness strata')
  
  suppl_figures$mental_illness_base <- 
    plot_grid(mental$plots$gad7_total + 
                theme(legend.position = 'none'), 
              mental$plots$gad7_total_class, 
              mental$plots$phq9_total + 
                theme(legend.position = 'none'), 
              mental$plots$phq9_total_class, 
              mental$plots$phq_events_total + 
                theme(legend.position = 'none'), 
              mental$plots$phqd_panic_total + 
                theme(legend.position = 'none'), 
              mental$plots$pss4_total + 
                theme(legend.position = 'none'), 
              mental$plots$soc9l_total + 
                theme(legend.position = 'none'), 
              mental$plots$rs13_total + 
                theme(legend.position = 'none'), 
              mental$plots$rs13_total_class, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>%
    as_figure(label = 'figure_s4_mental_illness_base', 
              ref_name = 'mental_illness_base', 
              caption = paste('Readouts of anxiety, depression,', 
                              'stress, loss of sense of coherence and of', 
                              'resilience in participants with and without', 
                              'mental illness.'), 
              w = 180, 
              h = 240)
  
# Figure S5: quality of life and PTSD in mental illness ------
  
  insert_msg('Figure S5: mental illness and QoL and PTSD')
  
  ## right panel: PTSD
  
  suppl_figures$mental_illness_ext$right <- 
    list(mental$panels$dsm, 
         mental$ptsd_cluster$plot +
           expand_limits(x = 35)) %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              align = 'hv', 
              axis = 'tblr')
  
  ## left panel: QoL
  
  suppl_figures$mental_illness_ext$left <- 
    plot_grid(mental$panels$eurohis + 
                theme(legend.position = 'none', 
                      plot.subtitle = element_blank()), 
              get_legend(mental$panels[[1]] + 
                           scale_fill_brewer(palette = 'Greys', 
                                             labels = ptsd$dataset %>% 
                                               label_n(psych_comorbidity), 
                                             name = 'Mental illness') + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  ## the entire figure
  
  suppl_figures$mental_illness_ext <- 
    plot_grid(suppl_figures$mental_illness_ext$left, 
              suppl_figures$mental_illness_ext$right, 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s5_mental_illness_qol_ptsd', 
              ref_name = 'mental_illness_ext', 
              caption = paste('Signs of diminished quality of life', 
                              'and symptoms of post-traumatic', 
                              'syndrome disorder in participants', 
                              'with and without mental illness.'), 
              w = 180, 
              h = 160)
  
# Saving the figures on the disc -------
  
  insert_msg('Saving the figures on the disc')
  
  suppl_figures %>% 
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -------
  
  insert_tail()