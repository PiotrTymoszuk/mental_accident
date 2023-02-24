# supplementary figures

  insert_head()
  
# container ------
  
  suppl_figures <- list()
  
# Figure S1: injured parts and mental health details -----
  
  insert_msg('Figure S1: injured parts and mental health, cohort')
  
  suppl_figures$parts_mental_cohort <- 
    list(cohort$injury$plot + 
           expand_limits(x = 50), 
         cohort$mental_details$plots$eurohis, 
         cohort$mental_details$plots$ptgi, 
         ggdraw(), 
         cohort$mental_details$plots$dsm, 
         cohort$ptsd_cluster$plot + 
           expand_limits(x = 13)) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', 'B', 'C', '', 'D'), 
              label_size = 10, 
              rel_heights = c(1.4, 1.1, 1)) %>% 
    as_figure(label = 'figure_s1_parts_mental_cohort', 
              ref_name = 'parts_mental_cohort', 
              caption = paste('Injured body regions and detailed scoring', 
                              'of quality of life, post-traumatic syndrome', 
                              'disorder and post-traumatic growth', 
                              'in the study cohort.'), 
              w = 180, 
              h = 210)
  
# Figure S2: cluster development ------
  
  insert_msg('Figure S2: cluster development')
  
  ## upper panel: variance and cross-validation
  
  suppl_figures$clust_dev$upper <- 
    plot_grid(clust_devel$result_plot, 
              ncol = 2, 
              rel_widths = c(0.8, 0.2))
  
  ## bottom panel: WSS and silhouette
  
  suppl_figures$clust_dev$bottom <- semi_clust$diagn_plots %>% 
    map(~.x + 
          labs(subtitle = paste0(.x$labels$subtitle, 
                                ', n = ', 
                                nrow(clust_globals$analysis_tbl$training))) + 
          theme(plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv')
  
  ## the entire figure
  
  suppl_figures$clust_dev <- 
    plot_grid(suppl_figures$clust_dev$upper, 
              suppl_figures$clust_dev$bottom, 
              nrow = 2, 
              rel_heights = c(1.1, 1), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s2_cluster_development', 
              ref_name = 'clust_dev', 
              caption = paste('Definition of the mental clusters', 
                              'in the training subset', 
                              'of the study cohort.'), 
              w = 180, 
              h = 150)
  
# Figure S3: semi-supervised clustering -------
  
  insert_msg('Figure S3: semi-supervised clustering')
  
  ## upper panel: UMAPS
  
  suppl_figures$semi_clust$upper <- semi_clust$data_umap %>% 
    map2(., paste('UMAP,', c('training', 'test')), 
         ~.x + labs(title = .y)) %>%  
    map2(., semi_clust$variance$variance, 
         ~.x + 
           theme(legend.position = 'bottom', 
                 plot.tag = element_blank()) + 
           labs(subtitle = paste0(.x$labels$subtitle, 
                                  ', expl. variance = ', 
                                  signif(.y, 2)))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr')
  
  ## bottom panel: distance heat maps
  
  suppl_figures$semi_clust$bottom <- semi_clust$dist_hm %>% 
    map2(., paste('Distance heat map,', c('training', 'test')), 
         ~.x + 
           theme(plot.tag = element_blank()) +
           scale_fill_gradient2(low = 'firebrick', 
                                mid = 'white',
                                high = 'steelblue', 
                                midpoint = 1, 
                                limits = c(0, 2), 
                                oob = scales::squish, 
                                name = 'distance') + 
           labs(title = .y))
  
  suppl_figures$semi_clust$bottom <- suppl_figures$semi_clust$bottom %>% 
    map(~.x + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(suppl_figures$semi_clust$bottom[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15))
  ## the entire figure
  
  suppl_figures$semi_clust <- 
    plot_grid(suppl_figures$semi_clust$upper, 
              suppl_figures$semi_clust$bottom, 
              nrow = 2, 
              rel_heights = c(1, 1.2), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s3_semi_supervised_clustering', 
              ref_name = 'semi_clust', 
              caption = paste('Semi-supervised clustering.'), 
              w = 180, 
              h = 200)
  
# Figure S4: feature heat maps -------
  
  insert_msg('Figure S4: feature heat maps')
  
  suppl_figures$feat_hm <- feat_clust$clust_hm %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.tag = element_blank(), 
                strip.background.y = element_blank(), 
                strip.text.y = element_blank(), 
                plot.title.position = 'plot')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(feat_clust$clust_hm[[1]] +
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_s4_clustering_feature_heatmaps', 
              ref_name = 'feat_hm', 
              caption = paste('Levels of psychometric clustering', 
                              'scores in the mental clusters.'), 
              w = 180, 
              h = 140)

# Figure S5: additional background factors ------
  
  insert_msg('Figure S5: additional background factors')
  
  suppl_figures$demo <- clust_bcg$plots %>% 
    map(~.x[c('age_class', 
              'education', 
              'prior_accident', 
              'traumatic_event')]) %>% 
    transpose %>% 
    map2(., c('Age', 'Education',  
              'Prior accident', 'Trauma event'), 
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
  
  suppl_figures$demo <- suppl_figures$demo %>% 
    as_figure(label = 'figure_s5_demographic_factors', 
              ref_name = 'demo', 
              caption = paste('Age, education,', 
                              'prior accidents and traumatic events', 
                              'in the mental clusters.'), 
              w = 180, 
              h = 140)
  
# Figure S6: additinal accident details -------
  
  insert_msg('Figure S6: additional accident details')

  suppl_figures$accident <- clust_bcg$plots %>% 
    map(~.x[c('sport_type', 'accident_rescue', 'accident_rescue_mode')]) %>%
    transpose %>% 
    map(function(x) map(x, 
                        ~.x + 
                          theme(legend.position = 'none')) %>% 
          c(list(get_legend(x[[1]])))) %>% 
    unlist(recursive = FALSE) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', '', 
                         'B', '', '', 
                         'C', '', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s6_accident_details', 
              ref_name = 'accident', 
              caption = paste('Accident sport type and accident rescue', 
                              'in the mental clusters.'), 
              w = 180, 
              h = 220)
  
# Figure S7: OneR, accuracy and kappa -------
  
  insert_msg('Figure S7: oneR')
  
  suppl_figures$oneR <- 
    class_one$plots$global[c("Accuracy", "Kappa")] %>% 
    map2(., 
         c('Cluster classification, single factors, accuracy', 
           'Cluster classification, single factors, kappa'), 
         ~.x + 
           labs(title = .y) +
           theme(legend.position = 'none', 
                 plot.title.position = 'plot', 
                 plot.title = element_text(hjust = 0.3))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(class_one$plots$global[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_s7_cluster_classification_single_factors', 
              ref_name = 'oneR', 
              caption = paste('Prediction of the mental cluster assignment', 
                              'by single demographic, socioeconomic,', 
                              'clinical and accident-related factors.'), 
              w = 180, 
              h = 120)
  
  
# Saving the figures on the disc -------
  
  insert_msg('Saving the figures on the disc')
  
  suppl_figures %>% 
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -------
  
  insert_tail()