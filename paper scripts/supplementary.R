# supplementary figures

  insert_head()
  
# container ------
  
  suppl_figures <- list()

# Figure S1: injured parts and PTSD -----
  
  insert_msg('Figure S1: injured parts and PTSD, cohort')
  
  ## top panel: injured body parts and PTSD scores
  ##
  ## bottom panel: the PTSD symptom overlap
  
  suppl_figures$parts_ptsd <- 
    ((cohort$injury$plot + 
       expand_limits(x = 50) + 
        theme(plot.tag.position = 'topleft', 
              plot.tag = element_text(size = 10, face = 'bold'))) + 
    (cohort$mental_details$plots$dsm + 
       theme(plot.tag.position = 'topleft', 
             plot.tag = element_text(size = 10, face = 'bold'))))/
    cohort$ptsd_overlap$upset_plot + 
    plot_layout(heights = c(1, 1.5)) + 
    plot_annotation(tag_levels = c('A')) + 
    theme(plot.tag = element_text(size = 10, face = 'bold'), 
          plot.margin = ggplot2::margin(2, 2, 2, 2, unit = 'mm'))
  
  suppl_figures$parts_ptsd <- 
    suppl_figures$parts_ptsd %>% 
    as_figure(label = 'figure_s1_injury_ptsd', 
              ref_name = 'parts_ptsd', 
              caption = paste('Injured body regions and symptoms of PTSD', 
                              'in the study cohort.'), 
              w = 180, 
              h = 190)
  
# Figure S2: quality of life and post-traumatic growth ---------
  
  insert_msg('Figure S2: quality of life and post-traumatic growth')
  
  suppl_figures$qol_ptg <- 
    plot_grid(cohort$qol_details$plot + 
                theme(legend.position = 'bottom'), 
              cohort$mental_details$plots$ptgi, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s2_qol_ptg', 
              ref_name = 'qol_ptg', 
              caption = paste('Scores of quality of life and post-traumatic', 
                              'growth in the study cohort.'), 
              w = 180, 
              h = 110)
  
# Figure S3: mental disorder symptoms in the cohort and in literature reports -------
  
  insert_msg('Figure S3: mental disorder symptoms in the cohort and literature')
  
  ## as requested by a reviewer
  
  suppl_figures$mental_symptoms <- 
    plot_grid(lit_plots$trauma_plots,
              lit_plots$ptsd_plot + 
                scale_x_continuous(limits = c(-3, 25)), 
              lit_plots$depr_anx_plot + 
                scale_x_continuous(limits = c(-3, 20)), 
              ncol = 2, 
              align = 'hv',
              axis = 'tblr', 
              rel_heights = c(1.2, 0.8), 
              labels = c('A', 'B', 
                         'C', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s3_mental_symptoms_literature', 
              ref_name = 'mental_symptoms', 
              caption = paste('Comparison of frequency of traumatic events,', 
                              'symptoms of PTSD, depression and anxiety', 
                              'in the study cohort and published population', 
                              'studies.'), 
              w = 180,
              h = 180)
  
# Figure S4: cluster development ------
  
  insert_msg('Figure S4: cluster development')
  
  ## upper panel: variance and cross-validation
  
  suppl_figures$clust_dev$upper <- clust_devel$result_plot
  
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
    as_figure(label = 'figure_s4_cluster_development', 
              ref_name = 'clust_dev', 
              caption = paste('Definition of the mental clusters', 
                              'in the training subset', 
                              'of the study cohort.'), 
              w = 180, 
              h = 150)
  
# Figure S5: semi-supervised clustering -------
  
  insert_msg('Figure S5: semi-supervised clustering')
  
  ## upper panel: UMAP plots
  
  suppl_figures$semi_clust$upper <- 
    list(x = semi_clust$data_umap, 
         y = paste('UMAP,', c('training', 'test')), 
         z = semi_clust$stats$frac_var) %>% 
    pmap(function(x, y, z) x + 
           labs(title = y, 
                subtitle = paste0(x$labels$subtitle, 
                                  ', explained variance = ', 
                                  signif(z, 2))) + 
           theme(plot.tag = element_blank(), 
                 legend.position = 'bottom')) %>% 
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
    as_figure(label = 'figure_s5_semi_supervised_clustering', 
              ref_name = 'semi_clust', 
              caption = paste('Semi-supervised clustering.'), 
              w = 180, 
              h = 200)
  
# Figure S6: cluster distribution, inter-cluster cosine distances ------
  
  insert_msg('Figure S6: cluster distribution and distances between clusters')

  suppl_figures$cosine <- 
    list(semi_clust$n_numbers$plot, 
         semi_clust$inter_distance$plot) %>% 
    map(~.x + theme(legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s6_cluster_distribution_distance', 
              ref_name = 'cosine', 
              caption = paste('Distribution of the mental clusters and', 
                              'cosine distances between the mental clusters', 
                              'in the training and test subset of', 
                              'the study cohort.'), 
              w = 180, 
              h = 100)

# Figure S7: feature heat maps -------
  
  insert_msg('Figure S6: feature heat maps')
  
  suppl_figures$feat_hm <- feat_clust$clust_hm %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(), 
                axis.line.x = element_blank(), 
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
    as_figure(label = 'figure_s7_clustering_feature_heatmaps', 
              ref_name = 'feat_hm', 
              caption = paste('Levels of psychometric scores', 
                              'used for the cluster definition', 
                              'in the mental clusters.'), 
              w = 180, 
              h = 140)
  
# Figure S8: sensitivity analysis for the mental health clusters ------
  
  insert_msg('Figure S8: Sensitivity analysis for the clusters')
  
  suppl_figures$cluster_se <- 
    plot_grid(se_summary$delta_plots$frac_var + 
                theme(legend.position = 'bottom')) %>% 
    as_figure(label = 'figure_s8_mental_clusters_sensitivity_analysis', 
              ref_name = 'cluster_se', 
              caption = paste('Sensitivity analysis of the mental clusters', 
                              'of mountain sport accident victims.'), 
              w = 180, 
              h = 210)
  
# Figure S9: mental clusters in the genders --------
  
  insert_msg('Figure S9: mental clusters in females and males')
  
  suppl_figures$cluster_genders <- se_summary$hm_plots[c("female", "male")] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.subtitle = element_text(size = 7), 
                axis.text.y = element_text(size = 7))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(se_summary$hm_plots$female + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_s9_mental_clusters_genders', 
              ref_name = 'clusters_genders', 
              caption = paste('Mental clusters developed for female and male', 
                              'montain sport accident victims.'), 
              w = 180, 
              h = 170)

# Figure S10: additional background factors ------
  
  insert_msg('Figure S10: additional background factors')
  
  suppl_figures$demo <- 
    clust_bcg$plots[c("employment_status", "prior_accident", 
                      "accident_season", "sport_type", 
                      "accident_alone", "accident_injured_persons")] %>% 
    map(~.x + theme(legend.position = 'right'))
  
  suppl_figures$demo$sport_type <- 
    suppl_figures$demo$sport_type + 
    scale_fill_brewer(palette = 'Reds', 
                      labels = c('ski\nsnowboard', 
                                 'sledding', 
                                 'climbing\nhiking\nmountaineering', 
                                 'biking', 
                                 'other'))
  
  suppl_figures$demo <- suppl_figures$demo %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_s10_demographic_factors', 
              ref_name = 'demo', 
              caption = paste('Employment status, prior sport accidents,', 
                              'and accident details in the mental clusters.'), 
              w = 180, 
              h = 180)
  
# Figure S12: mental cluster classifiers, full predictor set ------
  
  insert_msg('Figure S12: mental cluster classfier, full predictor set')
  
  ## upper panel: overall performance stats
  
  suppl_figures$full_class$upper <- 
    map2(full_class$kappa_bs_plots[c("train", "test")], 
         paste('Overall model performance,', 
               c('training', 'test')), 
         ~.x + 
           expand_limits(x = 1, y = 2) +
           labs(title = .y) + 
           theme(legend.position = 'none')) %>% 
    c(list(get_legend(full_class$kappa_bs_plots[[1]]  +
                        guides(fill = 'none')))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              rel_widths = c(1, 1, 0.2), 
              align = 'hv', 
              axis = 'tblr')
  
  ## bottom panel: ROC curves
  
  suppl_figures$full_class$bottom <- 
    map2(full_class$roc_plots$pts[c("train", "test")], 
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
  
  suppl_figures$full_class <- 
    plot_grid(suppl_figures$full_class$upper, 
              suppl_figures$full_class$bottom, 
              nrow = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s12_rf_full_classifier', 
              ref_name = 'full_class', 
              caption = paste('Assignment of accident victims to', 
                              'the mental clusters based on', 
                              'explanatory factors available', 
                              'during acute medical management', 
                              'of the accident and long-term follow-up.'), 
              w = 180, 
              h = 180)
  
# Figure S11 and S13: variable importance, early and all predictors -------
  
  insert_msg('Figure S11 and S13: variable importance, early and all predictors')
  
  suppl_figures[c('importance_early', 'importance_full')] <- 
    list(early_class$importance$plots[c("ranger", "svmRadial", "sda", "cforest")],  
         full_class$importance$plots[c("ranger", "svmRadial", "sda", "cforest")]) %>% 
    map(map, ~.x + 
          theme(plot.title.position = 'plot', 
                plot.title = element_text(hjust = 0.2), 
                axis.text = element_text(size = 6),
                axis.title.x = element_text(size = 6))) %>% 
    map(~plot_grid(plotlist = ., 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tblr', 
                   labels = LETTERS, 
                   label_size = 10))
  
  suppl_figures[c('importance_early', 'importance_full')] <- 
    suppl_figures[c('importance_early', 'importance_full')] %>% 
    list(x = ., 
         label = c('figure_s11_importance_early_predictors', 
                   'figure_s13_importance_all_predictors'), 
         ref_name = names(.), 
         caption = paste(paste('Variable importance metrics for the random forest,', 
                               'support vector machine, discriminant analysis,', 
                               'and conditional random forest algorithms.'), 
                         c(paste('Predictors available during acute medical', 
                                 'management of the accident.'), 
                           paste('Predictors available during acute medical', 
                                 'management of the accident and during', 
                                 'the follow-up.')))) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 220)
  
# Saving the figures on the disc -------
  
  insert_msg('Saving the figures on the disc')
  
  suppl_figures %>% 
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -------
  
  insert_tail()