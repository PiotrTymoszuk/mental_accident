# supplementary figures

  insert_head()
  
# container ------
  
  suppl_figures <- list()
  
# Figure S1: accident characteristic in the cohort and in Austria --------
  
  insert_msg('Figure S1: accident characteristic')
  
  suppl_figures$accident <- lit_kurasi$panels[c("sport_detail", 
                                                "accident_month", 
                                                "age_class")] %>% 
    map2(., c(68, 25, 35), 
         ~.x + 
           expand_limits(y = .y) + 
           theme(axis.title.x = element_blank()))
  
  suppl_figures$accident$sport_detail <- suppl_figures$accident$sport_detail + 
    guides(x = guide_axis(angle = 45))
  
  suppl_figures$accident <- suppl_figures$accident %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              axis = 'tblr',
              rel_heights = c(1.2, 1, 1), 
              labels = LETTERS, 
              label_size = 10) %>%
    as_figure(label = 'figure_s1_accident_characteristic', 
              ref_name = 'accident', 
              caption = paste('Mountain sport type, accident month, and age of', 
                              'the mountain accident victim in the study cohort', 
                              'and Austria.'), 
              w = 180, 
              h = 230)

# Figure S2: injured parts and PTSD -----
  
  insert_msg('Figure S2: injured parts and PTSD, cohort')
  
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
    as_figure(label = 'figure_s2_injury_ptsd', 
              ref_name = 'parts_ptsd', 
              caption = paste('Injured body regions and symptoms of PTSD', 
                              'in the study cohort.'), 
              w = 180, 
              h = 190)
  
# Figure S3: quality of life and post-traumatic growth ---------
  
  insert_msg('Figure S3: quality of life and post-traumatic growth')
  
  suppl_figures$qol_ptg <- 
    plot_grid(cohort$qol_details$plot + 
                theme(legend.position = 'bottom'), 
              cohort$mental_details$plots$ptgi, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s3_qol_ptg', 
              ref_name = 'qol_ptg', 
              caption = paste('Scores of quality of life and post-traumatic', 
                              'growth in the study cohort.'), 
              w = 180, 
              h = 110)
  
# Figure S4: traumatic events in PTSD in the cohort and literature -------
  
  insert_msg('Figure S4: PTSD in the cohort and literature')
  
  ## as requested by a reviewer

  suppl_figures$ptsd_literature <- 
    list(lit_plots$trauma_plots$general,
         lit_plots$ptsd_plot$general + 
           scale_x_continuous(limits = c(-0.5, 10)), 
         plot_grid(lit_plots$trauma_plots$alpine, 
                   get_legend(lit_plots$ptsd_symptom_plot), 
                   nrow = 2, 
                   rel_heights = c(2, 1)), 
         lit_plots$ptsd_symptom_plot + 
           theme(legend.position = 'none') + 
           scale_x_continuous(limits = c(-0.5, 25))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              axis = 'tblr', 
              rel_heights = c(5, 6), 
              labels = c('A', '', 'B', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s4_trauma_ptsd_literature', 
              ref_name = 'ptsd_literature', 
              caption = paste('Comparison of frequency of traumatic events,', 
                              'manifest PTSD, and symptoms of  PTSD', 
                              'in the study cohort and literature.'), 
              w = 180,
              h = 200)
  
# Figure S5: anxiety, depression and resilience in the cohort and literature ------
  
  insert_msg('Figure S5: anxiety, depression, resilience in the cohort and reports')
  
  suppl_figures$mental_literature <- 
    plot_grid(lit_plots$depr_anx_plot + 
                expand_limits(x = 35) + 
                labs(title = 'Anxious and depressive symptoms'), 
              lit_plots$aut_plot + 
                labs(title = 'Depressive symptoms') + 
                theme(legend.position = 'bottom'), 
              lit_plots$rs_plot, 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s5_anxiety_depression_resilience_literature', 
              ref_name = 'mental_literature', 
              caption = paste('Comparison of frequency of anxious and', 
                              'depressive symptoms and of resilience scoring', 
                              'in the study cohort and literature.'), 
              w = 180, 
              h = 200)
  
# Figure S6: cluster development ------
  
  insert_msg('Figure S6: cluster development')
  
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
    as_figure(label = 'figure_s6_cluster_development', 
              ref_name = 'clust_dev', 
              caption = paste('Definition of the mental clusters', 
                              'in the training subset', 
                              'of the study cohort.'), 
              w = 180, 
              h = 150)
  
# Figure S7: semi-supervised clustering -------
  
  insert_msg('Figure S7: semi-supervised clustering')
  
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
    as_figure(label = 'figure_s7_semi_supervised_clustering', 
              ref_name = 'semi_clust', 
              caption = paste('Semi-supervised clustering.'), 
              w = 180, 
              h = 200)
  
# Figure S8: clustering stats, distribution, inter-cluster cosine distances ------
  
  insert_msg('Figure S8: cluster stats, distribution and distances between clusters')
  
  ## upper panel: clustering stats
  
  suppl_figures$cosine$upper <- 
    semi_clust$stat_plots[c("sil_width", 
                            "frac_misclassified", 
                            "frac_var", 
                            "frac_np")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(semi_clust$stat_plots[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.8, 0.2))
  
  ## bottom panel: distribution and cosine distances

  suppl_figures$cosine$bottom <- 
    list(semi_clust$n_numbers$plot, 
         semi_clust$inter_distance$plot) %>% 
    map(~.x + theme(legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('B', 'C'), 
              label_size = 10)
  
  ## the entire figure
  
  suppl_figures$cosine <- 
    plot_grid(suppl_figures$cosine$upper, 
              suppl_figures$cosine$bottom, 
              nrow = 2, 
              labels = c('A', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s8_cluster_distribution_distance', 
              ref_name = 'cosine', 
              caption = paste('Distribution of the mental clusters and', 
                              'cosine distances between the mental clusters', 
                              'in the training and test subset of', 
                              'the study cohort.'), 
              w = 180, 
              h = 200)

# Figure S9: feature heat maps -------
  
  insert_msg('Figure S9: feature heat maps')
  
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
    as_figure(label = 'figure_s9_clustering_feature_heatmaps', 
              ref_name = 'feat_hm', 
              caption = paste('Levels of psychometric scores', 
                              'used for the cluster definition', 
                              'in the mental clusters.'), 
              w = 180, 
              h = 140)
  
# Figure S10: sensitivity analysis for the mental health clusters ------
  
  insert_msg('Figure S10: Sensitivity analysis for the clusters')
  
  ## 'confounders' that differ significantly 
  ## between the analyzed and excluded surveys are shown
  ## missing observations, income, injury severity class, 
  ## and hospitalization
  ##
  ## We're also showing prior accidents to answer the point with 
  ## the 'risk-seeking' population

  ## upper panel: explained clustering variance
  
  suppl_figures$cluster_se$upper <- se_summary$delta_plots$frac_var
  
  suppl_figures$cluster_se$upper$data <- suppl_figures$cluster_se$upper$data %>% 
    filter(split_factor %in% c('missing', 
                               'household_income_class', 
                               'prior_accident', 
                               'injury_sev_strata', 
                               'hospitalization', 
                               'accident_year')) %>% 
    mutate(split_factor = droplevels(split_factor))

  ## bottom panel: cluster sizes
  
  suppl_figures$cluster_se$bottom <- se_summary$size_plot
  
  suppl_figures$cluster_se$bottom$data <- suppl_figures$cluster_se$bottom$data %>% 
    filter(split_factor %in% c('global', 
                               'missing', 
                               'household_income_class', 
                               'prior_accident', 
                               'injury_sev_strata', 
                               'hospitalization', 
                               'accident_year')) %>% 
    mutate(split_factor = droplevels(split_factor))
  
  ## the entire figure
  
  suppl_figures$cluster_se <- suppl_figures$cluster_se %>% 
    map(~.x + 
          theme(axis.text.y = element_text(size = 7), 
                strip.text = element_text(size = 7))) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              rel_heights = c(1, 1.1), 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s10_mental_clusters_sensitivity_analysis', 
              ref_name = 'cluster_se', 
              caption = paste('Effects of incomplete psychometry data,', 
                              'annual income, prior mountain', 
                              'sport accidents, injury severity, hospitalization,', 
                              'and accident year on', 
                              'development of the mental health clusters.'), 
              w = 180, 
              h = 230)
 
# Figure S11: clusters for the cohort without and with the imputed records -------
  
  insert_msg('Figure S11: clusters: analysis cohort with and without imputation')
  
  suppl_figures$cluster_miss <- 
    c(se_summary$hm_plots[c("global", "merged")], 
      list(plot_grid(get_legend(se_summary$hm_plots$global + 
                                  theme(legend.position = 'bottom', 
                                        legend.text = element_text(size = 7), 
                                        legend.title = element_text(size = 7))), 
                     get_legend(se_summary$missing_plots$color_bar_plot + 
                                  theme(legend.position = 'bottom', 
                                        legend.text = element_text(size = 7), 
                                        legend.title = element_text(size = 7))), 
                     nrow = 2), 
           se_summary$missing_plots$color_bar_plot))
  
  suppl_figures$cluster_miss[c(1, 2, 4)] <- 
    suppl_figures$cluster_miss[c(1, 2, 4)] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                axis.text.y = element_text(size = 7), 
                plot.subtitle = element_text(size = 7)))
  
  suppl_figures$cluster_miss[[4]] <- 
    suppl_figures$cluster_miss[[4]] + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
  
  suppl_figures$cluster_miss <- suppl_figures$cluster_miss %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'v', 
              axis = 'tblr', 
              labels = c('A', 'B', '', ''), 
              label_size = 10, 
              rel_heights = c(0.8, 0.2))
  
  ## the entire figure
  
  suppl_figures$cluster_miss <- suppl_figures$cluster_miss %>% 
    as_figure(label = 'figure_s11_mental_clusters_incomplete_observations', 
              ref_name = 'cluster_miss', 
              caption = paste('Mental health cluster assignment of participants', 
                              'excluded from the analysis due incomplete', 
                              'psychometry data.'), 
              w = 180, 
              h = 150)

# Figure S12: additional background factors ------
  
  insert_msg('Figure S12: additional background factors')
  
  suppl_figures$demo <- 
    clust_bcg$plots[c("employment_status", 
                      "prior_accident", 
                      "accident_season", 
                      "accident_alone", 
                      "accident_injured_persons")] %>% 
    map(~.x + theme(legend.position = 'right')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_s12_demographic_factors', 
              ref_name = 'demo', 
              caption = paste('Employment status, prior sport accidents,', 
                              'and accident details in the mental clusters.'), 
              w = 180, 
              h = 180)
  
# Figure S13: sport types in the clusters -------
  
  insert_msg('Figure S13: Sport types in the clusters')
  
  suppl_figures$cluster_sport <- 
    plot_grid(clust_bcg$sport_type_panel + 
                theme(legend.position = 'bottom')) %>% 
    as_figure(label = 'figure_s13_cluster_sport_type', 
              ref_name = 'cluster_sport', 
              caption = 'Mountain sport types in the clusters.', 
              w = 180, 
              h = 160)
  
# Figure S15: mental cluster classifiers, full predictor set ------
  
  insert_msg('Figure S15: mental cluster classfier, full predictor set')
  
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
    as_figure(label = 'figure_s15_rf_full_classifier', 
              ref_name = 'full_class', 
              caption = paste('Assignment of accident victims to', 
                              'the mental clusters based on', 
                              'explanatory factors available', 
                              'during acute medical management', 
                              'of the accident and long-term follow-up.'), 
              w = 180, 
              h = 180)
  
# Figure S14 and S16: variable importance, early and all predictors -------
  
  insert_msg('Figure S14 and S16: variable importance, early and all predictors')
  
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
         label = c('figure_s14_importance_early_predictors', 
                   'figure_s16_importance_all_predictors'), 
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