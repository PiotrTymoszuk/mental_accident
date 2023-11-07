# Modeling of the mental health cluster assignment with the full 
# set of predictors

  insert_head()
  
# container -------
  
  early_class <- list()
  
# Building the models -------
  
  insert_msg('Building the models')
  
  ## I'm actually taking over the models tuned and trained previously
  
  early_class$models <- 
    list(ranger = ranger_tune, 
         nnet = nnet_tune, 
         svmRadial = svm_tune, 
         rpart = rpart_tune, 
         sda = sda_tune, 
         cforest = crf_tune, 
         elnet = elnet_tune) %>% 
    map(~.x$models$early) %>% 
    map(as_caretx)
  
# Predictions ------
  
  insert_msg('Predictions')
  
  early_class$predictions <- early_class$models %>% 
    map(predict, newdata = class_globals$analysis_tbl$test)
  
# Overall performance stats -------
  
  insert_msg('Overall model performance stats')

  ## overall performance stats
  
  early_class$overall_stats <- early_class$models %>% 
    map(summary, 
        newdata = class_globals$analysis_tbl$test, 
        wide = TRUE) %>% 
    map(compress, names_to = 'dataset') %>% 
    compress(names_to = 'method') %>% 
    mutate(dataset = factor(dataset, c('train', 'cv', 'test')))
  
# Plot of the model performance stats ------- 
  
  insert_msg('Plots of the overall performance stats')
  
  ## plots of kappa, accuracy, Brier score and Brier's skill score
  
  early_class$overall_stat_plots <- 
    plot_overall_stats(data = early_class$overall_stats, 
                       y_var = 'method', 
                       title_prefix = 'Mental cluster prediction') %>% 
    map(~.x + 
          scale_y_discrete(labels = class_globals$algo_labs))
  
  ## Kappa vs Brier score
  
  early_class$kappa_bs_plots <- 
    plot_kappa_bs(early_class$overall_stats) %>% 
    map(~.x + 
          scale_radius(range = c(0, 4.5), 
                       limits = c(0, 1), 
                       name = 'Accuracy'))
  
# Plots of square errors --------
  
  insert_msg('Plots of Brier square errors')
  
  early_class$square_error_plots <- early_class$predictions %>% 
    map(map, 
        plot, 
        type = 'class_p')

# Cluster prediction stats ------
  
  insert_msg('Cluster prediction stats')
  
  early_class$clust_stats <- early_class$models %>% 
    map(clstats, 
        newdata = class_globals$analysis_tbl$test) %>% 
    map(compress, names_to = 'dataset') %>% 
    compress(names_to = 'method') %>%  
    mutate(clust_id = factor(.outcome, 
                             levels(class_globals$assignment$training$clust_id)), 
           dataset = factor(dataset,
                            c('train', 'cv', 'test')))
  
  ## plotting: Specificity and Sensitivity for particular clusters
  
  early_class$clust_stat_plots <- 
    plot_cluster_stats(data = early_class$clust_stats, 
                       y_var = 'method', 
                       title_prefix = 'Cluster detection', 
                       y_lab = 'Algorithm') %>% 
    map(~.x + 
          scale_y_discrete(labels = class_globals$algo_labs))

# ROC for the PTS cluster --------
  
  insert_msg('ROC stats and plots for the PTS cluster')
  
  early_class$roc_plots[c('pts', 'pts_ptg')] <- 
    list(cluster = c('PTS', 'neutral'), 
         rev_levels = c(FALSE, TRUE), 
         title_prefix = c('PTS cluster', 'PTS + PTG clusters')) %>% 
    pmap(plot_ptb_roc, 
         early_class$predictions, 
         stats = early_class$clust_stats, 
         cutoffs.at = 1, 
         cutoff.labels = '') 
  
# Confusion matrices ------
  
  insert_msg('Plots of confusion matrices')
  
  ## plot subtitles with overall accuracy, kappa and numbers of cases
  
  early_class$confusion_plots[c('caps', 'titles')] <- 
    make_confusion_caps(stats = early_class$overall_stats, 
                        models = early_class$models)

  ## plots
  
  early_class$confusion_plots$plots <- early_class$predictions %>% 
    map(map, 
        plot, 
        type = 'confusion', 
        scale = 'percent', 
        cust_theme = globals$common_theme) %>% 
    map(map, 
        ~.x + 
          scale_fill_gradient2(low = 'steelblue', 
                               mid = 'white', 
                               high = 'firebrick', 
                               midpoint = 16, 
                               limits = c(0, 36), 
                               oob = scales::squish, 
                               name = '% of dataset'))
  
  ## appending with title and statistic values
  
  for(i in names(early_class$confusion_plots$plots)) {
    
    early_class$confusion_plots$plots[[i]] <- 
      list(x = early_class$confusion_plots$plots[[i]], 
           y = early_class$confusion_plots$titles[[i]], 
           z = early_class$confusion_plots$caps[[i]]) %>% 
      pmap(function(x, y, z) x + 
             labs(title = y, 
                  subtitle = z) + 
             theme(plot.tag = element_blank()))
    
  }
  
# Importance stats -------
  
  insert_msg('Variable importance')
  
  ## importance statistic
  
  early_class$importance$stats <- early_class$models %>% 
    get_importance
    
  ## plots: top 20 most important variables
  
  early_class$importance$plots <- early_class$importance$stats %>% 
    plot_importance

# END ------
  
  rm(i)
  
  insert_tail()