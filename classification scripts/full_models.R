# Modeling of the mental health cluster assignment with the full 
# set of predictors

  insert_head()
  
# container -------
  
  full_class <- list()
  
# Building the models -------
  
  insert_msg('Building the models')
  
  ## I'm actually taking over the models tuned and trained previously
  
  full_class$models <- 
    list(ranger = ranger_tune, 
         nnet = nnet_tune, 
         svmRadial = svm_tune, 
         rpart = rpart_tune, 
         sda = sda_tune, 
         cforest = crf_tune, 
         elnet = elnet_tune) %>% 
    map(~.x$models$full) %>% 
    map(as_caretx)
  
# Predictions ------
  
  insert_msg('Predictions')
  
  full_class$predictions <- full_class$models %>% 
    map(predict, newdata = class_globals$analysis_tbl$test)
  
# Overall performance stats -------
  
  insert_msg('Overall model performance stats')

  full_class$overall_stats <- full_class$models %>% 
    map(summary, 
        newdata = class_globals$analysis_tbl$test, 
        wide = TRUE) %>% 
    map(compress, names_to = 'dataset') %>% 
    compress(names_to = 'method') %>% 
    mutate(dataset = factor(dataset, c('train', 'cv', 'test')))
  
# Plot of the model performance stats ------- 
  
  insert_msg('Plots of the overall performance stats')
  
  ## plots of kappa, accuracy, Brier score and Brier's skill score
  
  full_class$overall_stat_plots <- 
    plot_overall_stats(data = full_class$overall_stats, 
                       y_var = 'method', 
                       title_prefix = 'Mental cluster prediction') %>% 
    map(~.x + 
          scale_y_discrete(labels = class_globals$algo_labs))
  
  ## Kappa vs Brier score
  
  full_class$kappa_bs_plots <- 
    plot_kappa_bs(full_class$overall_stats) %>% 
    map(~.x + 
          scale_size_area(max_size = 4.5, 
                          limits = c(0, 1), 
                          name = 'Accuracy'))
  
# Plots of square errors --------
  
  insert_msg('Plots of Brier square errors')
  
  full_class$square_error_plots <- full_class$predictions %>% 
    map(map, 
        plot,
        type = 'class_p')

# Cluster prediction stats ------
  
  insert_msg('Cluster prediction stats')
  
  full_class$clust_stats <- full_class$models %>% 
    map(clstats, 
        newdata = class_globals$analysis_tbl$test) %>% 
    map(compress, names_to = 'dataset') %>% 
    compress(names_to = 'method') %>%  
    mutate(clust_id = factor(.outcome, 
                             levels(class_globals$assignment$training$clust_id)), 
           dataset = factor(dataset,
                            c('train', 'cv', 'test')))
  
  ## plotting: Specificity and Sensitivity for particular clusters
  
  full_class$clust_stat_plots <- 
    plot_cluster_stats(data = full_class$clust_stats, 
                       y_var = 'method', 
                       title_prefix = 'Cluster detection', 
                       y_lab = 'Algorithm') %>% 
    map(~.x + 
          scale_y_discrete(labels = class_globals$algo_labs))

# ROC for the PTS and neutral cluster --------
  
  insert_msg('ROC stats and plots for the PTB cluster')
  
  full_class$roc_plots[c('pts', 'pts_ptg')] <- 
    list(cluster = c('PTS', 'neutral'), 
         rev_levels = c(FALSE, TRUE), 
         title_prefix = c('PTS cluster', 'PTS + PTG clusters')) %>% 
    pmap(plot_ptb_roc, 
         full_class$predictions, 
         stats = full_class$clust_stats, 
         cutoffs.at = 1, 
         cutoff.labels = '')

# Confusion matrices ------
  
  insert_msg('Plots of confusion matrices')
  
  ## plot subtitles with overall accuracy, kappa and numbers of cases
  
  full_class$confusion_plots[c('caps', 'titles')] <- 
    make_confusion_caps(stats = full_class$overall_stats, 
                        models = full_class$models)

  ## plots
  
  full_class$confusion_plots$plots <- full_class$predictions %>% 
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
  
  for(i in names(full_class$confusion_plots$plots)) {
    
    full_class$confusion_plots$plots[[i]] <- 
      list(x = full_class$confusion_plots$plots[[i]], 
           y = full_class$confusion_plots$titles[[i]], 
           z = full_class$confusion_plots$caps[[i]]) %>% 
      pmap(function(x, y, z) x + 
             labs(title = y, 
                  subtitle = z) + 
             theme(plot.tag = element_blank()))
    
  }
  
# Importance stats -------
  
  insert_msg('Variable importance')
  
  ## importance statistic
  
  full_class$importance$stats <- full_class$models %>% 
    get_importance
    
  ## plots: top 20 most important variables
  
  full_class$importance$plots <- full_class$importance$stats %>% 
    plot_importance

# END ------
  
  rm(i)
  
  insert_tail()