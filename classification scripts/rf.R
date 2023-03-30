# Multi-parameter RF classifier with the top factors for cluster classification
# identified by the OneR procedure

  insert_head()
  
# container ------
  
  class_rf <- list()

# parallel backend ------
  
  plan('multisession')
  
# analysis globals -------
  
  insert_msg('Analysis globals')

  ## tables of assignments for single clusters
  
  class_rf$clust_tbl <- 
    list(neutral = class_globals$analysis_tbl, 
         PTG = class_globals$analysis_tbl, 
         PTB = class_globals$analysis_tbl) %>% 
    map2(., names(.), 
         function(data, clust) data %>% 
           map(mutate, 
               clust_id = ifelse(clust_id == clust, clust, 'rest'), 
               clust_id = factor(clust_id)))

# tuning of the classifier -------
  
  insert_msg('Tuning')
  
  ## finding the optimal mtry parameter based on OOB prediction accuracy

  set.seed(1234)
  
  class_rf$tune_grid <- 
    expand.grid(mtry = 2:10, 
                teststat = c('quad', 'max'), 
                testtype = c('Teststatistic'), 
                mincriterion = c(0, 0.95, qnorm(0.9)), 
                minsplit = c(10, 20), 
                stringsAsFactors = FALSE)

  class_rf$tuning$controls <- class_rf$tune_grid %>% 
    pmap(cforest_control, 
         ntree = 1000)

  class_rf$tuning$models <- class_rf$tuning$controls %>% 
    future_map(~cforest(formula = class_globals$formula, 
                 data = class_globals$analysis_tbl$training, 
                 controls = .x), 
               .options = furrr_options(seed = TRUE))
  
  ## predictions and fit stats
  
  class_rf$tuning$predictions <- class_rf$tuning$models %>% 
    map(~data.frame(obs = class_globals$analysis_tbl$training$clust_id, 
                    pred = predict(.x, OOB = TRUE) %>% 
                      factor(levels = levels(class_globals$analysis_tbl$training$clust_id))))
  
  class_rf$tuning$fit_stats <- class_rf$tuning$predictions %>% 
    map(multiClassSummary, 
        lev = levels(class_globals$analysis_tbl$training$clust_id)) %>% 
    map(as.list) %>% 
    map_dfr(as_tibble)
  
  class_rf$tuning$fit_stats <- 
    cbind(class_rf$tuning$fit_stats, 
          class_rf$tune_grid) %>% 
    as_tibble
  
  class_rf$tuning$models <- NULL
  
  class_rf <- compact(class_rf)
  
  plan('sequential')
  
  gc()
  
# Plotting the tuning results ------
  
  insert_msg('Plottng the tuning results')
  
  ## accuracy, kappa, sensitivity and specificity
  
  class_rf$tune_plots <- 
    list(x = c('Accuracy', 'Kappa', 'Mean_Sensitivity', 'Mean_Specificity'), 
         y = paste0(c('Accuracy', 'Kappa', 'Sensitivity', 'Specificity'), 
                    ', training'), 
         w = c('Accuracy', '\u03BA', 'Sensitivity', 'Specificity')) %>% 
    pmap(function(x, y, w) class_rf$tuning$fit_stats %>% 
           ggplot(aes(x = mtry, 1:10, 
                      y = .data[[x]], 
                      color = teststat))  +
           facet_grid(minsplit ~ mincriterion)  +
           geom_line() + 
           expand_limits(y = 0) +
           scale_x_continuous(breaks = 1:12) + 
           globals$common_theme + 
           labs(title = y, 
                subtitle = paste('OOB predictions, n =', 
                                 nrow(clust_globals$analysis_tbl$training)), 
                x = 'mtry', 
                y = w)) %>% 
    set_names(c('Accuracy', 'Kappa', 'Mean_Sensitivity', 'Mean_Specificity'))

# training of the classifiers -------
  
  insert_msg('Training of the classifiers')
  
  set.seed(1234)
  
  class_rf$models <- 
    list(train_data = c(list(global = class_globals$analysis_tbl$training), 
                        map(class_rf$clust_tbl, ~.x$training)), 
         test_data = c(list(global = class_globals$analysis_tbl$test), 
                       map(class_rf$clust_tbl, ~.x$test))) %>% 
    pmap(model_crf, 
         response = 'clust_id', 
         expl_variables = class_globals$variables, 
         controls = cforest_control(teststat = "max",
                                    testtype = "Teststatistic", 
                                    mtry = 6, 
                                    mincriterion = 0, 
                                    minsplit = 20, 
                                    ntree = 1000))
  
# Fit stats and variable importance -------
  
  insert_msg('Fit stats and variable importance')
  
  class_rf$fit_stats <- class_rf$models %>% 
    map(~.x$stats) %>% 
    map(mutate, 
        variable = 'cForest classifier')
  
  class_rf$fit_stats$global <- 
    class_rf$fit_stats$global %>% 
    mutate(Sensitivity = Mean_Sensitivity, 
           Specificity = Mean_Specificity)
  
  ## variable importance
  
  class_rf$importance <- class_rf$models %>% 
    map(~.x$model) %>% 
    map(varimp, 
        conditional = FALSE) %>% 
    map(compress, 
        names_to = 'variable', 
        values_to = 'importance')
  
# Plotting the fit stats ------
  
  insert_msg('Plotting the fit stats')
  
  class_rf$fit_stat_plots <- 
    list(fit_stats = class_rf$fit_stats) %>% 
    pmap(plot_crf_stats)

# Plotting the importance ------
  
  insert_msg('Plotting the importance measures')
  
  class_rf$importance_plots <- 
    list(x = class_rf$importance, 
         y = paste0(c('Cluster classification', 
                      'Neutral cluster', 
                      'PTG cluster', 
                      'PTGI cluster'), 
                    ', variable importance'), 
         z = c('cornsilk', globals$clust_colors)) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = importance, 
                      y = reorder(variable, importance))) + 
           geom_bar(stat = 'identity', 
                    color = 'gray20', 
                    fill = z) + 
           scale_y_discrete(labels = exchange(class_globals$variables, 
                                              dict = ptsd$var_lexicon, 
                                              key = 'variable', 
                                              value = 'label')) +
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = '\u0394 accuracy'))
  
# Wordclouds with the factor importance ------
  
  insert_msg('Importance wordclouds')
  
  set.seed(1234)
  
  class_rf$importance_clouds <- class_rf$importance %>% 
    map(plot_importance_cloud)

# Plotting the confusion matrices --------
  
  insert_msg('Plotting the confusion matrices')
  
  class_rf$confusion_plots <- class_rf$models %>% 
    map(plot_confusion)

# END -----
  
  insert_tail()