# Development of a random forest classifier employing solely 'early' 
# explanatory variables, i.e. features available during an accident 
# and acute medical management

  insert_head()
  
# container ------
  
  class_early <- list()
  
# parallel backend ------
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## tables of assignments for single clusters
  
  class_early$clust_tbl <- 
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
  
  class_early$tune_grid <- 
    expand.grid(mtry = 2:10, 
                teststat = c('quad', 'max'), 
                testtype = c('Teststatistic'), 
                mincriterion = c(0, 0.95, qnorm(0.9)), 
                minsplit = c(10, 20), 
                stringsAsFactors = FALSE)
  
  class_early$tuning$controls <- class_early$tune_grid %>% 
    pmap(cforest_control, 
         ntree = 1000)
  
  class_early$tuning$models <- class_early$tuning$controls %>% 
    future_map(~cforest(formula = class_globals$early_formula, 
                        data = class_globals$analysis_tbl$training, 
                        controls = .x), 
               .options = furrr_options(seed = TRUE))
  
  ## predictions and fit stats
  
  class_early$tuning$predictions <- class_early$tuning$models %>% 
    map(~data.frame(obs = class_globals$analysis_tbl$training$clust_id, 
                    pred = predict(.x, OOB = TRUE) %>% 
                      factor(levels = levels(class_globals$analysis_tbl$training$clust_id))))
  
  class_early$tuning$fit_stats <- class_early$tuning$predictions %>% 
    map(multiClassSummary, 
        lev = levels(class_globals$analysis_tbl$training$clust_id)) %>% 
    map(as.list) %>% 
    map_dfr(as_tibble)
  
  class_early$tuning$fit_stats <- 
    cbind(class_early$tuning$fit_stats, 
          class_early$tune_grid) %>% 
    as_tibble
  
  class_early$tuning$models <- NULL
  
  class_early <- compact(class_early)
  
  plan('sequential')
  
  gc()
  
# Plotting the tuning results ------
  
  insert_msg('Plottng the tuning results')
  
  ## accuracy, kappa, sensitivity and specificity
  
  class_early$tune_plots <- 
    list(x = c('Accuracy', 'Kappa', 'Mean_Sensitivity', 'Mean_Specificity'), 
         y = paste0(c('Accuracy', 'Kappa', 'Sensitivity', 'Specificity'), 
                    ', training'), 
         w = c('Accuracy', '\u03BA', 'Sensitivity', 'Specificity')) %>% 
    pmap(function(x, y, w) class_early$tuning$fit_stats %>% 
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
  
  class_early$models <- 
    list(train_data = c(list(global = class_globals$analysis_tbl$training), 
                        map(class_early$clust_tbl, ~.x$training)), 
         test_data = c(list(global = class_globals$analysis_tbl$test), 
                       map(class_early$clust_tbl, ~.x$test))) %>% 
    pmap(model_crf, 
         response = 'clust_id', 
         expl_variables = class_globals$early_variables, 
         controls = cforest_control(teststat = "quad",
                                    testtype = "Teststatistic", 
                                    mtry = 7, 
                                    mincriterion = qnorm(0.9), 
                                    minsplit = 10, 
                                    ntree = 1000))
  
# Fit stats and variable importance -------
  
  insert_msg('Fit stats and variable importance')
  
  class_early$fit_stats <- class_early$models %>% 
    map(~.x$stats) %>% 
    map(mutate, 
        variable = 'cForest classifier')
  
  class_early$fit_stats$global <- 
    class_early$fit_stats$global %>% 
    mutate(Sensitivity = Mean_Sensitivity, 
           Specificity = Mean_Specificity)
  
  ## variable importance
  
  class_early$importance <- class_early$models %>% 
    map(~.x$model) %>% 
    map(varimp, 
        conditional = FALSE) %>% 
    map(compress, 
        names_to = 'variable', 
        values_to = 'importance')
  
# Plotting the fit stats ------
  
  insert_msg('Plotting the fit stats')
  
  class_early$fit_stat_plots <- 
    list(fit_stats = class_early$fit_stats) %>% 
    pmap(plot_crf_stats)
  
# Plotting the importance ------
  
  insert_msg('Plotting the importance measures')
  
  class_early$importance_plots <- 
    list(x = class_early$importance, 
         y = paste0(c('Cluster classification', 
                      'Neutral cluster', 
                      'PTG cluster', 
                      'PTB cluster'), 
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
  
  class_early$importance_clouds <- class_early$importance %>% 
    map(plot_importance_cloud)
  
# Plotting the confusion matrices --------
  
  insert_msg('Plotting the confusion matrices')
  
  class_early$confusion_plots <- class_early$models %>% 
    map(plot_confusion)
  
# END -----
  
  insert_tail()