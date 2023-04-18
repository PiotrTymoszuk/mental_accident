# Development of a random forest classifier employing solely 'early' 
# explanatory variables, i.e. features available during an accident 
# and acute medical management

  insert_head()
  
# container ------
  
  mix_early <- list()
  
# parallel backend ------
  
  plan('multisession')

# tuning of the classifier -------
  
  insert_msg('Tuning')
  
  ## finding the optimal mtry parameter based on OOB prediction accuracy
  
  set.seed(1234)
  
  mix_early$tune_grid <- 
    expand.grid(mtry = 2:10, 
                teststat = c('quad', 'max'), 
                testtype = c('Teststatistic'), 
                mincriterion = c(0, 0.95, qnorm(0.9)), 
                minsplit = c(10, 20), 
                stringsAsFactors = FALSE)
  
  mix_early$tuning$controls <- mix_early$tune_grid %>% 
    pmap(cforest_control, 
         ntree = 1000)
  
  mix_early$tuning$models <- mix_early$tuning$controls %>% 
    future_map(~cforest(formula = class_globals$early_formula$mix, 
                        data = class_globals$analysis_tbl$training, 
                        controls = .x), 
               .options = furrr_options(seed = TRUE))
  
  ## predictions and fit stats
  
  mix_early$tuning$predictions <- mix_early$tuning$models %>% 
    map(~data.frame(obs = class_globals$analysis_tbl$training$clust_mix, 
                    pred = predict(.x, OOB = TRUE) %>% 
                      factor(levels = levels(class_globals$analysis_tbl$training$clust_mix))))
  
  mix_early$tuning$fit_stats <- mix_early$tuning$predictions %>% 
    map(multiClassSummary, 
        lev = levels(class_globals$analysis_tbl$training$clust_mix)) %>% 
    map(as.list) %>% 
    map_dfr(as_tibble)
  
  mix_early$tuning$fit_stats <- 
    cbind(mix_early$tuning$fit_stats, 
          mix_early$tune_grid) %>% 
    as_tibble
  
  mix_early$tuning$models <- NULL
  
  mix_early <- compact(mix_early)
  
  plan('sequential')
  
  gc()
  
# Plotting the tuning results ------
  
  insert_msg('Plottng the tuning results')
  
  ## accuracy, kappa, sensitivity and specificity
  
  mix_early$tune_plots <- 
    list(x = c('Accuracy', 'Kappa', 'Mean_Sensitivity', 'Mean_Specificity'), 
         y = paste0(c('Accuracy', 'Kappa', 'Sensitivity', 'Specificity'), 
                    ', training'), 
         w = c('Accuracy', '\u03BA', 'Sensitivity', 'Specificity')) %>% 
    pmap(function(x, y, w) mix_early$tuning$fit_stats %>% 
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
  
  mix_early$model <- 
    model_crf(train_data = class_globals$analysis_tbl$training, 
              test_data = class_globals$analysis_tbl$test, 
              response = 'clust_mix', 
              expl_variables = class_globals$early_variables, 
              controls = cforest_control(teststat = "max",
                                         testtype = "Teststatistic", 
                                         mtry = 3, 
                                         mincriterion = 0, 
                                         minsplit = 10, 
                                         ntree = 1000))
  
# Fit stats and variable importance -------
  
  insert_msg('Fit stats and variable importance')
  
  mix_early$fit_stats <- mix_early$model$stats %>% 
    mutate(variable = 'cForest classifier', 
           Sensitivity = Mean_Sensitivity, 
           Specificity = Mean_Specificity)
  
  ## variable importance
  
  mix_early$importance <- mix_early$model$model %>%
    varimp(conditional = FALSE) %>% 
    compress(names_to = 'variable', 
             values_to = 'importance')

# Plotting the fit stats ------
  
  insert_msg('Plotting the fit stats')
  
  mix_early$fit_stat_plot <- 
    plot_crf_stats(mix_early$fit_stats)
  
# Plotting the importance ------
  
  insert_msg('Plotting the importance measures')
  
  mix_early$importance_plot <- 
    plot_crf_importance(mix_early$importance, 
                        plot_title = 'Cluster classification', 
                        fill_color = 'cornsilk')

  set.seed(1234)
  
  mix_early$importance_cloud <- mix_early$importance %>% 
    plot_importance_cloud
  
# Plotting the confusion matrices --------
  
  insert_msg('Plotting the confusion matrices')
  
  mix_early$confusion_plots <- mix_early$model %>% 
    plot_confusion
  
# END -----
  
  insert_tail()