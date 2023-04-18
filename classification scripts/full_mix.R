# Development of a random forest classifier employing solely 'early' 
# explanatory variables, i.e. features available during an accident 
# and acute medical management

  insert_head()
  
# container ------
  
  mix_full <- list()
  
# parallel backend ------
  
  plan('multisession')

# tuning of the classifier -------
  
  insert_msg('Tuning')
  
  ## finding the optimal mtry parameter based on OOB prediction accuracy
  
  set.seed(1234)
  
  mix_full$tune_grid <- 
    expand.grid(mtry = 2:10, 
                teststat = c('quad', 'max'), 
                testtype = c('Teststatistic'), 
                mincriterion = c(0, 0.95, qnorm(0.9)), 
                minsplit = c(10, 20), 
                stringsAsFactors = FALSE)
  
  mix_full$tuning$controls <- mix_full$tune_grid %>% 
    pmap(cforest_control, 
         ntree = 1000)
  
  mix_full$tuning$models <- mix_full$tuning$controls %>% 
    future_map(~cforest(formula = class_globals$formula$mix, 
                        data = class_globals$analysis_tbl$training, 
                        controls = .x), 
               .options = furrr_options(seed = TRUE))
  
  ## predictions and fit stats
  
  mix_full$tuning$predictions <- mix_full$tuning$models %>% 
    map(~data.frame(obs = class_globals$analysis_tbl$training$clust_mix, 
                    pred = predict(.x, OOB = TRUE) %>% 
                      factor(levels = levels(class_globals$analysis_tbl$training$clust_mix))))
  
  mix_full$tuning$fit_stats <- mix_full$tuning$predictions %>% 
    map(multiClassSummary, 
        lev = levels(class_globals$analysis_tbl$training$clust_mix)) %>% 
    map(as.list) %>% 
    map_dfr(as_tibble)
  
  mix_full$tuning$fit_stats <- 
    cbind(mix_full$tuning$fit_stats, 
          mix_full$tune_grid) %>% 
    as_tibble
  
  mix_full$tuning$models <- NULL
  
  mix_full <- compact(mix_full)
  
  plan('sequential')
  
  gc()
  
# Plotting the tuning results ------
  
  insert_msg('Plottng the tuning results')
  
  ## accuracy, kappa, sensitivity and specificity
  
  mix_full$tune_plots <- 
    list(x = c('Accuracy', 'Kappa', 'Mean_Sensitivity', 'Mean_Specificity'), 
         y = paste0(c('Accuracy', 'Kappa', 'Sensitivity', 'Specificity'), 
                    ', training'), 
         w = c('Accuracy', '\u03BA', 'Sensitivity', 'Specificity')) %>% 
    pmap(function(x, y, w) mix_full$tuning$fit_stats %>% 
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
  
  mix_full$model <- 
    model_crf(train_data = class_globals$analysis_tbl$training, 
              test_data = class_globals$analysis_tbl$test, 
              response = 'clust_mix', 
              expl_variables = class_globals$variables, 
              controls = cforest_control(teststat = "quad",
                                         testtype = "Teststatistic", 
                                         mtry = 3, 
                                         mincriterion = qnorm(0.9), 
                                         minsplit = 10, 
                                         ntree = 1000))
  
# Fit stats and variable importance -------
  
  insert_msg('Fit stats and variable importance')
  
  mix_full$fit_stats <- mix_full$model$stats %>% 
    mutate(variable = 'cForest classifier', 
           Sensitivity = Mean_Sensitivity, 
           Specificity = Mean_Specificity)
  
  ## variable importance
  
  mix_full$importance <- mix_full$model$model %>%
    varimp(conditional = FALSE) %>% 
    compress(names_to = 'variable', 
             values_to = 'importance')

# Plotting the fit stats ------
  
  insert_msg('Plotting the fit stats')
  
  mix_full$fit_stat_plot <- 
    plot_crf_stats(mix_full$fit_stats)
  
# Plotting the importance ------
  
  insert_msg('Plotting the importance measures')
  
  mix_full$importance_plot <- 
    plot_crf_importance(mix_full$importance, 
                        plot_title = 'Cluster classification', 
                        fill_color = 'cornsilk')

  set.seed(1234)
  
  mix_full$importance_cloud <- mix_full$importance %>% 
    plot_importance_cloud
  
# Plotting the confusion matrices --------
  
  insert_msg('Plotting the confusion matrices')
  
  mix_full$confusion_plots <- mix_full$model %>% 
    plot_confusion
  
# END -----
  
  insert_tail()