# Development of a random forest classifier employing solely 'early' 
# explanatory variables, i.e. features available during an accident 
# and acute medical management

  insert_head()
  
# container ------
  
  pam_early <- list()
  
# parallel backend ------
  
  plan('multisession')

# tuning of the classifier -------
  
  insert_msg('Tuning')
  
  ## finding the optimal mtry parameter based on OOB prediction accuracy
  
  set.seed(1234)
  
  pam_early$tune_grid <- 
    expand.grid(mtry = 2:10, 
                teststat = c('quad', 'max'), 
                testtype = c('Teststatistic'), 
                mincriterion = c(0, 0.95, qnorm(0.9)), 
                minsplit = c(10, 20), 
                stringsAsFactors = FALSE)
  
  pam_early$tuning$controls <- pam_early$tune_grid %>% 
    pmap(cforest_control, 
         ntree = 1000)
  
  pam_early$tuning$models <- pam_early$tuning$controls %>% 
    future_map(~cforest(formula = class_globals$early_formula$pam, 
                        data = class_globals$analysis_tbl$training, 
                        controls = .x), 
               .options = furrr_options(seed = TRUE))
  
  ## predictions and fit stats
  
  pam_early$tuning$predictions <- pam_early$tuning$models %>% 
    map(~data.frame(obs = class_globals$analysis_tbl$training$clust_pam, 
                    pred = predict(.x, OOB = TRUE) %>% 
                      factor(levels = levels(class_globals$analysis_tbl$training$clust_pam))))
  
  pam_early$tuning$fit_stats <- pam_early$tuning$predictions %>% 
    map(multiClassSummary, 
        lev = levels(class_globals$analysis_tbl$training$clust_pam)) %>% 
    map(as.list) %>% 
    map_dfr(as_tibble)
  
  pam_early$tuning$fit_stats <- 
    cbind(pam_early$tuning$fit_stats, 
          pam_early$tune_grid) %>% 
    as_tibble
  
  pam_early$tuning$models <- NULL
  
  pam_early <- compact(pam_early)
  
  plan('sequential')
  
  gc()
  
# Plotting the tuning results ------
  
  insert_msg('Plottng the tuning results')
  
  ## accuracy, kappa, sensitivity and specificity
  
  pam_early$tune_plots <- 
    list(x = c('Accuracy', 'Kappa', 'Mean_Sensitivity', 'Mean_Specificity'), 
         y = paste0(c('Accuracy', 'Kappa', 'Sensitivity', 'Specificity'), 
                    ', training'), 
         w = c('Accuracy', '\u03BA', 'Sensitivity', 'Specificity')) %>% 
    pmap(function(x, y, w) pam_early$tuning$fit_stats %>% 
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
  
  pam_early$model <- 
    model_crf(train_data = class_globals$analysis_tbl$training, 
              test_data = class_globals$analysis_tbl$test, 
              response = 'clust_pam', 
              expl_variables = class_globals$early_variables, 
              controls = cforest_control(teststat = "quad",
                                         testtype = "Teststatistic", 
                                         mtry = 7, 
                                         mincriterion = qnorm(0.9), 
                                         minsplit = 10, 
                                         ntree = 1000))
  
# Fit stats and variable importance -------
  
  insert_msg('Fit stats and variable importance')
  
  pam_early$fit_stats <- pam_early$model$stats %>% 
    mutate(variable = 'cForest classifier', 
           Sensitivity = Mean_Sensitivity, 
           Specificity = Mean_Specificity)
  
  ## variable importance
  
  pam_early$importance <- pam_early$model$model %>%
    varimp(conditional = FALSE) %>% 
    compress(names_to = 'variable', 
             values_to = 'importance')

# Plotting the fit stats ------
  
  insert_msg('Plotting the fit stats')
  
  pam_early$fit_stat_plot <- 
    plot_crf_stats(pam_early$fit_stats)
  
# Plotting the importance ------
  
  insert_msg('Plotting the importance measures')
  
  pam_early$importance_plot <- 
    plot_crf_importance(pam_early$importance, 
                        plot_title = 'Cluster classification', 
                        fill_color = 'cornsilk')

  set.seed(1234)
  
  pam_early$importance_cloud <- pam_early$importance %>% 
    plot_importance_cloud
  
# Plotting the confusion matrices --------
  
  insert_msg('Plotting the confusion matrices')
  
  pam_early$confusion_plots <- pam_early$model %>% 
    plot_confusion
  
# END -----
  
  insert_tail()