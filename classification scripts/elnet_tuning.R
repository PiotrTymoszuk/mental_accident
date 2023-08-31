# Tuning of classical Elastic Net models of the PAM cluster assignment 
# for the full predictor set and early predictors
#
# The tuning is directed by values of Cohen's kappa in 10-fold cross-validation

  insert_head()
  
# container -------
  
  elnet_tune <- list()

# Tune grids and control objects ------
  
  insert_msg('Tune grids and control objects')
  
  elnet_tune$tune_grids <- class_globals$variables %>% 
    map(~expand.grid(alpha = 0.5, 
                     lambda = seq(0.0001, 0.05, by = 0.000001)))

# Tuning -----
  
  insert_msg('Tuning')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  elnet_tune$models <- 
    list(form = class_globals$formulas, 
         tuneGrid = elnet_tune$tune_grids) %>% 
    pmap(train, 
         data = class_globals$analysis_tbl$training, 
         method = 'glmnet', 
         metric = 'Kappa', 
         trControl = class_globals$train_control, 
         family = 'multinomial')
  
  stopImplicitCluster()
  
# Tuning model stats -------
  
  insert_msg('Tuning model stats')
  
  elnet_tune$model_stats <- elnet_tune$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  elnet_tune$best_tunes <- elnet_tune$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting the results --------
  
  insert_msg('Plotting the tuning results')
  
  elnet_tune$tune_plots <- 
    list(x = elnet_tune$model_stats, 
         y = paste('ElasticNet tuning,', 
                   c('full predictor set', 'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = lambda, 
                      y = Kappa)) + 
           geom_path(color = 'orangered3') + 
           geom_point(shape = 16, 
                      color = 'orangered3') + 
           globals$common_theme + 
           labs(title = y, 
                x = 'Shirinkage paramater, \u03BB', 
                y = "Cohen's \u03BA"))

# caching the results -------
  
  insert_msg('Caching the results')
  
  save(elnet_tune, file = './cache/elnet_tune.RData')
  
# END ------
  
  insert_tail()