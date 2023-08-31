# Tuning of radial kernel support vector machines

# The tuning is directed by values of Cohen's kappa in 10-fold cross-validation

  insert_head()

# container -------

  svm_tune <- list()

# tune grids ------

  insert_msg('Tune grids')

  ## a single tune grid for both predictor sets
  
  svm_tune$tune_grid <- 
    expand.grid(sigma = seq(0.01, 0.1, by = 0.01), 
                C = seq(0.1, 4, by = 0.2))

# tuning -------

  insert_msg('Tuning')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  svm_tune$models <- 
    list(form = class_globals$formulas) %>% 
    pmap(train, 
         data = class_globals$analysis_tbl$training, 
         method = 'svmRadial', 
         metric = 'Kappa', 
         trControl = class_globals$train_control, 
         tuneGrid = svm_tune$tune_grid)

  stopImplicitCluster()
  
# Tuning model stats -------
  
  insert_msg('Tuning model stats')
  
  svm_tune$model_stats <- svm_tune$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  svm_tune$best_tunes <- svm_tune$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting -------
  
  insert_msg('Plotting')
  
  svm_tune$tune_plots <- 
    list(x = svm_tune$model_stats, 
         y = paste('SVM tuning,', 
                   c('full predictor set', 
                     'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = sigma, 
                      y = C, 
                      fill = Kappa, 
                      size = Kappa)) + 
           geom_point(shape = 21) + 
           scale_size_area(max_size = 4.5) + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'white', 
                                high = 'firebrick', 
                                name = "Cohen's \u03BA") + 
           globals$common_theme + 
           labs(title = y, 
                x = "Gaussian kernel's \u03C3", 
                y = 'Cost, C'))
  
# caching the results ------
  
  insert_msg('Caching the results')
  
  save(svm_tune, file = './cache/svm_tune.RData')
  
# END -----
  
  insert_tail()