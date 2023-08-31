# Tuning of RPart models

# The tuning is directed by values of Cohen's kappa in 10-fold cross-validation

  insert_head()

# container -------

  rpart_tune <- list()

# tune grids ------

  insert_msg('Tune grids')
  
  ## a single tune grid for both predictor sets
  
  rpart_tune$tune_grid <- 
    expand.grid(cp = seq(0, 0.1, by = 0.0025))

# tuning -------

  insert_msg('Tuning')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  rpart_tune$models <- 
    list(form = class_globals$formulas) %>% 
    pmap(train, 
         data = class_globals$analysis_tbl$training, 
         method = 'rpart', 
         metric = 'Kappa', 
         trControl = class_globals$train_control, 
         tuneGrid = rpart_tune$tune_grid)
  
  stopImplicitCluster()

# Tuning model stats -------

  insert_msg('Tuning model stats')
  
  rpart_tune$model_stats <- rpart_tune$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  rpart_tune$best_tunes <- rpart_tune$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)

# Plotting -------

  insert_msg('Plotting')
  
  rpart_tune$tune_plot <- 
    list(x = rpart_tune$model_stats, 
         y = paste('RPart tuning,', 
                   c('full predictor set', 
                     'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = cp, 
                      y = Kappa)) + 
           geom_hline(yintercept = 0, 
                      linetype = 'dashed') + 
           geom_path(color = 'plum4') + 
           geom_point(shape = 16, 
                      size = 2, 
                      color = 'plum4') + 
           scale_size_area(max_size = 4.5) + 
           globals$common_theme + 
           labs(title = y, 
                x = 'Complexity, cp', 
                y = "Cohen's \u03BA"))

# caching the results ------

  insert_msg('Caching the results')
  
  save(rpart_tune, file = './cache/rpart_tune.RData')

# END -----

insert_tail()