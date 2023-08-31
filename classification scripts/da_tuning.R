# Tuning of shrinkage discriminant analysis

# The tuning is directed by values of Cohen's kappa in 10-fold cross-validation

  insert_head()

# container -------

  sda_tune <- list()

# tune grids ------

  insert_msg('Tune grids')
  
  ## a single tune grid for both predictor sets
  
  sda_tune$tune_grid <- 
    expand.grid(diagonal = c(TRUE, FALSE), 
                lambda = seq(0.025, 1, by = 0.005))

# tuning -------

  insert_msg('Tuning')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  sda_tune$models <- 
    list(form = class_globals$formulas) %>% 
    pmap(train, 
         data = class_globals$analysis_tbl$training, 
         method = 'sda', 
         metric = 'Kappa', 
         trControl = class_globals$train_control,
         tuneGrid = sda_tune$tune_grid)
  
  stopImplicitCluster()

# Tuning model stats -------

  insert_msg('Tuning model stats')
  
  sda_tune$model_stats <- sda_tune$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  sda_tune$best_tunes <- sda_tune$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)

# Plotting -------

  insert_msg('Plotting')
  
  sda_tune$tune_plot <- 
    list(x = sda_tune$model_stats, 
         y = paste('SDA tuning,', 
                   c('full predictor set', 
                     'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = lambda, 
                      y = Kappa)) + 
           geom_hline(yintercept = 0, 
                      linetype = 'dashed') + 
           geom_path(color = 'brown4') + 
           geom_point(shape = 16, 
                      size = 2, 
                      color = 'brown4') + 
           facet_grid(. ~ diagonal) + 
           scale_size_area(max_size = 4.5) + 
           globals$common_theme + 
           labs(title = y, 
                x = 'Schrinkage, \u03BB', 
                y = "Cohen's \u03BA"))

# caching the results ------

  insert_msg('Caching the results')
  
  save(sda_tune, file = './cache/sda_tune.RData')

# END -----

insert_tail()