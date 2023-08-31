# Tuning of a single hidden-layer neural network/softmax with caret and nnet
# for the full predictor set and early predictors
#
# The tuning is directed by values of Cohen's kappa in 10-fold cross-validation

  insert_head()
  
# container -------
  
  nnet_tune <- list()
  
# tune grids ------
  
  insert_msg('Tune grids')
  
  nnet_tune$tune_grids <- class_globals$variables %>% 
    map(~expand.grid(size = 2:10, 
                     decay = c(0, 1e-4, 1e-3, 1e-2, 1e-1)))
  
# tuning -------
  
  insert_msg('Tuning')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  nnet_tune$models <- 
    list(form = class_globals$formulas, 
         tuneGrid = nnet_tune$tune_grids) %>% 
    pmap(train, 
         data = class_globals$analysis_tbl$training, 
         method = 'nnet', 
         metric = 'Kappa', 
         trControl = class_globals$train_control, 
         maxit = 500)

  stopImplicitCluster()
  
# Tuning model stats -------
  
  insert_msg('Tuning model stats')
  
  nnet_tune$model_stats <- nnet_tune$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  nnet_tune$best_tunes <- nnet_tune$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting the tuning results ------
  
  insert_msg('Plotting')
  
  nnet_tune$tune_plots <- 
    list(x = nnet_tune$model_stats, 
         y = paste('NNet tuning,', 
                   c('full predictor set', 
                     'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = size,
                      y = Kappa, 
                      color = factor(decay))) + 
           facet_grid(decay ~ .) + 
           geom_hline(yintercept = 0, 
                      linetype = 'dashed') + 
           geom_path() + 
           geom_point(shape = 16, 
                      size = 2, 
                      alpha = 0.75) + 
           scale_color_viridis_d(name = 'Decay') + 
           globals$common_theme + 
           labs(title = y, 
                x = 'Size of the hidden layer', 
                y = "Cohen's \u03BA"))
  
# Caching the results -------
  
  insert_msg('Caching the results')
  
  save(nnet_tune, file = './cache/nnet_tune.RData')
  
# END ------
  
  insert_tail()