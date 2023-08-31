# Tuning of conditional Random Forest models of the PAM cluster assignment 
# for the full predictor set and early predictors
#
# The tuning is directed by values of Cohen's kappa in 10-fold cross-validation

  insert_head()
  
# container -------
  
  crf_tune <- list()

# Tune grids and control objects ------
  
  insert_msg('Tune grids and control objects')
  
  crf_tune$tune_grids <- class_globals$variables %>% 
    map(~expand.grid(mtry = seq(2, length(.x), by = 1)))

# Tuning -----
  
  insert_msg('Tuning')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  crf_tune$models <- 
    list(form = class_globals$formulas, 
         tuneGrid = crf_tune$tune_grids) %>% 
    pmap(train, 
         data = class_globals$analysis_tbl$training, 
         method = 'cforest', 
         metric = 'Kappa', 
         trControl = class_globals$train_control, 
         controls = cforest_unbiased(ntree = 1000))
  
  stopImplicitCluster()
  
# Tuning model stats -------
  
  insert_msg('Tuning model stats')
  
  crf_tune$model_stats <- crf_tune$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  crf_tune$best_tunes <- crf_tune$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting the results --------
  
  insert_msg('Plotting the tuning results')
  
  crf_tune$tune_plots <- 
    list(x = crf_tune$model_stats, 
         y = paste('cForest tuning,', 
                   c('full predictor set', 
                     'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = mtry, 
                      y = Kappa)) + 
           geom_hline(yintercept = 0, 
                      linetype = 'dashed') + 
           geom_path(color = 'darkorange3') + 
           geom_point(shape = 16, 
                      size = 2, 
                      alpha = 0.75, 
                      color = 'darkorange3') + 
           globals$common_theme + 
           labs(title = y, 
                x = 'Number of random variables per tree, mtry', 
                y = "Cohen's \u03BA"))

# caching the results -------
  
  insert_msg('Caching the results')
  
  save(crf_tune, file = './cache/crf_tune.RData')
  
# END ------
  
  insert_tail()