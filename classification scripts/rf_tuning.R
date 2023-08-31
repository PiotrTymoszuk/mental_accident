# Tuning of classical Random Forest models of the PAM cluster assignment 
# for the full predictor set and early predictors
#
# The tuning is directed by values of Cohen's kappa in 10-fold cross-validation

  insert_head()
  
# container -------
  
  ranger_tune <- list()

# Tune grids and control objects ------
  
  insert_msg('Tune grids and control objects')
  
  ranger_tune$tune_grids <- class_globals$variables %>% 
    map(~expand.grid(mtry = 2:length(.x), 
                     splitrule = c('extratrees', 'gini'), 
                     min.node.size = c(1, 3, 5)))

# Tuning -----
  
  insert_msg('Tuning')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  ranger_tune$models <- 
    list(form = class_globals$formulas, 
         tuneGrid = ranger_tune$tune_grids) %>% 
    pmap(train, 
         data = class_globals$analysis_tbl$training, 
         method = 'ranger', 
         metric = 'Kappa', 
         trControl = class_globals$train_control, 
         num.trees = 1000, 
         importance = 'permutation')
  
  stopImplicitCluster()
  
# Tuning model stats -------
  
  insert_msg('Tuning model stats')
  
  ranger_tune$model_stats <- ranger_tune$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  ranger_tune$best_tunes <- ranger_tune$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting the results --------
  
  insert_msg('Plotting the tuning results')
  
  ranger_tune$tune_plots <- 
    list(x = ranger_tune$model_stats, 
         y = paste('RF tuning,', 
                   c('full predictor set', 
                     'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = mtry, 
                      y = Kappa, 
                      color = splitrule)) + 
           facet_grid(splitrule ~ min.node.size) + 
           geom_hline(yintercept = 0, 
                      linetype = 'dashed') + 
           geom_path() + 
           geom_point(shape = 16, 
                      size = 2, 
                      alpha = 0.75) + 
           scale_color_manual(values = c(extratrees = 'coral3', 
                                         gini = 'steelblue'), 
                              name = 'Splitting rule') + 
           globals$common_theme + 
           labs(title = y, 
                x = 'Number of random variables per tree, mtry', 
                y = "Cohen's \u03BA"))

# caching the results -------
  
  insert_msg('Caching the results')
  
  save(ranger_tune, file = './cache/ranger_tune.RData')
  
# END ------
  
  insert_tail()