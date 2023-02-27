# Analysis of classification capability of single factors

  insert_head()
  
# container ------
  
  class_one <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## analysis tables for single clusters
  
  class_one$clust_tbl <- 
    list(neutral = class_globals$analysis_tbl, 
         PTG = class_globals$analysis_tbl, 
         PTS = class_globals$analysis_tbl) %>% 
    map2(., names(.), 
         function(data, clust) data %>% 
           map(mutate, 
               clust_id = ifelse(clust_id == clust, clust, 'rest'), 
               clust_id = factor(clust_id)))

# univariable modeling ------
  
  insert_msg('Univariable modeling')
  
  ## global classification
  
  class_one$global_models <- class_globals$variables %>% 
    map(model_one, 
        train_data = class_globals$analysis_tbl$training, 
        test_data = class_globals$analysis_tbl$test, 
        response = 'clust_id') %>% 
    set_names(class_globals$variables)
  
  ## neutral cluster
  
  class_one$neutral_models <- class_globals$variables %>% 
    map(model_one, 
        train_data = class_one$clust_tbl$neutral$training, 
        test_data = class_one$clust_tbl$neutral$test, 
        response = 'clust_id') %>% 
    set_names(class_globals$variables)
  
  ## PTG cluster
  
  class_one$PTG_models <- class_globals$variables %>% 
    map(model_one, 
        train_data = class_one$clust_tbl$PTG$training, 
        test_data = class_one$clust_tbl$PTG$test, 
        response = 'clust_id') %>% 
    set_names(class_globals$variables)
  
  ## PTS cluster
  
  class_one$PTS_models <- class_globals$variables %>% 
    map(model_one, 
        train_data = class_one$clust_tbl$PTS$training, 
        test_data = class_one$clust_tbl$PTS$test, 
        response = 'clust_id') %>% 
    set_names(class_globals$variables)
  
# model summaries -------
  
  insert_msg('Model summaries')
  
  class_one$fit_stats[c('global', 
                        'neutral', 
                        'PTG', 
                        'PTS')] <- class_one[c("global_models", 
                                                "neutral_models", 
                                                "PTG_models", 
                                                "PTS_models")] %>% 
    map(~map(.x, ~.x$stats)) %>% 
    map(compress, names_to = 'variable')
  
  class_one$fit_stats$global <- 
    class_one$fit_stats$global %>% 
    mutate(Sensitivity = Mean_Sensitivity, 
           Specificity = Mean_Specificity)
  
# Top explanatory factors ------
  
  insert_msg('Top explanatory factors')
  
  ## non-zero kappa in both the training and test subsets
  
  class_one$top_factors <- class_one$fit_stats %>% 
    map(dlply, 'partition', filter, Kappa > 0) %>% 
    map(~map(.x, ~.x$variable)) %>% 
    map(reduce, intersect)
  
# Plotting the fit stats for the top explanatory factors -------
  
  insert_msg('Plotting fit stats for the top factors')
  
  class_one$plots <- 
    list(fit_stats = class_one$fit_stats, 
         factors = class_one$top_factors) %>% 
    pmap(plot_one_factors)
  
# Wordclouds with the kappas ------
  
  insert_msg('Kappa wordclouds')
  
  class_one$kappa_clouds$training <- 
    class_one$fit_stats %>% 
    map(filter, 
        partition == 'training', 
        Kappa > 0) %>% 
    map(plot_importance_cloud, 
        importance_var = 'Kappa')
  
  class_one$kappa_clouds$test <- 
    class_one$fit_stats %>% 
    map(filter, 
        partition == 'test', 
        Kappa > 0) %>% 
    map(plot_importance_cloud, 
        importance_var = 'Kappa')

# END -----
  
  insert_tail()