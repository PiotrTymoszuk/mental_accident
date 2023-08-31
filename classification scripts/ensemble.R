# Building stacked model ensembles.
# I'm building them with the Ranger, SVM, SDA, and cForest models
# RPart has the poorest performance in the test cohort!
#
# The explanatory variables for the ensembles are probabilities of 

  insert_head()
  
# container ------
  
  ens_class <- list()
  
# model lists and re-samples -------
  
  insert_msg('Model lists and resamples')
  
  ens_class$model_lst <- 
    list(full = full_class$models[c("svmRadial", "cforest", "ranger", "sda")], 
         early = early_class$models[c("svmRadial", "cforest", "ranger", "sda")])
  
  ens_class$resamples <- ens_class$model_lst %>% 
    map(resamples)
  
# correlations of the re-samples ------
  
  insert_msg('Correlation')
  
  ens_class$model_correlations$data <- 
    ens_class$resamples %>% 
    map(modelCor)
  
  ## correlogram
  
  ens_class$model_correlations$plot_tbl <- 
    ens_class$model_correlations$data %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'model_1') %>% 
    map(pivot_longer, 
        cols = all_of(names(ens_class$model_lst[[1]])), 
        names_to = 'model_2', 
        values_to = 'corr') %>% 
    map(mutate, 
        model_1 = factor(model_1, names(ens_class$model_lst[[1]])), 
        model_2 = factor(model_2, names(ens_class$model_lst[[1]])))
  
  ens_class$model_correlations$plots <- 
    list(x = ens_class$model_correlations$plot_tbl, 
         y = paste('Model outcome correlation,', 
                   c('full predictor set', 'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = model_1, 
                      y = model_2, 
                      fill = corr, 
                      size = abs(corr))) + 
           geom_point(shape = 21,
                      color = 'black') +
           geom_text(aes(label = signif(corr, 2)),
                     size = 2.75, 
                     hjust = 0.5, 
                     vjust = -1.6) +
           scale_x_discrete(labels = class_globals$algo_labs) + 
           scale_y_discrete(labels = class_globals$algo_labs) + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'white', 
                                high = 'firebrick', 
                                midpoint = 0, 
                                limits = c(-1, 1), 
                                name = 'r') + 
           scale_size_area(max_size = 4.5, 
                           limits = c(0, 1)) + 
           guides(size = 'legend', 
                  fill = 'legend') + 
           globals$common_theme + 
           theme(axis.title = element_blank()) + 
           labs(title = y))
  
# Analysis data frames for fitting the ensembles -------
  
  insert_msg('Training and test predictions')
  
  ## they will be used straight away as modeling data frames
  
  ens_class$analysis_tbl$training <- ens_class$model_lst %>% 
    map(predict, 
        newdata = class_globals$analysis_tbl$training) %>% 
    map(map, ~.x$cv$data[c('neutral', 'PTG', 'PTS')])
  
  ens_class$analysis_tbl$test <- ens_class$model_lst %>% 
    map(predict, 
        newdata = class_globals$analysis_tbl$test) %>% 
    map(map, ~.x$test$data[c('neutral', 'PTG', 'PTS')])

  ## joining and re-naming the variables
  ## appending with the cluster assignment
  
  for(i in names(ens_class$analysis_tbl)) {
    
    ens_class$analysis_tbl[[i]] <- ens_class$analysis_tbl[[i]] %>% 
      map(~map2(.x, names(.x), 
                ~set_names(.x, paste(names(.x), .y, sep = '_')))) %>% 
      map(reduce, cbind) %>% 
      map(as_tibble)
    
    ens_class$analysis_tbl[[i]] <- 
      ens_class$analysis_tbl[[i]] %>% 
      map(mutate, 
          clust_id = class_globals$analysis_tbl[[i]]$clust_id)
    
  }
  
# Training the ensemble models ------
  
  insert_msg('Training the ensemble models')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  ens_class$models <- 
    list(data = ens_class$analysis_tbl$training) %>% 
    pmap(caret:::train.formula, 
         form = clust_id ~ ., 
         method = 'cforest',
         metric = 'Kappa', 
         tuneGrid = expand.grid(mtry = 2:10), 
         trControl = class_globals$train_control, 
         controls = cforest_unbiased(ntree = 500)) %>% 
    map(as_caretx)
  
  stopImplicitCluster()
  
# Plots of the tuning process ------
  
  insert_msg('Plots of the tuning process')
  
  ens_class$tune_stats <- ens_class$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  ens_class$tune_stat_plots <- 
    list(x = ens_class$tune_stats, 
         y = paste('Tuning of ensemble models,', 
                   c('full predictor set', 
                     'early predictors'))) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = mtry, 
                      y = Kappa)) + 
           geom_path(color = class_globals$algo_colors["ensemble"]) + 
           geom_point(size = 2, 
                      shape = 16) + 
           globals$common_theme + 
           labs(title = y, 
                x = 'Number of random variables, mtry', 
                y = "Cohen's kappa"))
  
# Predictions --------
  
  insert_msg('Predictions')
  
  ens_class$predictions <- 
    list(object = ens_class$models, 
         newdata = ens_class$analysis_tbl$test) %>% 
    pmap(predict)
  
# Fit stat summary ------
  
  insert_msg('Overall fit stat summary')
  
  ## square errors and Brier scores, the reference 
  ## is a purely random classifier
  
  set.seed(12345)
  
  ens_class$brier <- ens_class$predictions %>% 
    map(class_brier, return_ref = TRUE)
  
  ## overall performance stats
  
  ens_class$overall_stats <- ens_class$predictions %>% 
    map(map, summary) %>% 
    map(map, select, statistic, estimate) %>% 
    map(map, column_to_rownames, 'statistic') %>% 
    map(map, t) %>% 
    map(map, as.data.frame) %>% 
    map(compress, names_to = 'dataset')
  
  ## a common table with the performance stats
  
  ens_class$overall_stats <- 
    map2(ens_class$overall_stats, 
         map(ens_class$brier, ~.x$total_bs), 
         left_join, by = 'dataset') %>% 
    map(as_tibble) %>% 
    compress(names_to = 'method') %>% 
    mutate(dataset = factor(dataset, c('train', 'cv', 'test')))

# Plot of the model performance stats ------- 
  
  insert_msg('Plots of the overall performance stats')
  
  ## plots of kappa, accuracy, Brier score and Brier's skill score
  
  ens_class$overall_stat_plots <- 
    plot_overall_stats(data = ens_class$overall_stats, 
                       y_var = 'method', 
                       title_prefix = 'Mental cluster prediction') %>% 
    map(~.x + 
          scale_y_discrete(labels = class_globals$predictor_labs))
  
  ## Kappa vs Brier score
  
  ens_class$kappa_bs_plots <- 
    plot_kappa_bs(ens_class$overall_stats, 
                  palette = class_globals$predictor_colors, 
                  labels = class_globals$predictor_labs) %>% 
    map(~.x + 
          scale_size_area(max_size = 4.5, 
                          limits = c(0, 1), 
                          name = 'Accuracy'))

# Plots of square errors --------
  
  insert_msg('Plots of Brier square errors')
  
  ens_class$square_error_plots <- 
    list(sq_table = map(ens_class$brier, 
                        ~.x$squares), 
         plot_title = class_globals$predictor_labs[names(ens_class$brier)]) %>% 
    pmap(plot_clust_squares)  
  
# Cluster-specific stats ------
  
  insert_msg('Cluster-specific stats')
  
  ens_class$clust_stats <- ens_class$predictions %>% 
    map(clust_pred_stats) %>% 
    map(compress, names_to = 'clust_id') %>% 
    compress(names_to = 'method') %>% 
    mutate(clust_id = factor(clust_id, 
                             levels(class_globals$assignment$training$clust_id)), 
           dataset = factor(dataset,
                            c('train', 'cv', 'test')))
  
  ## plotting: bubble plots with sensitivity, specificity and correct rate

  ens_class$clust_stat_plots <- 
    plot_cluster_stats(data = ens_class$clust_stats, 
                       y_var = 'method', 
                       title_prefix = 'Cluster detection, ensemble',
                       y_lab = 'Predictor set')
  
# ROC for the PTS and PTG clusters --------
  
  insert_msg('ROC stats and plots for the PTS and PTG clusters')
  
  ens_class$roc_plots[c('pts', 'pts_ptg')] <- 
    list(cluster = c('PTS', 'neutral'), 
         rev_levels = c(FALSE, TRUE), 
         title_prefix = c('PTS cluster', 'PTS + PTG clusters')) %>% 
    pmap(plot_ptb_roc, 
         ens_class$predictions, 
         stats = ens_class$clust_stats, 
         palette = class_globals$predictor_colors, 
         labels = class_globals$predictor_labs, 
         cutoffs.at = 1, 
         cutoff.labels = '')
    
# Confusion matrices -------
  
  insert_msg('Confusion matrix plots')
  
  ## captions and titles
  
  ens_class$confusion_plots[c('caps', 'titles')] <- 
    make_confusion_caps(stats = ens_class$overall_stats, 
                        models = ens_class$models, 
                        split_factor = 'method', 
                        label_vector = class_globals$predictor_labs)
  
  ## confusion matrix heat maps
  
  ens_class$confusion_plots$plots <- ens_class$predictions %>% 
    map(map, 
        plot, 
        type = 'confusion', 
        scale = 'percent', 
        cust_theme = globals$common_theme) %>% 
    map(map, 
        ~.x + 
          scale_fill_gradient2(low = 'steelblue', 
                               mid = 'white', 
                               high = 'firebrick', 
                               midpoint = 16, 
                               limits = c(0, 36), 
                               oob = scales::squish, 
                               name = '% of dataset'))
  
  ## appending with title and statistic values
  
  for(i in names(ens_class$confusion_plots$plots)) {
    
    ens_class$confusion_plots$plots[[i]] <- 
      list(x = ens_class$confusion_plots$plots[[i]], 
           y = ens_class$confusion_plots$titles[[i]], 
           z = ens_class$confusion_plots$caps[[i]]) %>% 
      pmap(function(x, y, z) x + 
             labs(title = y, 
                  subtitle = z) + 
             theme(plot.tag = element_blank()))
    
  }
  
# Variable importance -------
  
  insert_msg('Variable importance')
  
  ens_class$importance$stats <- ens_class$models %>% 
    map(varImp)
  
# END ------
  
  rm(i)
  
  insert_tail()