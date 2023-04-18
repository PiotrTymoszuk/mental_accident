# A model-based approach employing non-parametric EM finite mixture modeling

  insert_head()

# container ------
  
  clust_em <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# globals ------
  
  insert_msg('Analysis globals')
  
  ## analysis table
  
  clust_em$analysis_tbl <- clust_globals$analysis_tbl$training
  
  ## number of variables and observations
  
  clust_em$n_numbers[c('observations', 'variables')] <- 
    list(nrow, ncol) %>% 
    map(~.x(clust_em$analysis_tbl))
  
  ## range of class numbers
  
  clust_em$class_no <- 1:10
  
# tuning: optimal class number ------
  
  insert_msg('Optimal class number')
  
  set.seed(1234)
  
  ## models
  
  clust_em$tuning$models <- clust_em$class_no %>% 
    future_map(~npEM(x = as.matrix(clust_em$analysis_tbl), 
                     mu0 = .x, 
                     #samebw = FALSE, 
                     verb = FALSE), 
               .options = furrr_options(seed = TRUE)) %>% 
    set_names(paste0('class_', clust_em$class_no))
  
  ## log-likelihood (final) and BIC

  clust_em$tuning$stats <- clust_em$tuning$models %>% 
    map(~.x$loglik * -1) %>% 
    map_dbl(~.x[length(.x)]) %>% 
    compress(names_to = 'k', 
             values_to = 'loglik') %>% 
    mutate(k = stri_extract(k, regex = '\\d+'), 
           k = as.numeric(k), 
           bic = 2 * loglik + log(clust_em$n_numbers$observations) * clust_em$n_numbers$variables)
  
  ## plotting the BIC against the cluster number
  
  clust_em$tuning$plot <- clust_em$tuning$stats %>% 
    ggplot(aes(x = k, 
               y = bic)) + 
    geom_vline(xintercept = 3, 
               linetype = 'dashed') + 
    geom_path(color ='steelblue') + 
    scale_x_continuous(breaks = clust_em$class_no) + 
    globals$common_theme + 
    labs(title = 'EM model fit', 
         x = 'Number of clusters, k', 
         y = 'BIC', 
         subtitle = 'Non-parametric EM mixture model')
  
# A cluster analysis object with the final cluster assignment -----
  
  insert_msg('Cluster analysis object')
  
  ## the final model
  
  clust_em$model <- clust_em$tuning$models$class_3
  
  ## and the cluster analysis object
  ## used further for prediction and visualization
  ## the cluster assignment is done by simple voting (nodal selection)
  ## for the clustering analysis, cosine distance is assumed - it will be
  ## used by the k-NN classifer in the test subset data and for visualization
  
  clust_em$posteriors <- clust_em$model$posteriors %>% 
    set_colnames(c('neutral', 'PTG', 'PTB')) %>% 
    set_rownames(rownames(clust_em$analysis_tbl))
  
  clust_em$clust_assignment <- rownames(clust_em$posteriors) %>% 
    map(~clust_em$posteriors[.x, ]) %>% 
    map(~.x[.x == max(.x)]) %>% 
    map_chr(names) %>% 
    set_names(rownames(clust_em$analysis_tbl)) %>% 
    compress(names_to = 'observation', 
             values_to = 'clust_id') %>% 
    mutate(clust_id = factor(clust_id, c('neutral', 'PTG', 'PTB')))
  
  clust_em$clust_obj <- 
    list(data = quo(clust_em$analysis_tbl), 
         dist_mtx = calculate_dist(clust_em$analysis_tbl, 
                                   method = 'cosine'), 
         dist_method = 'cosine', 
         clust_fun = 'kmeans', 
         clust_obj = clust_em$model, 
         clust_assignment = clust_em$clust_assignment, 
         dots = NULL) %>% 
    clust_analysis
  
# N numbers ------
  
  insert_msg('N numbers')
  
  clust_em$clust_n <- clust_em$clust_obj %>% 
    ngroups
  
  clust_em$clust_n <- 
    map2_chr(clust_em$clust_n[[1]], 
             clust_em$clust_n[[2]], 
             paste, sep = ': n = ') %>% 
    set_names(clust_em$clust_n[[1]])
  
# Means and variances of the explanatory factors ------
  
  insert_msg('Means and variances of the explanatory factors')
  
  clust_em$var_mean$stats <- 
    summary(clust_em$model)[c('means', 'variances')] %>% 
    map(set_colnames, levels(clust_em$clust_assignment$clust_id)) %>% 
    map(set_rownames, clust_globals$variables) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'variable') %>% 
    map(mutate, 
        var_label = exchange(variable, 
                             dict = ptsd$var_lexicon), 
        var_label = stri_replace(var_label, 
                                 regex = '\\s{1}score$', 
                                 replacement = ''), 
        var_class = exchange(variable, 
                             dict = ptsd$var_lexicon, 
                             value = 'section_short'), 
        var_class = factor(var_class, 
                           c('MH/RC/SOC', 'QoL', 'PTGI', 'PTSD'))) %>%
    map(as_tibble)
  
# Plotting the modeled means and variances for the explanatory factors ------
  
  insert_msg('Plotting the means and variances of the explanatory factors')
  
  clust_em$var_mean$plot <- 
    map2(clust_em$var_mean$stats, 
         c('mean', 'variance'), 
         ~pivot_longer(data = .x, 
                       cols = levels(clust_em$clust_assignment$clust_id), 
                       names_to = 'clust_id', 
                       values_to = .y)) %>% 
    reduce(left_join, 
           by = c('variable', 'clust_id', 'var_label', 'var_class')) %>% 
    mutate(clust_id = factor(clust_id, 
                             levels(clust_em$clust_assignment$clust_id)), 
           variable = factor(variable, clust_globals$variables), 
           var_label = unname(var_label)) %>% 
    ggplot(aes(x = mean, 
               y = reorder(var_label, mean), 
               color = clust_id, 
               fill = clust_id, 
               group = clust_id)) + 
    geom_ribbon(aes(xmin = mean - 2 * sqrt(variance)/sqrt(clust_em$n_numbers$observations), 
                    xmax = mean + 2 * sqrt(variance)/sqrt(clust_em$n_numbers$observations)), 
                alpha = 0.5, 
                color = NA) + 
    geom_ribbon(aes(xmin = mean - sqrt(variance), 
                    xmax = mean + sqrt(variance)), 
                alpha = 0.075, 
                color = NA) + 
    geom_ribbon(aes(xmin = mean, 
                    xmax = mean), 
                linewidth = 0.75) + 
    scale_color_manual(values = globals$clust_colors, 
                       labels = clust_em$clust_n, 
                       name = '') +
    scale_fill_manual(values = globals$clust_colors, 
                      labels = clust_em$clust_n, 
                      name = '') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Distribution of explanatory variables', 
         subtitle = 'Non-parametric EM mixture model', 
         x = 'Mean, 2 \u00D7 SEM') + 
    facet_grid(var_class ~., 
               scales = 'free', 
               space = 'free')
  
# Convergence stats ------
  
  insert_msg('Convergence stats')
  
  clust_em$convergence$stats <- 
    tibble(iteration = 1:length(clust_em$model$loglik), 
           log_lik = clust_em$model$loglik) %>% 
    cbind(clust_em$model$lambda %>% 
            set_colnames(levels(clust_em$clust_assignment$clust_id)) %>% 
            as_tibble) %>% 
    as_tibble
  
# Convergence plots ------
  
  insert_msg('Convergence plots')
  
  ## log-likelihood
  
  clust_em$convergence$log_lik_plot <- clust_em$convergence$stats %>% 
    ggplot(aes(x = iteration, 
               y = log_lik)) + 
    geom_path(color = 'steelblue') + 
    globals$common_theme + 
    labs(title = 'Model convergence: log-likelihood', 
         subtitle = 'Non-parametric EM mixture model', 
         x = '# iteration', 
         y = 'log-likelihood')
  
  ## lambdas
  
  clust_em$convergence$lambda_plot <- clust_em$convergence$stats %>% 
    pivot_longer(cols = levels(clust_em$clust_assignment$clust_id), 
                 values_to = 'lambda', 
                 names_to = 'clust_id') %>% 
    ggplot(aes(x = iteration, 
                y = lambda, 
                color = clust_id)) + 
    geom_path() + 
    expand_limits(y = 0) + 
    scale_color_manual(values = globals$clust_colors, 
                       name = '') + 
    globals$common_theme + 
    labs(title = 'Model convergence: lambda', 
         subtitle = 'Non-parametric EM mixture model', 
         x = '# iteration', 
         y = expression(lambda))
    
# END ------
  
  plan('sequential')
  
  insert_tail()