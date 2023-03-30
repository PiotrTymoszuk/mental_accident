# Subset size for clustering with PAM/cosine

  insert_head()
  
# container -------
  
  pow <- list()
  
# parallel backend ------
  
  plan('multisession')
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## clustering variables
  
  pow$variables <- pow$mental_variables
  
  ## normalized, median-centered psychometric data
  
  pow$analysis_tbl <- ptsd$dataset %>% 
    select(ID, all_of(ptsd$mental_variables)) %>% 
    column_to_rownames('ID') %>% 
    center_data(type = 'median')

  ## samples of the dataset of varying sizes
  
  pow$sizes <- c(50, 100, 150, 200, 250, 300)
  
  set.seed(1234)
  
  for(i in paste0('run_', 1:50)) {
    
    pow$samples[[i]] <- pow$sizes %>% 
      map(~pow$analysis_tbl[sample(1: nrow(pow$analysis_tbl), 
                                   size = .x, 
                                   replace = FALSE), ]) %>% 
      set_names(pow$sizes)
    
  }
  
  pow$samples <- pow$samples %>% 
    unlist(recursive = FALSE)
  
# Clustering tendency -------
  
  insert_msg('Clustering tendencies')
  
  pow$clust_tendency <- pow$samples %>% 
    future_map(~get_clust_tendency(.x, n = 0.5 * nrow(.x)), 
               .options = furrr_options(seed = TRUE))
  
  ## extraction of the Hopkins stats
  
  pow$hopkins <- pow$clust_tendency %>% 
    map_dbl(~.x$hopkins_stat) %>% 
    compress(names_to = 'run', 
             values_to = 'hopkins_stat') %>% 
    mutate(size = stri_split_fixed(run, pattern = '.', simplify = TRUE)[, 2], 
           size = as.numeric(size), 
           run = stri_split_fixed(run, pattern = '.', simplify = TRUE)[, 1])
  
  ## plotting the Hopkins stats as a function of the dataset size
  
  pow$hopkins_plot <- pow$hopkins %>% 
    ggplot(aes(x = factor(size), 
               y = hopkins_stat)) + 
    geom_boxplot(outlier.color = NA, 
                 fill = 'coral3', 
                 alpha = 0.3) + 
    geom_point(shape = 16, 
               size = 2, 
               color = 'black', 
               alpha = 0.5, 
               position = position_jitter(0.1)) + 
    globals$common_theme + 
    labs(title = 'Clustering tendency vs dataset size', 
         subtitle = '50 random draws per dataset size', 
         y = 'Hopkins statistic', 
         x = 'Dataset size')
  
# Clustering ------
  
  insert_msg('Clustering')
  
  pow$clust_obj <- pow$samples %>% 
    future_map(~kcluster(.x, 
                         distance_method = 'cosine', 
                         clust_fun = 'pam', 
                         k = 3), 
               .options = furrr_options(seed = TRUE))
  
# Clustering variances ------
  
  insert_msg('Clustering variances')
  
  pow$variance <- pow$clust_obj %>% 
    map(var) %>% 
    map(~.x$frac_var) %>% 
    compress(names_to = 'run', 
             values_to = 'variance') %>% 
    map_dfc(unlist) %>% 
    mutate(size = stri_split_fixed(run, pattern = '.', simplify = TRUE)[, 2], 
           size = as.numeric(size), 
           run = stri_split_fixed(run, pattern = '.', simplify = TRUE)[, 1])
  
  ## plotting the clustering variance as a function of the sample size
  
  pow$variance_plot <- pow$variance %>% 
    ggplot(aes(x = factor(size), 
               y = variance)) + 
    geom_boxplot(outlier.color = NA, 
                 fill = 'steelblue2', 
                 alpha = 0.3) + 
    geom_point(shape = 16, 
               size = 2, 
               color = 'black', 
               alpha = 0.5, 
               position = position_jitter(0.1)) + 
    globals$common_theme + 
    labs(title = 'Explained variance vs dataset size', 
         subtitle = '50 random draws per dataset size, PAM/cosine distance', 
         y = 'Explained clustering variance', 
         x = 'Dataset size')
  
# saving the results --------
  
  insert_msg('Saving the resuls')
  
  save(pow, file = './cache/power.RData')
    
# END -----
  
  rm(i)
  
  plan('sequential')
  
  insert_tail()
  
  
  
  
 

    
  
  
  
  