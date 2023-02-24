# Principal component analysis and testing of the clustering tendency
# normalization and median centering

  insert_head()
  
# container ------
  
  pca <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables: total scores for PTSD, PTG and EUROHIS QoL are skipped
  ## cause the scales are per definition multi-factorial
  
  pca$variables <- ptsd$mental_variables
  
  pca$variables <- 
    pca$variables[!pca$variables %in% c('dsm5_total', 
                                        'ptgi_total', 
                                        'eurohis_total')]
  
  pca$analysis_tbl <- ptsd$dataset %>% 
    select(ID, all_of(pca$variables)) %>% 
    column_to_rownames('ID') %>% 
    center_data(type = 'median')
  
  ## colors
  
  pca$comp_colors <- c('firebrick4', 
                       'darkolivegreen4', 
                       'steelblue3', 
                       'plum4', 
                       'cornflowerblue', 
                       'bisque2', 
                       'coral4', 
                       'orangered2', 
                       'firebrick2', 
                       'cornsilk3')
  
# PCA -------
  
  insert_msg('PCA')
  
  pca$pca_obj <- pca$analysis_tbl %>% 
    reduce_data(kdim = 10, red_fun = 'pca')
  
  ## variable renaming
  
  pca$pca_obj$loadings <- pca$pca_obj$loadings %>% 
    mutate(var_r = variable) %>% 
    exchange(variable = 'variable', 
             dict = ptsd$var_lexicon, 
             value = 'label') %>% 
    mutate(variable = stri_replace(variable, 
                                   regex = '\\s{1}score$', 
                                   replacement = ''))
  
# PCA plots: scree and score, the first two dimensions -----
  
  insert_msg('PCA plots')
  
  pca$pca_plots[c('scree', 
                  'scores')] <- c('scree', 
                                  'scores') %>% 
    map(~plot(pca$pca_obj, 
              type = .x, 
              cust_theme = globals$common_theme, 
              label_points = FALSE)) %>%
    map(~.x + labs(subtitle = '10-d PCA, median-centered psych scores'))
  
# Eigenvectors per component and in total -------
  
  insert_msg('Eigenvectors per component and in total')
  
  pca$eigen_tbl <- pca$pca_obj$loadings
  
  pca$eigen_tbl$total_eigen <- pca$pca_obj$loadings %>% 
    select(starts_with('comp')) %>%
    map(~.x^2) %>% 
    reduce(`+`)
  
  pca$eigen_tbl <- pca$eigen_tbl %>% 
    mutate(total_eigen = sqrt(total_eigen))
  
# Top 10 eigenvector plots ------
  
  insert_msg('Top eigenvector plots')
  
  pca$top_eigen_plots <- 
    list(x = c('total_eigen', paste0('comp_', 1:10)), 
         y = c('Total varaible impact', 
               'PC1: PTG and resilience', 
               'PC2: PTG and QoL', 
               'PC3: finances', 
               'PC4: energy, finances', 
               'PC5: spirituality, health', 
               'PC6: energy, relations', 
               'PC7: somatization, PTSD', 
               'PC8: health, spiritual, relationship', 
               'PC9: QoL and PTSD', 
               'PC10: resilience, sense of coherence'), 
         v = c(100, var(pca$pca_obj)$perc_var), 
         z = c('gray60', 
               pca$comp_colors)) %>% 
    pmap(function(x, y, v, z) ggplot(pca$eigen_tbl, 
                                     aes(x = .data[[x]], 
                                         y = reorder(variable, .data[[x]]))) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    fill = z) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank(), 
                 plot.title.position = 'plot') + 
           labs(title = y, 
                subtitle = paste0(signif(v, 3), '% variance'), 
                x = 'Loading')) %>% 
    set_names(c('total_eigen', paste0('comp_', 1:10)))
  
# Loadings plot with the top factors labeled -----
  
  insert_msg('Loadings plot')
  
  pca$pca_plots$loadings <- plot(pca$pca_obj, 
                                 type = 'loadings', 
                                 label_points = FALSE, 
                                 cust_theme = globals$common_theme, 
                                 point_alpha = 0, 
                                 segment_color = 'gray60')
  
  
  pca$pca_top_fcts <- paste0('comp_', 1:3) %>% 
    map(~top_n(pca$eigen_tbl, n = 10, abs(.data[[.x]]))) %>% 
    map(~.x$variable) %>% 
    reduce(union)
  
  pca$pca_plots$loadings <- 
    pca$pca_plots$loadings + 
    geom_hline(yintercept = 0, 
               linetype = 'dashed') + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_point(data = pca$pca_plots$loadings$data %>% 
                 filter(variable %in% pca$pca_top_fcts), 
               size = 2, 
               color = 'steelblue4') +
    geom_text_repel(data = pca$pca_plots$loadings$data %>% 
                      filter(variable %in% pca$pca_top_fcts), 
                    aes(label = variable), 
                    size = 2.5, 
                    color = 'steelblue4',
                    seed = 1234)
  
# UMAP object and plot -----
  
  insert_msg('UMAP')
  
  pca$umap_obj <- reduce_data(pca$analysis_tbl, 
                              distance_method = 'cosine', 
                              kdim = 2, 
                              red_fun = 'umap')
  
  pca$umap_plot <- plot(pca$umap_obj, 
                        cust_theme = globals$common_theme)
  
# MDS object and plot -----
  
  insert_msg('MDS')
  
  pca$mds_obj <- reduce_data(pca$analysis_tbl, 
                             distance_method = 'cosine', 
                             kdim = 2, 
                             red_fun = 'mds')
  
  pca$mds_plot <- plot(pca$mds_obj, 
                       cust_theme = globals$common_theme)
  
# Clustering tendency for the data set, PCA, UMAP and MDS layout -----
  
  insert_msg('Clustering tendency')
  
  plan('multisession')
  
  pca$clust_tendency <- list(data = pca$analysis_tbl, 
                             pca = pca$pca_obj$component_tbl %>% 
                               select(-observation), 
                             umap = pca$umap_obj$component_tbl %>% 
                               select(- observation), 
                             mds = pca$mds_obj$component_tbl %>% 
                               select(-observation)) %>% 
    future_map(get_clust_tendency, 
               n = 100, 
               .options = furrr_options(seed = TRUE))
  
  plan('sequential')
  
# END -----
  
  insert_tail()