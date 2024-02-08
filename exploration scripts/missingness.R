# Missing answers per participant and variable in the analysis data set

  insert_head()
  
# container ------
  
  miss <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  miss$analysis_mtx <- 
    ptsd$dataset[c('ID', eda_globals$variables$variable)] %>% 
    column_to_rownames('ID') %>% 
    as.matrix
  
  miss$analysis_mtx <- is.na(miss$analysis_mtx)
  
# record and variable missingness: missing answers per participant ----
  
  insert_msg('Missingness per participant and variable')
  
  miss$tables[c('variable', 'record')] <- 
    list(miss$analysis_mtx, t(miss$analysis_mtx)) %>%
    map(colSums) %>% 
    map2(., c('variable', 'ID'), 
         ~compress(.x, names_to = .y, values = 'n')) %>% 
    map2(., c(nrow(miss$analysis_mtx), 
              ncol(miss$analysis_mtx)), 
         ~mutate(.x, 
                 n_total = .y, 
                 fraction = n/n_total, 
                 percent = fraction * 100))
  
# plots with percentages of missing entries -------
  
  insert_msg('Plotting the percentages of missing entries')
  
  miss$plots <- 
    list(x = miss$tables[c('variable', 'record')], 
         y = c('Missing entries per variable', 
               'Missing entries per participant'), 
         z = c('steelblue', 'coral3'), 
         w = c('variable', 'ID')) %>% 
    pmap(function(x, y, z, w, v) x %>% 
           filter(percent != 0) %>% 
           ggplot(aes(x = percent, 
                      y = reorder(.data[[w]], percent))) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    fill = z) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                subtitle = c(paste('variables: n =', 
                                   ncol(miss$analysis_mtx)), 
                             paste('participants: n =', 
                                   nrow(miss$analysis_mtx))) %>% 
                  paste(collapse = ', '), 
                x = '% of missing entries'))
  
  ## labels for variables
  
  miss$plots$variable <- miss$plots$variable + 
    scale_y_discrete(labels = colnames(miss$analysis_mtx) %>% 
                       exchange(ptsd$var_lexicon, 
                                key = 'variable', 
                                value = 'label'))
  
# END ------
  
  insert_tail()