# Admission - survey time and the cluster assignment

  insert_head()
  
# container -------
  
  clust_time <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  clust_time$analysis_tbl <- semi_clust$clust_obj %>% 
    map_dfr(extract, 'assignment') %>% 
    set_names(c('ID', 'clust_id')) %>% 
    left_join(ptsd$dataset[c("ID", "obs_time")], 
              by = "ID") %>% 
    mutate(time_class = cut(obs_time, 
                            c(-Inf, 1:2 * 730, Inf),
                            c('6-24 months', '25-48 months', '49+ months')))
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  clust_time$stats <- clust_time$analysis_tbl %>% 
    explore(split_factor = 'time_class', 
            variables = 'clust_id', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>%
    set_names(c('variable', levels(clust_time$time_class)))
  
# Testing for differences ------
  
  insert_msg('Testing for differences')
  
  clust_time$test <- clust_time$analysis_tbl %>% 
    compare_variables(variables = 'clust_id', 
                      split_factor = 'time_class', 
                      what = 'eff_size', 
                      types = 'cramer_v', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Stack plot -------
  
  insert_msg('Stack plot')
  
  clust_time$plot <- 
    plot_variable(clust_time$analysis_tbl, 
                  variable = 'clust_id', 
                  split_factor = 'time_class', 
                  type = 'stack', 
                  scale = 'percent', 
                  plot_title = 'Observation time and cluster assignment', 
                  plot_subtitle = clust_time$test$plot_cap, 
                  cust_theme = globals$common_theme, 
                  x_n_labs = TRUE, 
                  x_lab = 'Admission to survey time') + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = 'Cluster')
  
# END ------
  
  insert_tail()