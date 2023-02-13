# Investigating distribution of the numeric variables: 
# (1) normality by Shapiro-Wilk test
# (2) compatibility with the Poisson distribution (Mean vs Variance)

  insert_head()
  
# container list -----
  
  distr <- list()

# normality check with Shapiro-wilk test -----
  
  insert_msg('Normality check')
  
  ## table
  
  distr$norm_results <- explore(ptsd$dataset, 
                                what = 'normality', 
                                variables = eda_globals$numeric_vars, 
                                pub_styled = TRUE)
  
  ## summary plot
  
  distr$norm_summ_plot <- explore(ptsd$dataset, 
                                  variables = eda_globals$numeric_vars, 
                                  what = 'normality', 
                                  pub_styled = FALSE) %>% 
    exchange(variable = 'variable', 
             dict = ptsd$var_lexicon, 
             value = 'label') %>% 
    ggplot(aes(x = stat, 
               y = reorder(variable, stat))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             fill = 'steelblue') + 
    geom_vline(xintercept = 0.9, 
               linetype = 'dashed') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Normality of variable distribution', 
         subtitle = 'Shapiro-Wilk test, normality cutoff W > 0.9', 
         x = 'W statistic')
  
# Mean and variance comparison -----
  
  insert_msg('Mean and variance')
  
  ## table
  
  distr$mvr_tbl <- explore(ptsd$dataset, 
                           variables = eda_globals$numeric_vars, 
                           what = 'list', 
                           pub_styled = FALSE) %>% 
    map(~.x$statistic) %>% 
    map(~tibble(mean = .x$value[1], 
                sd = .x$value[2])) %>% 
    map2_dfr(., names(.), 
             ~mutate(.x, 
                     variable = .y, 
                     var = sd^2, 
                     mvr = mean/var)) %>% 
    select(variable, mean, sd, var, mvr)
  
  ## plotting the Mean-Variance Ratios

  distr$mvr_plot <- distr$mvr_tbl %>% 
    exchange(variable = 'variable', 
             dict = ptsd$var_lexicon, 
             value = 'label') %>% 
    ggplot(aes(x = mvr, 
               y = reorder(variable, -mvr))) + 
    geom_bar(stat = 'identity',
             color = 'black', 
             fill = 'steelblue') + 
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Compatibility with Poisson distribution', 
         subtitle = 'Mean to variance ratio. Poisson: mean = variance', 
         x = 'mean:variance ratio')
  
# END ------
  
  insert_tail()