# analysis of missingness of the mental battery variables, with all respondents

  insert_head()
  
# container -----
  
  miss_mental <- list()
  
# the analysis data frame -----
  
  insert_msg('The analysis data frame')
  
  miss_mental$analysis_tbl <- ptsd$cleared %>% 
    filter(ID %in% ptsd$interview_ID) %>% 
    select(ID, all_of(ptsd$mental_variables)) %>% 
    mutate(status = ifelse(ID %in% ptsd$dataset$ID, 
                           'analyzed', 'excluded'), 
           status = factor(status, c('analyzed', 'excluded')))

# Fractions of missing observations per variable -------
  
  insert_msg('Fractions of missing observations per variable')
  
  ## percentages of excluded participants
  
  miss_mental$stats <- miss_mental$analysis_tbl %>% 
    select(all_of(ptsd$mental_variables)) %>% 
    map(is.na) %>% 
    map_dbl(sum) %>% 
    compress(names_to = 'variable', 
             values_to = 'n') %>% 
    mutate(n_total = nrow(miss_mental$analysis_tbl), 
           n_excluded = table(miss_mental$analysis_tbl$status)['excluded'], 
           percent_excluded = n/n_excluded * 100) %>% 
    arrange(-percent_excluded)
  
# Plot -------
  
  insert_msg('Plot of frequency of missing observations per variable')
  
  miss_mental$plot <- miss_mental$stats %>% 
    ggplot(aes(x = percent_excluded, 
               y = reorder(variable, percent_excluded))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             fill = 'steelblue') + 
    scale_y_discrete(labels = function(x) exchange(x, 
                                                   ptsd$var_lexicon)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Missing observations, mental health battery', 
         subtitle = paste('excluded survey respondents: n =', 
                          miss_mental$stats$n_excluded[1]), 
         x = '% of excluded respondents')
  
# END ----
  
  insert_tail()