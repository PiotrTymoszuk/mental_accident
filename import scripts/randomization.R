# Finding the optimal split of the cohort's dataset 
# into a training and a test subset used in modeling 
# and multi-parameter classification

  insert_head()
  
# container -----
  
  rand <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables to be kept possibly indifferent between the partitions
  
  rand$variables <- c('accident_year', 
                      'age', 
                      'sex', 
                      'somatic_comorbidity', 
                      'psych_comorbidity', 
                      'traumatic_event', 
                      'injury_sev_strata')
  
  ## analysis table
  
  rand$analysis_tbl <- ptsd$dataset %>% 
    select(ID, all_of(rand$variables)) %>% 
    mutate(obs = 1:nrow(.))
  
# creating data partitions ------
  
  insert_msg('Creating data partitions')
  
  set.seed(1234)
  
  rand$part_obs <- 
    createDataPartition(y = rand$analysis_tbl$accident_year, 
                        p = 1/3, 
                        times = 1000)
  
  rand$part_ids <- set_names(names(rand$part_obs), 
                             names(rand$part_obs))
  
  for(i in rand$part_ids) {
    
    rand$analysis_tbl <- rand$analysis_tbl %>% 
      mutate(!!i := ifelse(obs %in% rand$part_obs[[i]], 
                           'test', 'training'), 
             !!i := factor(.data[[i]], c('training', 'test')))
    
  }
  
# comparing the partitions, categorical variables -------
  
  insert_msg('Comparing the partitions, categorical vars')
  
  rand$comparison[rand$variables[rand$variables != 'age']] <- 
    rand$variables[rand$variables != 'age'] %>% 
    map(freq_tester, 
        data = rand$analysis_tbl, 
        part_variables = rand$part_ids)

# age ------
  
  insert_msg('Comparing the partitions: age')
  
  ## descriptive stats: medians
  
  rand$comparison$age$median <- rand$part_ids %>% 
    future_map(~group_by(rand$analysis_tbl, .data[[.x]]) %>% 
                 summarise(median = median(age)))
  
  ## Mann-Whitney tests
  
  rand$comparison$age$test <- rand$part_ids %>% 
    future_map(~wilcox_test(formula = as.formula(paste('age ~', .x)), 
                     data = rand$analysis_tbl), 
               .options = furrr_options(seed = TRUE)) %>% 
    compress(names_to = 'partition')
  
# Top most stable partitions per variable -----
  
  insert_msg('Top most stable partitions')

  rand$top_stable <- rand$comparison %>% 
    map(~.x$test) %>% 
    map(filter, p > 0.1) %>% 
    map(top_n, n = 300, p)
  
  rand$best_partitions <- rand$top_stable %>% 
    map(~.x$partition) %>% 
    reduce(intersect)
  
# chaching the results -------
  
  insert_msg('Saving the results')
  
  save(rand, file = './cache/randomization.RData')
  
# END ------
  
  plan('sequential')
  
  insert_tail()