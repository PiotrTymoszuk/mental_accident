# Finding the optimal split of the cohort's dataset 
# into a training and a test subset used in modeling 
# and multi-parameter classification
#
# The criterion is the minimal mean Gower distance between observations 
# in respect to explanatory, basic , non-psychometric outcomes

  insert_head()

# container ------
  
  part <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables to be kept possibly indifferent between the partitions
  
  part$variables <- 
    c('age', 'age_class', 'obs_time', 
      'sex', 'education', 'employment_status', 
      'sport_profession', 'trauma_risk_profession', 
      'medical_profession', 'household_income_class', 
      'residence_alpine_region', 'smoking_status', 
      'somatic_comorbidity', 'psych_comorbidity', 'traumatic_event', 
      'cage_total', 'prior_accident', 'accident_year', 'injury_sev_strata', 
      'hospitalization', 'surgery_done')

  ## analysis table
  
  part$analysis_tbl <- ptsd$dataset %>% 
    select(ID, all_of(part$variables)) %>% 
    column_to_rownames('ID') #%>% 
    #center_data('median')

  ## size of the test subset and repetitions
  
  part$test_size <- ceiling(1/4 * nrow(part$analysis_tbl))
  
  part$reps <- paste0('rep_', 1:100)
  
  part$reps <- set_names(part$reps, part$reps)

# Candidate data partitions -------
  
  insert_msg('Candidate data partitions')
  
  set.seed(1234)
  
  part$test_ids <- part$reps %>% 
    map(function(x) sample(rownames(part$analysis_tbl), 
                           size = part$test_size, 
                           replace = FALSE))
  
# Calculating the distances --------
  
  insert_msg('Calculating the distances')

  part$dists <- part$test_ids %>% 
    future_map(partition_similarity, 
               data = part$analysis_tbl, 
               method = 'gower', 
               to_matrix = FALSE, 
               cross_only = TRUE, 
               .options = furrr_options(seed = TRUE)) %>% 
    compress(names_to = 'partition') %>% 
    mutate(optimum = ifelse(cross == min(cross), 'yes', 'no'))
  
  part$best_partition <- part$dists %>% 
    filter(optimum == 'yes') %>% 
    .$partition
  
  part$best_partition <- part$test_ids[[part$best_partition[1]]]
  
  part$best_scheme <- part$analysis_tbl %>% 
    rownames_to_column('ID') %>% 
    mutate(partition = ifelse(ID %in% part$best_partition, 
                              'test', 'training'), 
           partition = factor(partition, c('training', 'test'))) %>% 
    select(ID, partition) %>% 
    as_tibble

# caching the results ------
  
  insert_msg('Caching the results')
  
  part$analysis_tbl <- NULL
  part <- compact(part)
  
  save(part, file = './cache/randomization.RData')

# END ------
  
  plan('sequential')
  
  insert_tail()