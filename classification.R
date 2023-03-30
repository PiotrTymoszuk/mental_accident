# Building a classifier (random forest) to identify the mental health cluster
# with help of socioeconomic, demographic, clnical and accident-related factors

# tools ------

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(stringi)
  library(exda)
  library(soucer)
  library(furrr)
  library(rstatix)
  library(trafo)
  library(clustTools)
  library(ggrepel)
  library(caret)
  library(caretExtra)
  library(doParallel)
  library(OneR)
  library(party)
  library(ggwordcloud)
  
  
  insert_head()
  
  source_all('./tools/tools.R', 
             message = TRUE, 
             crash = TRUE)
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  class_globals <- list()

  ## explanatory variables 
  
  class_globals$variables <- ptsd$var_lexicon %>% 
    filter(type %in% c('characteristic')) %>% 
    .$variable
  
  class_globals$variables <- 
    class_globals$variables[!class_globals$variables %in% c('accident_injured_persons', 
                                                            'obs_time', 
                                                            'accident_year', 
                                                            'somatic_comorbidity_type')]
  
  ## full model formula
  
  class_globals$formula <-  
    as.formula(paste('clust_id ~ ', 
                     paste(class_globals$variables, 
                           collapse = '+')))
  
  
  ## explanatory variables for early diagnosis
  
  class_globals$early_variables <- 
    class_globals$variables[!class_globals$variables %in% c('psych_support_post_accident', 
                                                            'psych_support_need', 
                                                            'accident_aftermath', 
                                                            'same_sport_type_post_accident', 
                                                            'caution_post_accident', 
                                                            'unwilling_flashback', 
                                                            'flashback_frequency', 
                                                            'confusion_during_sport')]
  
  ## early model formula
  
  class_globals$early_formula <- 
    as.formula(paste('clust_id ~ ', 
                     paste(class_globals$early_variables, 
                           collapse = '+')))
  
  ## cluster assignment schemes
  
  class_globals$clust_assignment <- semi_clust$clust_obj %>% 
    map(~.x$clust_assignment) %>% 
    map(set_names, c('ID', 'clust_id'))
  
  ## analysis table, splitting into the training and test subset
  ## appending with the cluster assignment scheme
  
  class_globals$analysis_tbl <- ptsd$dataset %>% 
    dlply('partition', 
          select, 
          ID, 
          any_of(class_globals$variables)) %>% 
    map(as_tibble)
  
  class_globals$analysis_tbl <- 
    map2(class_globals$analysis_tbl, 
         class_globals$clust_assignment, 
         left_join, by = 'ID')
  
  ## analysis table: recoding the missing injury information
  
  class_globals$analysis_tbl <- class_globals$analysis_tbl %>% 
    map(mutate, 
        
        injury_severity_ais = as.character(injury_severity_ais), 
        injury_severity_ais = ifelse(is.na(injury_severity_ais), 
                                     'no information', injury_severity_ais), 
        injury_severity_ais = factor(injury_severity_ais, 
                                     c('no information', as.character(1:5))), 
        injured_count = as.character(injured_count), 
        injured_count = ifelse(is.na(injured_count), 
                               'no information', injured_count), 
        injured_count = factor(injured_count, 
                               c('no information', as.character(1:7))), 
        injury_sev_strata = ifelse(is.na(injury_sev_strata), 
                                   'no information', as.character(injury_sev_strata)), 
        injury_sev_strata = factor(injury_sev_strata, 
                                   c('no information', '1', '2', '3+')))
  
  for(i in globals$injury_vars) {
    
    class_globals$analysis_tbl <- class_globals$analysis_tbl %>% 
      map(mutate, 
          !!i := ifelse(is.na(.data[[i]]), 
                        'no information', as.character(.data[[i]])), 
          !!i := factor(.data[[i]], c('no information', 'no', 'yes')))
    
  }
  
  rm(i)
  
  ## analysis tables: complete cases and scaling of the numeric variables
  
  class_globals$analysis_tbl <- class_globals$analysis_tbl %>% 
    map(~map_dfc(.x, function(x) if(is.numeric(x)) scale(x)[, 1] else x))
  
  class_globals$analysis_tbl <- class_globals$analysis_tbl %>% 
    map(column_to_rownames, 'ID') %>% 
    map(~filter(.x, complete.cases(.x)))

  ## train control and CV folds
  
  set.seed(1234)
  
  class_globals$folds <- 
    createFolds(class_globals$analysis_tbl$training$clust_id, 
                k = 10, 
                returnTrain = TRUE)
  
  class_globals$train_control <- 
    trainControl(method = 'cv', 
                 index = class_globals$folds, 
                 savePredictions = 'final', 
                 returnData = TRUE, 
                 returnResamp = 'final', 
                 classProbs = TRUE)
  
# Analysis scripts -------
  
  insert_msg('Analysis scripts')
  
  c('./classification scripts/one_rule.R', 
    './classification scripts/rf.R', 
    './classification scripts/early_rf.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END ----
  
  insert_tail()
