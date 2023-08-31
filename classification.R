# Building a classifier (random forest) to identify the mental health cluster
# with help of socioeconomic, demographic, clinical and accident-related factors
# 
# as discussed in the study team, presence of flashbacks is not a suitable 
# explanatory factor but rather a symptom of a mental disorder.

# tools ------

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(stringi)
  library(soucer)
  library(trafo)

  library(doParallel)
  library(furrr)

  library(ggwordcloud)
  library(ggrepel)

  library(clustTools)
  library(rstatix)
  library(exda)

  library(caret)
  library(caretExtra)
  library(plotROC)

  library(ranger)
  library(nnet)
  library(kernlab)
  library(party)
  
  insert_head()
  
  train <- caret::train
  
  source_all('./tools/tools.R', 
             message = TRUE, 
             crash = TRUE)
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  class_globals <- list()

  ## explanatory variables: full and 
  ## explanatory variables for early diagnosis
  ## only those which are not messy (e.g. number of injured people, 
  ## type of somatic comorbidity)
  ## and 
  
  class_globals$variables$full <- ptsd$var_lexicon %>% 
    filter(type %in% c('characteristic')) %>% 
    .$variable
  
  class_globals$variables$full <- 
    class_globals$variables$full[!class_globals$variables$full %in% c('obs_time', 
                                                                      'accident_year', 
                                                                      'unwilling_flashback', 
                                                                      'flashback_frequency')]
  
  class_globals$variables$early <- 
    class_globals$variables$full[!class_globals$variables$full %in% c('psych_support_post_accident', 
                                                                      'psych_support_need', 
                                                                      'accident_aftermath', 
                                                                      'same_sport_type_post_accident', 
                                                                      'caution_post_accident', 
                                                                      'confusion_during_sport')]
  
  ## formulas
  
  class_globals$formulas <- class_globals$variables %>% 
    map(~paste('clust_id ~ ', 
               paste(.x, collapse = ' + '))) %>% 
    map(as.formula)

  ## cluster assignment schemes
  
  class_globals$assignment <- semi_clust$assignment

  ## analysis table, splitting into the training and test subset
  ## appending with the cluster assignment scheme
  
  class_globals$analysis_tbl <- ptsd$dataset %>% 
    blast(partition) %>% 
    map(select, 
        ID, 
        any_of(class_globals$variables$full))

  class_globals$analysis_tbl <- 
    map2(class_globals$analysis_tbl, 
         class_globals$assignment, 
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
  
  ## analysis tables: complete cases and normalization of the numeric variables
  
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
                 #summaryFunction = ptb_tuner, 
                 index = class_globals$folds, 
                 savePredictions = 'final', 
                 returnData = TRUE, 
                 returnResamp = 'final', 
                 classProbs = TRUE)
  
  ## algorithm colors and labels 
  
  class_globals$algo_labs <- 
    c(ranger = 'RF', 
      nnet = 'NNet', 
      svmRadial = 'SVM/radial', 
      rpart = 'RPart', 
      cforest = 'cForest', 
      sda = 'SDA', 
      elnet = 'ElasticNet', 
      ensemble = 'ensemble')
  
  class_globals$algo_colors <- 
    c(ranger = 'darkolivegreen', 
      nnet = 'indianred3', 
      svmRadial = 'cornflowerblue', 
      rpart = 'plum4', 
      cforest = 'darkorange3', 
      sda = 'brown4', 
      elnet = 'orangered2', 
      ensemble = 'gray60')
  
  class_globals$predictor_labs <-
    c(full = 'all predictors', 
      early = 'early predictors')
  
  class_globals$predictor_colors <-
    c(full = 'coral3', 
      early = 'steelblue3')
  
# Tuning scripts -------
  
  insert_msg('Tuning scripts')
  
  ## working essentially with cached tuning results
  
  if(file.exists('./cache/ranger_tune.RData')) {
    
    insert_msg('Loading cached RF tuning results')
    
    load('./cache/ranger_tune.RData')
    
  } else {
    
    source_all('./classification scripts/rf_tuning.R', 
               message = TRUE, crash = TRUE)
    
  }

  if(file.exists('./cache/nnet_tune.RData')) {
    
    insert_msg('Loading cached NNet tuning results')
    
    load('./cache/nnet_tune.RData')
    
  } else {
    
    source_all('./classification scripts/nnet_tuning.R', 
               message = TRUE, crash = TRUE)
    
  }

  if(file.exists('./cache/svm_tune.RData')) {
    
    insert_msg('Loading cached SVM tuning results')
    
    load('./cache/svm_tune.RData')
    
  } else {
    
    source_all('./classification scripts/svm_tuning.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  if(file.exists('./cache/rpart_tune.RData')) {
    
    insert_msg('Loading cached RPart tuning results')
    
    load('./cache/rpart_tune.RData')
    
  } else {
    
    source_all('./classification scripts/rpart_tuning.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  if(file.exists('./cache/sda_tune.RData')) {
    
    insert_msg('Loading cached SDA tuning results')
    
    load('./cache/sda_tune.RData')
    
  } else {
    
    source_all('./classification scripts/da_tuning.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  if(file.exists('./cache/crf_tune.RData')) {
    
    insert_msg('Loading cached SDA tuning results')
    
    load('./cache/crf_tune.RData')
    
  } else {
    
    source_all('./classification scripts/cforest_tuning.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  if(file.exists('./cache/elnet_tune.RData')) {
    
    insert_msg('Loading cached Elastic Net tuning results')
    
    load('./cache/elnet_tune.RData')
    
  } else {
    
    source_all('./classification scripts/elnet_tuning.R')
    
  }
  
# Modeling ------
  
  insert_msg('Modeling scripts')
  
  c('./classification scripts/full_models.R', 
    './classification scripts/early_models.R', 
    './classification scripts/ensemble.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ----
  
  insert_tail()
