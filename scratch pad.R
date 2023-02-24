# parallel backend ------

insert_msg('Parallel backend')

registerDoParallel(cores = 7)

# tuning of the classifier ------

insert_msg('Tuning')

set.seed(1234)

tune_model <- 
  train(form = as.formula(paste('clust_id ~ ', 
                                paste(class_globals$variables, 
                                      collapse = '+'))), 
        data = class_globals$analysis_tbl$training, 
        method = 'C5.0', 
        metric = 'Kappa', 
        trControl = trainControl(method = 'cv', 
                                 number = 10, 
                                 savePredictions = 'final', 
                                 returnData = TRUE, 
                                 returnResamp = 'final', 
                                 classProbs = TRUE))



tune_model %>% 
  as_caretx %>% 
  predict(newdata = class_globals$analysis_tbl$test) %>% 
  map(plot, 'confusion')


varImp(tune_model)

library(caretEnsemble)

test_ensemble <- 
  caretList(form = as.formula(paste('clust_id ~ ', 
                                    paste(class_globals$variables, 
                                          collapse = '+'))), 
            data = class_globals$analysis_tbl$training, 
            methodList=c("ranger", "C5.0"), 
            trControl = trainControl(method = 'cv', 
                                     number = 10, 
                                     savePredictions = 'final', 
                                     returnData = TRUE, 
                                     returnResamp = 'final', 
                                     classProbs = TRUE))

stopImplicitCluster()