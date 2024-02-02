# Launches the entire pipeline

  library(soucer)
  
  ## analysis of single factors affecting the psychometric outcomes
  ## is skipped for now
  
  print(source_all(c('import.R', 
                     'exploration.R', 
                     #'factor.R', 
                     'clustering.R', 
                     'classification.R', 
                     'sensitivity.R', 
                     'literature.R', 
                     'paper.R'), 
                   message = TRUE, crash = TRUE))
  