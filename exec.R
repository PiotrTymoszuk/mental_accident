# Launches the entire pipeline

  library(soucer)
  
  print(source_all(c('import.R', 
                     'exploration.R', 
                     'factor.R', 
                     'clustering.R', 
                     'classification.R', 
                     'paper.R'), 
                   message = TRUE, crash = TRUE))