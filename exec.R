# Launches the entire pipeline

  library(soucer)
  
  print(source_all(c('import.R', 
                     'exploration.R', 
                     'factor.R', 
                     'paper.R')))