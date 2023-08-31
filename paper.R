# Report-generating scripts

# tools -----

  library(plyr)
  library(tidyverse)
  library(exda)
  library(cowplot)
  library(figur)
  library(writexl)
  library(knitr)
  library(rmarkdown)
  library(bookdown)
  library(flextable)
  library(soucer)
  library(rmdformats)
  library(trafo)
  library(patchwork)

  insert_head()
  
  source_all('./tools/tools.R', 
             message = TRUE, crash = TRUE)
  
# Report scripts -----
  
  insert_msg('Launching the report scripts')
  
  c('./paper scripts/links.R', 
    './paper scripts/tables.R', 
    './paper scripts/figures.R', 
    './paper scripts/supplementary.R', 
    './paper scripts/render.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END -----
  
  insert_tail()