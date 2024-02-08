# Population estimates of depressive symptoms in the Austrian population 2019
# obtained in a microcensus study

  insert_head()
  
# container --------
  
  lit_aut <- list()
  
# raw data ------
  
  insert_msg('Raw data')
  
  ## keeping data in thousands for faster bootstrap
  
  lit_aut$raw_data <- 
    read_ods('./data/population stats/Gesundheitszustand_selbstberichtet_ATHIS2019.ods', 
             sheet = 'Tabelle_49', 
             skip = 3)
  
  lit_aut$raw_data <- lit_aut$raw_data[1, ] %>% 
    set_names(c('strata', 'n_ktotal', 
                levels(ptsd$dataset$phq8_total_class), 'severe')) %>% 
    mutate(n_ktotal = round(n_ktotal), 
           none = none * n_ktotal/100, 
           mild = mild * n_ktotal/100, 
           moderate = moderate * n_ktotal/100, 
           `moderately severe` = `moderately severe` * n_ktotal/100, 
           severe = severe * n_ktotal/100)
  
  ## counts and the data set in a long format
  
  lit_aut$counts <- 
    lit_aut$raw_data[c('none', 
                       'mild', 'moderate', 
                       'moderately severe', 'severe')] %>% 
    t %>% 
    as.data.frame %>% 
    set_names('n') %>% 
    rownames_to_column('phq8_total_class') %>% 
    mutate(n = round(n), 
           phq8_total_class = factor(phq8_total_class, 
                                     c('none', 
                                       'mild', 'moderate', 
                                       'moderately severe', 'severe'))) %>% 
    as_tibble
  
  lit_aut$data <- lit_aut$counts %>% 
    counts_to_tbl
  
# Boostrap stats ------
  
  insert_msg('Bootstrap stats')
  
  lit_aut$stats <- boot_estimates(lit_aut$data, B = 1000) %>% 
    mutate(source = 'microcensus')
  
# END -------
  
  insert_tail()