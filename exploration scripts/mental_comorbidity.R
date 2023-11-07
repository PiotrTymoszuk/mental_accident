# Frequency of pre-existing mental comorbidities in the cohort

  insert_head()
  
# container -------
  
  mental <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## analysis table: known psychiatric diagnoses
  
  mental$analysis_tbl <- 
    ptsd$mental_types[ptsd$mental_types$ID %in% ptsd$complete_ID, ]
  
  ## other diagnosis not provided
  
  mental$analysis_tbl <- 
    full_rbind(mental$analysis_tbl, 
               ptsd$dataset %>% 
                 filter(psych_comorbidity == 'yes', 
                        !ID %in% mental$analysis_tbl$ID) %>% 
                 mutate(other = 'yes') %>% 
                 select(ID, other))
  
  ## no pre-existing mental comorbidity
  
  mental$analysis_tbl <- 
    full_rbind(mental$analysis_tbl, 
               ptsd$dataset %>% 
                 filter(!ID %in% mental$analysis_tbl$ID) %>% 
                 select(ID)) %>% 
    map_dfc(~ifelse(is.na(.x), 'no', as.character(.x)))
  
  mental$analysis_tbl[, -1] <- mental$analysis_tbl[, -1] %>% 
    map_dfc(factor, c('no', 'yes'))
  
  ## variables
  
  mental$variables <- 
    c(affective = 'affective disorder', 
      personality = 'personality disorder', 
      ptsd = 'post-traumatic stress disorder', 
      somatoform = 'somatoform disorder', 
      anxiety = 'anxiety disorder', 
      adhs = 'attention-deficit hyperactivity disorder', 
      addiction = 'addiction', 
      bulimia = 'bulimia nervosa') %>% 
    compress(names_to = 'variable', 
             values_to = 'label')
  
# Table with frequencies -------
  
  insert_msg('Table with frequencies')
  
  mental$desc_stats <- mental$analysis_tbl %>% 
    explore(variables = mental$variables$variable, 
            what = 'table', 
            pub_styled = TRUE)
  
# Ready to use table --------
  
  insert_msg('Ready to use table')
  
  mental$result_tbl <- mental$desc_stats %>% 
    format_summ_tbl(dict = mental$variables, 
                    out_value = 'label') %>% 
    map_dfc(stri_replace, 
            regex = '\\nn\\s{1}=\\s{1}\\d+$', 
            replacement = '') %>% 
    mutate(variable = stri_capitalize(variable)) %>% 
    set_names(c('Variable', 'Statistic'))
  
  ## a compacted table version requested by the study team
  
  mental$short_result_tbl <- 
    map2_chr(tolower(mental$result_tbl[[1]]), 
             mental$result_tbl[[2]], 
             paste, sep = ': ') %>% 
    paste(collapse = '\n') %>% 
    tibble(Variable = 'Type of pre-existing diagnosed mental disorder', 
           Statistic = .)
  
# END -------
  
  insert_tail()