# Manuscript and supplement tables 

  insert_head()
  
# container list ----
  
  tables <- list()
  suppl_tables <- list()
  
# Tables 1 - 3: demographic and socioeconomic characteristic of the cohort ------
  
  insert_msg('Table 1 - 3: study characteristic')
  
  tables[c('cohort_demo', 
           'cohort_accident', 
           'cohort_mental')] <- 
    cohort$result_tbl[c("demo", "accident", "mental")] %>% 
    list(x = ., 
         label = c('table_1_cohort_demography', 
                   'table_2_cohort_accident', 
                   'table_3_cohort_mental_health'), 
         ref_name = c('cohort_demo', 
                      'cohort_accident', 
                      'cohort_mental'), 
         caption = c(paste('Demographic and socioeconomic characteristic', 
                           'of the study cohort.', 
                           'Numeric variables are presented as medians', 
                           'with interquartile ranges (IQR). Categorical', 
                           'variables are presented as percentages', 
                           'and counts within the complete', 
                           'observation set.'), 
                     paste('Characteristic of the sport accident,', 
                           'injury, psychological management', 
                           'and accident consequences.', 
                           'Numeric variables are presented as medians', 
                           'with interquartile ranges (IQR). Categorical', 
                           'variables are presented as percentages', 
                           'and counts within the complete', 
                           'observation set.'), 
                     paste('Mental health characteristic of the study', 
                           'participants at survey completion.', 
                           'Numeric variables are presented as medians', 
                           'with interquartile ranges (IQR). Categorical', 
                           'variables are presented as percentages', 
                           'and counts within the complete', 
                           'observation set.'))) %>% 
    pmap(mdtable)
  
# Supplementary Table S1: variables used in the analysis pipeline -----
  
  insert_msg('Table S1: variables used in the analysis pipeline')
  
  suppl_tables$analysis_vars <- ptsd$var_lexicon %>% 
    filter(variable %in% eda_globals$variables$variable) %>% 
    transmute(Section = section, 
              Variable = label, 
              Description = description) %>% 
    mdtable(label = 'table_s1_analysis_variables', 
            ref_name = 'analysis_vars', 
            caption = paste('Variables used in the analysis pipeline.'))
  
# Supplementary Table S2: mental health assessment battery -----
  
  insert_msg('Table S2: mental health assessment battery')
  
  suppl_tables$mental_battery <- ptsd$var_lexicon %>% 
    filter(type == 'response') %>% 
    transmute(Section = section, 
              Variable = label, 
              Description = description) %>% 
    mdtable(label = 'table_s2_mental_battery', 
            ref_name = 'mental_battery', 
            caption = paste('Mental health assessment battery.'))
  
# Supplementary Table S3: differences between included and excluded participants -------
  
  insert_msg('Table S3: included versus excluded')
  
  suppl_tables$incl_excl <- excl$result_tbl %>% 
    mdtable(label = 'table_s3_included_excluded', 
            ref_name = 'incl_excl', 
            caption = paste('Significant and near signifcant', 
                            '(unadjusted p < 0.05) differences between', 
                            'individuals excluded from analysis', 
                            'and analyzed study participants.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR). Categorical', 
                            'variables are presented as percentages', 
                            'and counts within the complete', 
                            'observation set.'))

# Saving the tables in the disc -----
  
  insert_msg('Saving the tables')
  
  ## main tables
  
  tables <- compact(tables)
  
  tables$cover <- 
    tibble(Table = paste('Table', 1:length(tables)), 
           Caption = map_chr(tables, attr, 'caption'))
  
  tables <- 
    tables[c('cover', names(tables)[names(tables) != 'cover'])]
 
  tables %>% 
    set_names(c('Cover', paste('Table', 1:(length(tables) - 1)))) %>% 
    write_xlsx(path = './paper/tables.xlsx')
  
  ## supplementary tables
  
  suppl_tables <- compact(suppl_tables)
  
  suppl_tables$cover <- 
    tibble(Table = paste0('Supplementary Table S', 1:length(suppl_tables)), 
           Caption = map_chr(suppl_tables, attr, 'caption'))
  
  suppl_tables <- 
    suppl_tables[c('cover', names(suppl_tables)[names(suppl_tables) != 'cover'])]
  
  suppl_tables %>% 
    set_names(c('Cover', 
                paste0('Supplementary Table S', 1:(length(suppl_tables) - 1)))) %>% 
    write_xlsx(path = './paper/supplementary_tables.xlsx')
  
  
# END -----
  
  insert_tail()