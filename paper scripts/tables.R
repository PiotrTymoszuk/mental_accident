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
    filter(type %in% c('characteristic', 'response')) %>% 
    transmute(`Variable name in R` = variable, 
              Variable = label, 
              Description = description,
              Format = format,  
              Unit = unit)
  
  ## finding the possible factor levels
  
  fct_levs <- suppl_tables$analysis_vars %>% 
    filter(Format == 'factor') %>% 
    .$`Variable name in R`
  
  fct_levs <- ptsd$dataset[fct_levs] %>% 
    map(levels) %>% 
    compress(names_to = 'Variable name in R', 
             values_to = 'Categories') %>% 
    mutate(Categories = map_chr(Categories, paste, collapse = ', '))
    
  ## the entire table
  
  suppl_tables$analysis_vars <- 
    left_join(suppl_tables$analysis_vars, 
              fct_levs, 
              by = 'Variable name in R') %>% 
    mdtable(label = 'table_s1_analysis_variables', 
            ref_name = 'analysis_vars', 
            caption = paste('Variables used in the analysis pipeline.'))
  
  rm(fct_levs)
  
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
  
# Supplementary Table S3: mental scale consistency -------
  
  insert_msg('Table S3: mental scale consistency')
  
  suppl_tables$consistency <- 
    left_join(compress(cons$pca_factor_n, 
                       names_to = 'scale', 
                       values_to = 'n'), 
              cons$omega_tbl, 
              by = 'scale') %>% 
    filter(scale != 'cage_total') %>% 
    arrange(-omega) %>% 
    mutate(scale = ifelse(scale != 'brcs_total', 
                          exchange(scale, 
                                   dict = ptsd$var_lexicon, 
                                   key = 'variable', 
                                   value = 'label'), 
                          'BRCS'), 
           scale = stri_replace(scale, 
                                fixed = ' score', 
                                replacement = ''), 
           omega = signif(omega, 2)) %>% 
    set_names(c('Scale', 'Number of latent factors', 'Total omega')) %>% 
    mdtable(label = 'table_s3_consistency', 
            ref_name = 'consistency', 
            caption = paste("Consistency of the psychometric tools used", 
                            "in the study measured by McDonald's omega."))
  
# Supplementary Table S4: differences between included and excluded participants -------
  
  insert_msg('Table S4: included versus excluded')
  
  suppl_tables$incl_excl <- excl$result_tbl %>% 
    mdtable(label = 'table_s4_included_excluded', 
            ref_name = 'incl_excl', 
            caption = paste('Significant differences between', 
                            'individuals excluded from analysis', 
                            'and analyzed study participants.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR). Categorical', 
                            'variables are presented as percentages', 
                            'and counts within the complete', 
                            'observation set.'))
  
# Supplementary Table S5: differences between the data partitions -----
  
  insert_msg('Table S5: training/test differences')
  
  suppl_tables$partition <- partition$result_tbl %>% 
    mdtable(label = 'table_s5_training_test', 
            ref_name = 'partition', 
            caption = paste('Significant and near-significant (p < 0.1)', 
                            'differences between the training and test', 
                            'subsets of the study cohort.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR). Categorical', 
                            'variables are presented as percentages', 
                            'and counts within the complete', 
                            'observation set.'))

# Supplementary Table S6: clustering factors in the mental clusters -------
  
  insert_msg('Table S6: clustering factors in the mental clusters')
  
  suppl_tables$clust_fct <- feat_clust$result_tbl %>% 
    compress(names_to = 'Cohort subset') %>% 
    mdtable(label = 'table_s6_clustering_factors', 
            ref_name = 'clust_fct', 
            caption = paste('Differences in psychometric clustering factors', 
                            'between the mental clusters.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR).', 
                            'The table is available in a', 
                            'supplementary Excel file.'))
  
# Supplementary Table S7: differences between the clusters -----
  
  insert_msg('Table S7: differences between the clusters')
  
  suppl_tables$clust_bcg <- clust_bcg$result_tbl %>% 
    compress(names_to = 'Cohort subset') %>% 
    mdtable(label = 'table_s7_clustering_factors', 
            ref_name = 'clust_bcg', 
            caption = paste('Significant and near-significant (p < 0.1)',
                            'differences in demographic, socioeconomic,', 
                            'clinical and accident-related factors', 
                            'between the mental clusters.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR). Categorical', 
                            'variables are presented as percentages', 
                            'and counts within the complete', 
                            'observation set.',  
                            'The table is available in a', 
                            'supplementary Excel file.'))
  
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