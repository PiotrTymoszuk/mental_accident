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
    map(map_dfc, 
        stri_replace, 
        fixed = 'primary/apprenticeship', 
        replacement = 'primary') %>% 
    map(map_dfc, 
        stri_replace, 
        fixed = 'partner/third party', 
        replacement = 'comrade')
  
  ## appending the baseline characteristic table
  ## with pre-existing psychiatric conditions
  
  tables$cohort_demo <- 
    rbind(tables$cohort_demo, 
          mental$short_result_tbl)
  
  ## re-wording requested by the study team
  
 # tables$cohort_mental %>% 
  #  map_dfc(stri_replace)
  
  ## table objects
  
  tables[c('cohort_demo', 
           'cohort_accident', 
           'cohort_mental')] <- tables[c('cohort_demo', 
                                         'cohort_accident', 
                                         'cohort_mental')] %>% 
    list(x = ., 
         label = c('table_1_cohort_demography', 
                   'table_2_cohort_accident', 
                   'table_3_cohort_mental_health'), 
         ref_name = names(.), 
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
    filter(scale %in% ptsd$mental_variables) %>% 
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
  
# Supplementary Table S4 - S5: differences between included and excluded participants -------
  
  insert_msg('Table S4 - S5: included versus excluded')
  
  suppl_tables[c('no_response', 'missing')] <- 
    excl$result_tbl %>% 
    map(map_dfc, stri_replace_all, regex = '000\\s{1}', replacement = 'K ') %>% 
    map(map_dfc, stri_replace, fixed = 'no income', replacement = 'none') %>% 
    list(x = ., 
         label = c('table_s4_no_response', 
                   'table_s5_missing_data'), 
         ref_name = names(.), 
         caption = list(paste('Significant differences between', 
                              'individuals who did not respond', 
                              'to the study invitation', 
                              'and the analyzed study participants.'), 
                        paste('Significant differences between', 
                              'the study survey responders excluded from', 
                              'analysis due to missingness of', 
                              'psychometric data', 
                              'and the analyzed study participants.')) %>% 
           map(paste, 
               paste('Numeric variables are presented as medians', 
                     'with interquartile ranges (IQR). Categorical', 
                     'variables are presented as percentages', 
                     'and counts within the complete', 
                     'observation set.'))) %>% 
    pmap(mdtable)
  
# Supplementary Table S6: differences between the cohort and Austrian population ------
  
  insert_msg('Table S6: differences between the cohort and Austrian population')
  
  suppl_tables$aut_population <- aut_stats$result_tbl %>% 
    mdtable(label = 'table_s6_austrian_population', 
            ref_name = 'aut_population', 
            caption = paste('Comparison of sociodemographic features of the', 
                            'study cohort and estimates for the general', 
                            'Austrian population.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR). Categorical', 
                            'variables are presented as percentages', 
                            'and counts within the complete', 
                            'observation set.'))
  
# Supplementary Table S7: differences between the training and test subset -------
  
  insert_msg('Table S7: training vas test')
  
  suppl_tables$train_test <- partition$result_tbl %>% 
    mdtable(label = 'table_s7_included_excluded', 
            ref_name = 'train_test', 
            caption = paste('Significant differences between', 
                            'the training and test subset of the study cohort.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR). Categorical', 
                            'variables are presented as percentages', 
                            'and counts within the complete', 
                            'observation set.'))
  
# Supplementary Table S8: clustering factors in the mental clusters -------
  
  insert_msg('Table S8: clustering factors in the mental clusters')
  
  suppl_tables$clust_fct <- feat_clust$result_tbl %>% 
    compress(names_to = 'Cohort subset') %>% 
    mdtable(label = 'table_s8_clustering_factors', 
            ref_name = 'clust_fct', 
            caption = paste('Differences in psychometric clustering factors', 
                            'between the mental clusters.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR).', 
                            'Statistical significance was determined by', 
                            'false discovery rate-corrected Kruskal-Wallis', 
                            'test with eta-square effect size statistic.', 
                            'The table is available in a', 
                            'supplementary Excel file.'))
  
# Supplementary Table S9: mental disorder symptoms in the clusters ------
  
  insert_msg('Supplementary Table S9: mental disorder symptoms in the clusters')
  
  suppl_tables$clust_symptoms <- clust_bcg$result_tbl %>% 
    filter(source_var %in% c('n_number', clust_bcg$mental_variables)) %>% 
    select(- source_var) %>% 
    mdtable(label = 'table_s9_clustrer_mental_symptoms', 
            ref_name = 'clust_symptoms', 
            caption = paste('Frequency of mental disorder symptoms in the', 
                            'mental health clusters in the entire cohort.', 
                            'Categorical variables are presented as', 
                            'percentages and counts within the clusters.'))
  
# Supplementary Table S10: differences between the clusters -----
  
  insert_msg('Table S10: differences between the clusters')
  
  suppl_tables$clust_bcg <- clust_bcg$result_tbl %>% 
    filter(!source_var %in% c(clust_bcg$mental_variables)) %>% 
    select(- source_var) %>% 
    mdtable(label = 'table_s10_cluster_background', 
            ref_name = 'clust_bcg', 
            caption = paste('Differences in demographic, socioeconomic,', 
                            'clinical, accident- and recovery-related factors,', 
                            'and between the mental clusters in the entire', 
                            'cohort.', 
                            'Significant effects are presented, the full table', 
                            'is available as a supplementary Excel file.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR). Categorical', 
                            'variables are presented as percentages', 
                            'and counts within the clusters.'))
  
# Supplementary Table S11: early and late candidate predictors of mental clusters ------
  
  insert_msg('Table S10: explanatory factors for mental cluster modeling')

  suppl_tables$mod_variables <- class_globals$variables %>% 
    map(exchange, dict = ptsd$var_lexicon) %>% 
    map(unname) %>% 
    map_chr(paste, collapse = ', ') %>% 
    set_names(c('early predictor model', 'full set predictor model')) %>% 
    compress(names_to = 'Classifier type', 
             values_to = 'Explanatory variables') %>% 
    mdtable(label = 'table_s11_modeling_variables', 
            ref_name = 'mod_variables', 
            caption = paste('Sets of explanatory factors', 
                            'used for modeling of the mental', 
                            'cluster assignment.'))
  
# Supplementary Table S12: tuning -------
  
  insert_msg('Tables S12: tuning')
  
  suppl_tables$tuning <- 
    list(ranger = ranger_tune, 
         nnet = nnet_tune, 
         svmRadial = svm_tune, 
         rpart = rpart_tune, 
         sda = sda_tune, 
         cforest = crf_tune, 
         elnet = elnet_tune) %>% 
    map(~.x$best_tune) %>% 
    transpose %>% 
    map(map, map_dfc, function(x) if(is.numeric(x)) signif(x, 3) else x) %>% 
    map(map, 
        ~map2_chr(names(.x), .x, 
                  paste, sep = ' = ')) %>% 
    map(map_chr, paste, collapse = ', ') %>% 
    map(compress, 
        names_to = 'Algorithm', 
        values_to = 'Tuning parameters') %>% 
    compress(names_to = 'Classifier type') %>% 
    mutate(Algorithm = class_globals$algo_labs[Algorithm]) %>% 
    relocate(`Classifier type`) %>% 
    mdtable(label = 'table_s12_tuning', 
            ref_name = 'tuning', 
            caption = paste('The optimal combinations of machine learning', 
                            'algorithm parameters found in 10-fold', 
                            'cross-validation of the training subset', 
                            'of the study cohort.'))
  
# Supplementary Table S13 - S14: performance of machine learning models -----
  
  insert_msg('Table S13 - S14: performance of cluster classifiers')
  
  ## The table includes: overall accuracy, kappa, Brier score
  ## as well as sensitivity and specificity for the PTS cluster
  
  suppl_tables[c('early_classifiers', 'full_classifiers')] <- 
    list(early_class, full_class) %>% 
    map(~left_join(.x$overall_stats[c('method', 'dataset', 'correct_rate', 'kappa', 'brier_score')], 
                   filter(.x$clust_stats, clust_id == 'PTS')[c('method', 'dataset', 'Se', 'Sp')], 
                   by = c('method', 'dataset'))) %>%
    map(mutate, 
        dataset = globals$part_labels[as.character(dataset)], 
        method = class_globals$algo_labs[method]) %>% 
    map(map_dfc, function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    map(select, 
        method, dataset, 
        correct_rate, kappa, brier_score, Se, Sp) %>% 
    map(set_names, 
        c('Algorithm', 'Data subset', 
          'Accuracy', "Cohen's \u03BA", 
          'Brier score', 
          'Sensitivity, PTS cluster', 'Specificity, PTS cluster'))

  suppl_tables[c('early_classifiers', 'full_classifiers')] <- 
    suppl_tables[c('early_classifiers', 'full_classifiers')] %>% 
    list(x = ., 
         label = c('table_s13_early_predictor_classifiers', 
                   'table_s14_late_predictor_classifiers'), 
         ref_name = names(.), 
         caption = paste('Performance statistics of machine learning', 
                         'classifiers at predicting the mental cluster', 
                         'assignment.', 
                         c(paste('Models employing early predictors available during', 
                                 'acute medical management of the patient.'), 
                           paste('Models employing the full predictor set available', 
                                 'during acute medical management of the patient', 
                                 'and follow-up.')))) %>% 
    pmap(mdtable)
    
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