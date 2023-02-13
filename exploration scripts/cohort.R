# Cohort characteristic in a tabular form
# 

  insert_head()
  
# container list ------
  
  cohort <- list()

# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  cohort$desc_stats <- ptsd$dataset %>% 
    explore(variables = eda_globals$variables$variable, 
            what = 'table', 
            pub_styled = TRUE)
  
# ready to use tables: demographic, accident and mental health features -----
 
  insert_msg('Ready-to-use tables')
  
  ## variable categories 
  
  cohort$var_class <- 
    list(demo = c('obs_time', 
                  'sex', 
                  'age', 
                  'residence_alpine_region', 
                  'education', 
                  'employment_status', 
                  'sport_profession', 
                  'trauma_risk_profession', 
                  'household_income_class', 
                  'smoking_status', 
                  'drug_status', 
                  'somatic_comorbidity', 
                  'somatic_comorbidity_type', 
                  'psych_comorbidity', 
                  'traumatic_event', 
                  'prime_trauma_event', 
                  'prime_trauma_event_past'), 
         accident = c('prior_accident', 
                      'sport_type', 
                      'accident_alone', 
                      'accident_culprit', 
                      'accident_injured_persons', 
                      'accident_rescue', 
                      'accident_rescue_mode', 
                      'injury_sev_strata', 
                      'injury_head', 
                      'injury_face', 
                      'injury_neck', 
                      'injury_chest', 
                      'injury_abdomen', 
                      'injury_spine', 
                      'injury_upper_limbs', 
                      'injury_lower_limbs', 
                      'injury_external_other', 
                      'psych_support_post_accident', 
                      'psych_therapy_post_accident', 
                      'accident_aftermath', 
                      'same_sport_type_post_accident', 
                      'caution_post_accident'), 
         mental = c(## gneral measures of mental health and QoL
                    'pss4_total', 
                    'gad7_total', 
                    'gad7_total_class', 
                    'phq9_total', 
                    'phq9_total_class', 
                    'phq_events_total', 
                    'phq_events_total_class', 
                    'eurohis_total', 
                    'eurohis_qol', 
                    'eurohis_health', 
                    'eurohis_energy', 
                    'eurohis_finances', 
                    'eurohis_activity', 
                    'eurohis_selfesteem', 
                    'eurohis_relationship', 
                    'eurohis_housing', 
                    'soc9l_total', 
                    'rs13_total', 
                    'rs13_total_class', 
                    'cage_total_class', 
                    ## accident-specific measures
                    'unwilling_flashback', 
                    'flashback_frequency', 
                    'dsm5_total', 
                    'dsm5_B', 
                    'dsm5_B_class', 
                    'dsm5_C', 
                    'dsm5_C_class',
                    'dsm5_D', 
                    'dsm5_D_class', 
                    'dsm5_E', 
                    'dsm5_E_class', 
                    'dsm5_cluster_class', 
                    'ptgi_total', 
                    'ptgi_fctI', 
                    'ptgi_fctII', 
                    'ptgi_fctIII', 
                    'ptgi_fctIV', 
                    'ptgi_fctV'))
  
  ## splitting the main table
  
  cohort$result_tbl <- cohort$var_class %>% 
    map(~mutate(cohort$desc_stats, 
                variable = factor(variable, .x)) %>% 
          filter(!is.na(variable))) %>% 
    map(arrange, variable) %>% 
    map(format_summ_tbl) %>% 
    map(set_names, c('Variable', 'Statistic'))
  
# END -----
  
  insert_tail()
