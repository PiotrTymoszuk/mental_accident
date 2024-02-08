# comparison of selected sociodemographic features of the study cohort 
# with the general Austrian population. 
# The general population data are derived from Statistic Austria for 2023.

  insert_head()
  
# container ------
  
  aut_stats <- list()
  
# parallel backend --------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# Variable lexicon ------
  
  insert_msg('Variables')
  
  aut_stats$variables <- c('age', 
                           'age_class', 
                           'sex', 
                           'education', 
                           'employment_status', 
                           'smoking_status', 
                           'illness')
  
  aut_stats$var_lexicon <- ptsd$var_lexicon %>% 
    select(variable, label, axis_lab, format) %>% 
    filter(variable %in% aut_stats$variables) %>% 
    rbind(tibble(variable = 'illness', 
                 label = 'physical or mental illness', 
                 axis_lab = 'physical or mental illness', 
                 format = 'factor')) %>% 
    mutate(test_type = ifelse(format == 'numeric', 
                              'wilcoxon_r', 'cramer_v'), 
           plot_type = ifelse(format == 'numeric', 
                              'box', 'stack'))
  
# The general population: age and sex -------
  
  insert_msg('General population: age and sex, 2023')

  ## sex and age distribution
  
  aut_stats$aut_data$age_sex_raw_data <- 
    read_ods('./data/population stats/Bev_nach_Alter_Geschlecht_Staatsangeh_Bundesl_Zeitreihe.ods', 
             sheet = 2, 
             skip = 1)
  
  aut_stats$aut_data$age_sex_raw_data <- 
    aut_stats$aut_data$age_sex_raw_data[-1, c(1, 23)]
  
  ## genders: age range of the study cohort
  
  aut_stats$aut_data$counts_sex <- 
    list(male = aut_stats$aut_data$age_sex_raw_data[122:186, ][[2]], 
         female = aut_stats$aut_data$age_sex_raw_data[224: 288, ][[2]]) %>% 
    map_dbl(sum) %>% 
    compress(names_to = 'sex', 
             values_to = 'n') %>% 
    mutate(sex = factor(sex, c('female', 'male')))

  ## age: 18 years, restraining to the maximal age in the cohort
  
  aut_stats$aut_data$counts_age <- 
    aut_stats$aut_data$age_sex_raw_data[2:102, ] %>% 
    set_names(c('age', 'n')) %>% 
    mutate(age = stri_extract(age, regex = '^\\d+'), 
           age = as.numeric(age)) %>% 
    filter(age >= 18, 
           age <= max(ptsd$dataset$age, na.rm = TRUE))
  
  ## age classes as specified for the cohort
  
  aut_stats$aut_data$counts_age_class <- 
    aut_stats$aut_data$counts_age %>% 
    mutate(age_class = cut(age, 
                           c(-Inf, 30, 65, Inf), 
                           c('18-30', '31-65', '>65'))) %>% 
    select(age_class, n) %>% 
    group_by(age_class) %>% 
    summarise(n = sum(n)) %>% 
    ungroup
  
# Annual income per household ---------
  
  insert_msg('Annual income per household, 2022')
  
  aut_stats$aut_data$income_raw_data <- 
    read_ods('./data/population stats/Tabelle_01.ods', 
             skip = 2)
  
  aut_stats$aut_data$income_raw_data <- 
    aut_stats$aut_data$income_raw_data[2, 1:5] %>% 
    set_names(c('strata', 'n', 'q25_eur', 'q50_eur', 'q75_eur'))
  
# Education --------
  
  insert_msg('Education, 2020')
  
  ## People aged 25 - 64
  
  aut_stats$aut_data$education_raw_data <- 
    read_ods('./data/population stats/3-2_P21.ods', 
             skip = 2)
  
  aut_stats$aut_data$education_raw_data <- 
    aut_stats$aut_data$education_raw_data [2, ] %>% 
    set_names(c('strata', 'n_total', 
                'primary', 'apprenticeship', 
                'BMS', 'AHS', 'BHS', 
                'college', 'academy', 'university'))
  
  ## classification as in the cohort
  
  aut_stats$aut_data$counts_education <- 
    aut_stats$aut_data$education_raw_data[, 3:10] %>% 
    t %>% 
    as.data.frame %>% 
    set_names('n') %>% 
    rownames_to_column('education') %>% 
    mutate(education = car::recode(education, 
                                   "'primary' = 'primary/apprenticeship'; 
                                   'apprenticeship' = 'primary/apprenticeship';
                                   'BMS' = 'secondary'; 
                                   'AHS' = 'secondary'; 
                                   'BHS' = 'secondary'; 
                                   'college' = 'tertiary'; 
                                   'academy' = 'tertiary'; 
                                   'university' = 'tertiary'"), 
           education = factor(education, 
                              c('primary/apprenticeship', 'secondary', 'tertiary'))) %>% 
    group_by(education) %>% 
    summarise(n = sum(n)) %>% 
    ungroup %>% 
    as_tibble
  
# Employment -------
  
  insert_msg('Employment, 2021')
  
  aut_stats$aut_data$employment_raw_data <- 
    read_ods('./data/population stats/RZ2021Erwerbstaetigkeit.ods', 
             sheet = 2, 
             skip = 2)
  
  aut_stats$aut_data$employment_raw_data <- 
    aut_stats$aut_data$employment_raw_data[14, ] %>% 
    select(-`Erwerbs-personen zusammen `, 
           -`Nicht-Erwerbs-personen zusammen `) %>% 
    set_names(c('year', 'n_total', 
                'employed', 'unemployed', 
                'below_15', 'retired', 
                'student', 'other')) %>% 
    select(-below_15)
  
  aut_stats$aut_data$employment_raw_data <- 
    aut_stats$aut_data$employment_raw_data %>% 
    mutate(employed = employed * n_total/100, 
           unemployed = unemployed * n_total/100, 
           retired = retired * n_total/100, 
           student = student * n_total/100, 
           other = other * n_total/100)
  
  ## counts for the employment classes consistent with the study cohort
  ## 'other' classified as unemployed
  
  aut_stats$aut_data$counts_employment <- 
    aut_stats$aut_data$employment_raw_data %>% 
    select(employed, unemployed, retired, student, other) %>% 
    t %>% 
    as.data.frame %>% 
    set_names('n') %>% 
    rownames_to_column('employment_status') %>% 
    mutate(employment_status = car::recode(employment_status, 
                                           "'other' = 'unemployed'"), 
           employment_status = factor(employment_status, 
                                      c('employed', 'unemployed', 
                                        'student', 'retired'))) %>% 
    group_by(employment_status) %>% 
    summarise(n = sum(n)) %>% 
    ungroup %>% 
    as_tibble
  
# Smoking -------
  
  insert_msg('Smoking, 2019, at least 15 years old')
  
  aut_stats$aut_data$smoking_raw_data <- 
    read_ods('./data/population stats/Rauchen_ATHIS2019.ods', 
             skip = 2, 
             sheet = 2)
  
  aut_stats$aut_data$smoking_raw_data <- 
    aut_stats$aut_data$smoking_raw_data[2, ] %>% 
    set_names(c('strata', 'n_total', 
                'smoker_daily', 'smoker_irregular', 
                'ex_smoker', 'never_smoker'))
  
  aut_stats$aut_data$smoking_raw_data[, 2:6] <- 
    aut_stats$aut_data$smoking_raw_data[, 2:6] %>% 
    map_dfc(as.numeric)
  
  aut_stats$aut_data$smoking_raw_data <- 
    aut_stats$aut_data$smoking_raw_data %>% 
    mutate(n_total = n_total  * 1000, 
           smoker_daily = smoker_daily * n_total/100, 
           smoker_irregular = smoker_irregular * n_total/100, 
           ex_smoker = ex_smoker * n_total/100, 
           never_smoker = never_smoker * n_total/100)
    
  ## absolute counts, classification as in the study cohort
  
  aut_stats$aut_data$counts_smoking <- 
    aut_stats$aut_data$smoking_raw_data[, 3:6] %>% 
    t %>% 
    as.data.frame %>% 
    set_names('n') %>% 
    rownames_to_column('smoking_status') %>% 
    mutate(smoking_status = car::recode(smoking_status, 
                                        "'smoker_daily' = 'yes'; 
                                        'smoker_irregular' = 'yes'; 
                                        'ex_smoker' = 'no'; 
                                        'never_smoker' = 'no'"), 
           smoking_status = factor(smoking_status, c('no', 'yes'))) %>% 
    group_by(smoking_status) %>% 
    summarise(n = sum(n)) %>% 
    ungroup %>% 
    as_tibble
  
# Chronic physical and mental illness ------
  
  insert_msg('Chronic physical and mental illness, 2019, over 15 years old')
  
  ## the Statistic Austria survey includes both somatic and mental illness!
  
  aut_stats$aut_data$illness_raw_data <- 
    read_ods('./data/population stats/Gesundheitszustand_selbstberichtet_ATHIS2019.ods', 
             sheet = 3, 
             skip = 3) %>% 
    set_names(c('strata', 'n_total', 'yes', 'no')) %>% 
    filter(strata == 'Insgesamt') %>% 
    mutate(n_total = n_total  * 1000, 
           yes = yes * n_total/100, 
           no = no * n_total/100)
  
  ## counts
  
  aut_stats$aut_data$counts_illenss <- 
    aut_stats$aut_data$illness_raw_data[, 3:4] %>% 
    t %>% 
    as.data.frame %>% 
    set_names('n') %>% 
    rownames_to_column('illness') %>% 
    mutate(illness = factor(illness, c('yes', 'no'))) %>% 
    as_tibble
  
# Long format of the Austrian data -------
  
  insert_msg('Long format of the Austrian data')
  
  aut_stats$data <- aut_stats$aut_data[c("counts_sex", 
                                         "counts_age", 
                                         "counts_age_class", 
                                         "counts_education", 
                                         "counts_employment", 
                                         "counts_smoking", 
                                         "counts_illenss")] %>% 
    map(counts_to_tbl) %>% 
    map(mutate, subset = 'Austria')
  
# Own data and merging with the Austrian estimates ------
  
  insert_msg('Own data and merging with the Austrian estimates')
  
  ## own data
  
  aut_stats$own_data <- ptsd$dataset %>% 
    mutate(illness = ifelse(somatic_comorbidity == 'yes' | psych_comorbidity == 'yes', 
                            'yes', 'no'), 
           illness = factor(illness, c('no', 'yes'))) %>% 
    select(sex, 
           age, 
           age_class, 
           education, 
           employment_status, 
           smoking_status, 
           illness)
  
  aut_stats$own_data <- names(aut_stats$own_data) %>% 
    map(~select(aut_stats$own_data, all_of(.x))) %>% 
    map(mutate, subset = 'cohort') %>% 
    set_names(names(aut_stats$own_data))
  
  ## merging the own and Austrian data

  aut_stats$data <- map2(aut_stats$own_data, aut_stats$data, rbind) %>% 
    map(mutate, 
        subset = factor(subset, c('cohort', 'Austria')))
  
  aut_stats$aut_data <- NULL
  aut_stats$own_data <- NULL
  
  aut_stats <- compact(aut_stats)
  aut_stats$data <- aut_stats$data[aut_stats$variables]
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  aut_stats$stats <- 
    future_map2(aut_stats$data, 
                aut_stats$variables, 
                ~explore(.x, 
                         variables = .y, 
                         split_factor = 'subset', 
                         what = 'table', 
                         pub_styled = TRUE), 
                .options = furrr_options(seed = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map_dfr(set_names, c('variable', levels(aut_stats$data[[1]]$subset)))
  
# Testing for differences between the cohort and Austria -------
  
  insert_msg('Testing for differences')
  
  aut_stats$test <- 
    list(x = aut_stats$data, 
         y = aut_stats$var_lexicon$variable, 
         z = aut_stats$var_lexicon$test_type) %>% 
    future_pmap(function(x, y, z) x %>% 
                  compare_variables(variables = y, 
                                    split_factor = 'subset', 
                                    what = 'eff_size', 
                                    types = z, 
                                    ci = FALSE, 
                                    exact = FALSE, 
                                    pub_styled = TRUE), 
                .options = furrr_options(seed = TRUE)) %>% 
    map_dfr(mutate, plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots ---------
  
  insert_msg('Plots')
  
  ## done only at request
  
 # aut_stats$plots <- 
  #  list(x = aut_stats$data, 
   #      y = aut_stats$test$variable, 
    #     z = aut_stats$test$variable %>% 
     #      exchange(aut_stats$var_lexicon, 
      #              value = 'plot_type'), 
       #  v = aut_stats$test$variable %>% 
        #   exchange(aut_stats$var_lexicon), 
      #   w = aut_stats$test$plot_cap, 
       #  u = c('age at accident, years', 
        #       rep('% of strata', nrow(aut_stats$test) - 1))) %>% 
  #  pmap(function(x, y, z, v, w, u) x %>% 
   #        plot_variable(variable = y, 
    #                     split_factor = 'subset', 
     #                    type = z, 
      #                   scale = 'percent', 
       #                  point_color = NA, 
        #                 cust_theme = globals$common_theme,
         #                plot_title = v, 
          #               plot_subtitle = w, 
           #              x_lab = '', 
            #             y_lab = u, 
             #            x_n_labs = TRUE))
    
# Result table -------
  
  insert_msg('Result table')
  
  aut_stats$result_tbl <- 
    left_join(aut_stats$stats, 
              aut_stats$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_summ_tbl(dict = aut_stats$var_lexicon, 
                    rm_n = FALSE) %>% 
    mutate(variable = stri_replace(variable, 
                                   fixed = ' at the accident', 
                                   replacement = '')) %>% 
    set_names(c('Variable', 'Cohort', 'Austrian population', 
                'Significance', 'Effect size'))
  
# Caching the results -------
  
  insert_msg('Caching the results')
  
  aut_stats$data <- NULL
  aut_stats$plots <- NULL
  
  aut_stats <- compact(aut_stats)
  
  save(aut_stats, file = './cache/aut_stats.RData')
    
# END ------
  
  plan('sequential')
  
  insert_tail()