# This script imports the data of the CovILD study

# toolbox ----

  library(readxl)
  library(foreign)
  library(soucer)
  library(stringi)
  library(plyr)
  library(tidyverse)
  library(rlang)
  library(trafo)
  library(caret)
  library(rstatix)
  library(furrr)
  library(clustTools)

  source_all('./tools/tools.R', 
             message = TRUE, crash = TRUE)

  insert_head()
  
# data containers ----
  
  ptsd <- list()
  globals <- list()
  
# globals ----

  ## graphics
  
  globals$corr_colors <- c('negative' = 'steelblue4', 
                           'positive' = 'firebrick4', 
                           'ns' = 'gray60')
  
  globals$common_text <- element_text(size = 8, 
                                      face = 'plain', 
                                      color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')
  
  globals$common_theme <- theme_classic() + 
    theme(axis.text = globals$common_text, 
          axis.title = globals$common_text, 
          plot.title = element_text(size = 8, 
                                    face = 'bold', 
                                    color = 'black', 
                                    hjust = 0), 
          plot.subtitle = globals$common_text, 
          plot.tag = element_text(size = 8, 
                                  face = 'plain', 
                                  color = 'black', 
                                  hjust = 0), 
          plot.tag.position = 'bottom', 
          legend.text = globals$common_text, 
          legend.title = globals$common_text, 
          strip.text = globals$common_text,
          strip.background = element_rect(fill = 'gray95', color = 'gray80'), 
          plot.margin = globals$common_margin, 
          panel.grid.major = element_line(color = 'gray90'))
  
  ## cluster colors
  
  globals$clust_colors <- c('neutral' = 'cadetblue3',
                            'PTG' = 'darkolivegreen3', 
                            'PTS' = 'coral4')
  
  ## injury regions
  
  globals$injury_vars <- c('injury_head', 
                           'injury_face', 
                           'injury_neck', 
                           'injury_upper_limbs', 
                           'injury_chest', 
                           'injury_spine', 
                           'injury_abdomen', 
                           'injury_lower_limbs', 
                           'injury_external_other')
  
  globals$injury_labs <- c('Head', 
                           'Face', 
                           'Neck', 
                           'Upper limbs', 
                           'Chest', 
                           'Spine region', 
                           'Abdomen', 
                           'Lower limbs', 
                           'Other external') %>% 
    set_names(globals$injury_vars)
  
  ## data partition colors
  
  globals$part_colors <- c(training = 'steelblue', 
                           train = 'steelblue', 
                           test = 'coral3', 
                           cv = 'gray60')
  
  globals$part_labels <- c(training = 'training', 
                           train = 'training', 
                           test = 'test', 
                           cv = '10-fold CV')

# reading the recoding scheme -----
  
  insert_msg('Reading the recoding scheme')
  
  ptsd$var_coding <- read_excel('./data/var_recoding.xlsx') %>% 
    mutate(args1 = stri_replace_all(args1, regex = '“|”', replacement = ''), 
           args1 = stri_replace_all(args1, regex = '‘|’', replacement = "'"), 
           args1 = map(args1, function(x) if(any(is.na(x))) NULL else x)) %>% 
    mutate(args2 = stri_replace_all(args2, regex = '“|”', replacement = ''), 
           args2 = stri_replace_all(args2, regex = '‘|’', replacement = "'"), 
           args2 = map(args2, function(x) if(any(is.na(x))) NULL else x)) %>% 
    mutate(args3 = stri_replace_all(args3, regex = '“|”', replacement = ''), 
           args3 = stri_replace_all(args3, regex = '‘|’', replacement = "'"), 
           args3 = map(args3, function(x) if(any(is.na(x))) NULL else x)) %>% 
    mutate(args4 = stri_replace_all(args4, regex = '“|”', replacement = ''), 
           args4 = stri_replace_all(args4, regex = '‘|’', replacement = "'"), 
           args4 = map(args4, function(x) if(any(is.na(x))) NULL else x)) %>% 
    mutate(args = pmap(list(x = args1, 
                            y = args2, 
                            v = args3, 
                            z = args4), 
                       function(x, y, v, z) list(x, y, v, z))) %>% 
    select(old_var, new_var, trans_fun, args)

# Reading the SPSS files ------
  
  insert_msg('Reading the SPSS files')
  
  ptsd$raw_data <- 
    read.spss('./data/PTSD Datei mit Unfallcodes NUR Eingeladene23.08.sav', 
              to.data.frame = TRUE) %>% 
    as_tibble
  
# Generating a variable label table -----
  
  insert_msg('Variable label table')
  
  ptsd$var_labels <- tibble(old_var = names(attr(ptsd$raw_data, 
                                                 'variable.labels')) %>% 
                              make.names, 
                            question_text = unname(attr(ptsd$raw_data, 
                                                        'variable.labels')))
  
# serial clearing according to the variable recoding scheme ------
  
  insert_msg('Serial recoding')
  
  ptsd$cleared <- ptsd$var_coding %>% 
    pmap(recode_var, 
         data = ptsd$raw_data) %>% 
    reduce(left_join, 
           by = 'Passwort')
  
# removal of the nonsense age -----
  
  insert_msg('Removal of the zero-age and child age records')
  
  ptsd$cleared <- ptsd$cleared %>% 
    filter(is.na(age) | age >= 18)
  
# manual polishing -----
  
  insert_msg('Manual polishing')
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(age_class = cut(age, 
                           c(-Inf, 30, 65, Inf), 
                           c('16-30', '31-65', '>65')), 
           ward_date = pss2date(ward_date), 
           hospitalization = ifelse(hospitalization == 'X', 'yes', 'no'), 
           hospitalization = factor(hospitalization, c('no', 'yes')), 
           survey_date = pss2date(survey_date), 
           obs_time = floor(as.numeric(survey_date - ward_date)), 
           accident_month = stri_split_fixed(accident_date, 
                                             pattern = '-', 
                                             simplify = TRUE)[, 2], 
           accident_month = as.numeric(accident_month), 
           accident_season = ifelse(accident_month %in% c(12, 1, 2), 
                                    'winter', 
                                    ifelse(accident_month %in% c(3, 4, 5), 
                                           'spring', 
                                           ifelse(accident_month %in% c(6, 7, 8), 
                                                  'summer', 
                                                  'fall'))),
           accident_season = ifelse(is.na(accident_season), 
                                    'no information', accident_season), 
           accident_season = factor(accident_season, 
                                    c('spring', 'summer', 'fall', 
                                      'winter', 'no information')), 
           accident_hour = stri_extract(accident_time, 
                                        regex = '^\\d{2}'), 
           accident_hour = as.numeric(accident_hour), 
           accident_daytime = ifelse(accident_hour %in% 6:18, 
                                     'day', 'night'), 
           accident_daytime = ifelse(is.na(accident_daytime), 
                                     'no information', accident_daytime), 
           accident_daytime = factor(accident_daytime, 
                                     c('day', 'night', 'no information')), 
           weight_class = stri_replace(weight_class, regex = '\\s{1}$', replacement = ''), 
           weight_class = ifelse(weight_class == '', NA, weight_class), 
           weight_class = factor(weight_class, 
                                 c('30-40', '40-50', '50-60', 
                                   '60-70', '70-80', '80-90', 
                                   '90-100', '100-110', '110-120')), 
           height_class = stri_replace(height_class, regex = '\\s{1}$', replacement = ''), 
           height_class = ifelse(height_class == '', NA, height_class), 
           height_class = factor(height_class), 
           mother_tongue = factor(mother_tongue, c('German', 'English', 'Other')), 
           education = car::recode(as.character(education), 
                                   "'primary' = 'primary/apprenticeship'; 
                                'apprenticeship' = 'primary/apprenticeship'"), 
           education = factor(education, 
                              c('primary/apprenticeship', 
                                'secondary', 'tertiary')), 
           employment_status = car::recode(as.character(employment_status), 
                                           "'household' = 'unemployed'; 
                                        'unemployed' = 'unemployed'"), 
           employment_status = factor(employment_status, 
                                      c('employed', 
                                        'unemployed',
                                        'student', 
                                        'retired')), 
           household_income_class = car::recode(as.character(household_income_class), 
                                                "'no income' = 'no income'; 
                                             '< 15000 Euro' = '< 30000 EUR'; 
                                             '15000 - 30000 Euro' = '< 30000 EUR'; 
                                             '30000 - 45000 Euro' = '30000 - 45000 EUR'; 
                                             '> 45000 Euro' = '\u2265 45000 EUR'"), 
           household_income_class = factor(household_income_class, 
                                           c('no income', 
                                             '< 30000 EUR', 
                                             '30000 - 45000 EUR', 
                                             '\u2265 45000 EUR')),
           high_income = ifelse(as.character(household_income_class) == '\u2265 45000 EUR', 
                                'yes', 'no'), 
           high_income = factor(high_income, c('no', 'yes')), 
           injury_severity_ais = as.numeric(injury_severity_ais), 
           injury_severity_ais = ifelse(injury_severity_ais > 10, 
                                        NA, injury_severity_ais), 
           injury_sev_strata = cut(injury_severity_ais, 
                                   c(0, 1, 2, 3, 10), 
                                   c('0', '1', '2', '3+'), 
                                   right = FALSE), 
           injury_sev_strata = droplevels(injury_sev_strata), 
           surgery_diagnosis = ifelse(surgery_diagnosis %in% c('0', ''), 
                                      NA, surgery_diagnosis), 
           accident_culprit = ifelse(accident_culprit == 'Anderes: !!TEXT', 
                                     'other reason', 
                                     accident_culprit), 
           accident_culprit = ifelse(accident_culprit == 'Durch einen von einem/einer Tour Partner/in verursachten Fehler', 
                                     'tour partner', 
                                     accident_culprit), 
           accident_culprit = car::recode(accident_culprit, 
                                          "'Durch eine Naturkatastrophe' = 'natural diseaster'; 
                                          'Durch einen durch Dritte verursachten Fehler' = 'third party'; 
                                          'Durch einen Schicksalsschlag' = 'blow of fate'; 
                                          'Durch einen von Ihnen verursachten Fehler' = 'self'; 
                                          '' = NA; '0' = NA"),
           accident_culprit = ifelse(as.character(accident_culprit) != 'self', 
                                     'non-self', 'self'), 
           accident_culprit = factor(accident_culprit, c('self', 'non-self')), 
           accident_injured_persons = ifelse(is.na(accident_injured_persons), 
                                             'no information', accident_injured_persons), 
           accident_injured_persons = factor(accident_injured_persons, 
                                             c('only self', 
                                               'self and partner', 
                                               '3+ persons', 
                                               'no information')), 
           accident_rescue = car::recode(accident_rescue, 
                                         "'tour partner' = 'partner/third party'; 
                                         'third party' = 'partner/third party'"), 
           accident_rescue = factor(accident_rescue , c('self', 
                                                        'partner/third party', 
                                                        'rescue team')), 
           accident_rescue_mode = ifelse(accident_rescue != 'rescue team', 
                                         'no rescue team involved', 
                                         accident_rescue_mode), 
           accident_rescue_mode = ifelse(accident_rescue_mode == 'Anderes: !!TEXT', 
                                         'other', accident_rescue_mode), 
           accident_rescue_mode = car::recode(accident_rescue_mode, 
                                              "'no rescue team involved' = 'no professional rescue'; 
                                              'on ground with stretcher' = 'on ground'; 
                                              'on ground by foot' = 'on ground'"), 
           accident_rescue_mode = factor(accident_rescue_mode, c('no professional rescue', 
                                                                 'airborne', 
                                                                 'on ground', 
                                                                 'other')), 
           accident_rescue_waiting_time = stri_replace(accident_rescue_waiting_time, 
                                                       regex = '\\s{1}$', 
                                                       replacement = ''), 
           accident_rescue_waiting_time = ifelse(accident_rescue != 'rescue team', 
                                                 'no rescue team involved', 
                                                 accident_rescue_waiting_time), 
           accident_rescue_waiting_time = factor(accident_rescue_waiting_time, 
                                                 c('no rescue team involved', 
                                                   '0-30', '30-60', '60-120')), 
           caution_post_accident = factor(caution_post_accident, 
                                          c('no change', 'more cautious', 'less cautious')), 
           flashback_frequency = ifelse(unwilling_flashback == 'no', 
                                        'no flashbacks', flashback_frequency), 
           flashback_frequency = car::recode(as.character(flashback_frequency), 
                                             "'no flashbacks' = 'none'; 
                                          '> 1 – 2 per week' = '> 1/month'; 
                                          'several per month' = '> 1/month'; 
                                          '1 – 2 per month' = '> 1/month'; 
                                          '1 – 2 per year' = '> 1/year'"), 
           flashback_frequency = factor(flashback_frequency, 
                                        c('none', '> 1/year', '> 1/month')), 
           psych_support_post_accident = ifelse(psych_therapy_post_accident == 'yes', 
                                                'yes', 
                                                as.character(psych_support_post_accident)), 
           psych_support_post_accident = factor(psych_support_post_accident, 
                                                c('no', 'yes')), 
           psych_support_need = ifelse(psych_support_post_accident == 'yes', 
                                       'no', as.character(psych_support_need)), 
           psych_support_need = factor(psych_support_need, c('no', 'yes')), 
           somatic_comorbidity_type = ifelse(somatic_comorbidity_type == 'Anderes: !!TEXT', 
                                             'other', 
                                             somatic_comorbidity_type), 
           somatic_comorbidity_type = ifelse(somatic_comorbidity == 'no', 
                                             'none', 
                                             somatic_comorbidity_type), 
           somatic_comorbidity_type = factor(somatic_comorbidity_type, 
                                             c('none', 'CVD', 'metabolic', 
                                               'pulmonary', 'neurological', 
                                               'rheumatoid', 'skin', 
                                               'cancer', 'other')), 
           prime_trauma_event = factor(prime_trauma_event, 
                                       c('none', 
                                         'severe accident', 
                                         'physical assult', 
                                         'sexual molestation', 
                                         'rape', 
                                         'severe disease', 
                                         'natural diseaster', 
                                         'war')), 
           prime_trauma_event_past = ifelse(prime_trauma_event == 'none', 
                                            'no trauma event', 
                                            as.character(prime_trauma_event_past)), 
           prime_trauma_event_past = factor(prime_trauma_event_past, 
                                            c('no trauma event', 
                                              '0 – 1 years ago', 
                                              '1 – 5 years ago', 
                                              '5 – 10 years ago', 
                                              '10+ years ago')), 
           smoking_length_years = ifelse(smoking_status == 'no', 
                                         0, smoking_length_years))
  
# types of somatic comorbidities ------
  
  insert_msg('Types of somatic comorbidities')
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(somatic_comorbidity_type = ifelse(somatic_comorbidity == 'no', 
                                             'none', 
                                             ifelse(stri_detect(somatic_comorbidity_type, 
                                                                fixed = 'Anderes'), 
                                                    as.character(somatic_comorbidity_type_text), 
                                                    as.character(somatic_comorbidity_type))), 
           somatic_comorbidity_type = car::recode(somatic_comorbidity_type, 
                                                  "'0' = 'other'; 
                                                  'tinitus' = 'neurological'; 
                                                  'Rückenschmerzen' = 'musculoskeletal'; 
                                                  'Arthrose' = 'musculoskeletal'; 
                                                  'Morbus Whipple' = 'gastrointestinal'; 
                                                  'Probleme mit den Halswirbeln und Schulter' = 'musculoskeletal'; 
                                                  '-verletzung des meniskus' = 'musculoskeletal'; 
                                                  'Erkrankung des Bewegungsapparates' = 'musculoskeletal'; 
                                                  'Darmerkrankung' = 'gastrointestinal'; 
                                                  'Hüftverletzung mit Hüftkopfnekrose' = 'musculoskeletal'; 
                                                  'Colitis ulcerosa' = 'gastrointestinal'; 
                                                  'Kreuzschmerzen' = 'musculoskeletal'; 
                                                  'Morbus Chron' = 'gastrointestinal'; 
                                                  'Bewegungsapparat' = 'musculoskeletal'; 
                                                  'HWS, BWS' = 'musculoskeletal'; 
                                                  'ich leide immer noch an den folgen des Unfalles von 20.09.2020 (krankenstand)' = 'other'; 
                                                  'Band schiben vorfal. Not related to the ski accident, ut happened two years after.' = 'musculoskeletal'; 
                                                  'Niere Dialyse' = 'metabolic'; 
                                                  'regelmäßige Schmerzen an der Wirbelsäule' = 'musculoskeletal'; 
                                                  'Diverse Schleimbeutelentzündungen' = 'musculoskeletal'; 
                                                  'Endometriose' = 'other'; 
                                                  'Allergie / Heuschnupfen' = 'other'; 
                                                  'angeborener Herzfehler (DILV)' = 'CVD'; 
                                                  'Magen Darm Beschwerden, eventuell Reizdarm' = 'gastrointestinal'; 
                                                  'immer noch wegen dem Sturz beim Eislaufen (Instabilität Ulnakopf, viele Revisionsoperationen, Chronische Schmerzen.)' = 'musculoskeletal'; 
                                                  'Skoliose + Versteifung der Wirbelsäule' = 'musculoskeletal'; 
                                                  'Konzentrations Störung' = 'other'; 
                                                  '' = 'other'"), 
           somatic_comorbidity_type = factor(somatic_comorbidity_type, 
                                             c('none', 
                                               'musculoskeletal', 
                                               'CVD', 
                                               'gastrointestinal', 
                                               'neurological', 
                                               'metabolic', 
                                               'pulmonary', 
                                               'cancer', 
                                               'rheumatoid', 
                                               'skin', 
                                               'other'))) 
  
# profession text extraction -------
  
  insert_msg('Profession text extraction')
  
  ## medical vs non-medical profession
  
  ptsd$cleared  <- ptsd$cleared %>%
    mutate(medical_profession = stri_detect(tolower(profession_text), 
                                            regex = '(arzt)|(schwester)|(therapeut)|(ärzt)|(pfleg)'), 
           medical_profession_A = stri_detect(tolower(profession_text_A), 
                                              regex = '(arzt)|(schwester)|(therapeut)|(ärzt)|(pfleg)'), 
           medical_profession = ifelse(medical_profession | medical_profession_A, 
                                       'yes', 'no'), 
           medical_profession = factor(medical_profession, c('no', 'yes')))

# major categories of sport types ------
  
  insert_msg('Recoding of the sport type')
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(sport_type = car::recode(as.character(sport_type), 
                                    "'alpine skiing' = 'ski/snowboard/cross-country'; 
                                    'biking' = 'biking'; 
                                    'sledding' = 'sledding'; 
                                    'rock climbing' = 'climbing/hiking/mountaineering/skitour'; 
                                    'hiking' = 'climbing/hiking/mountaineering/skitour'; 
                                    'MTB' = 'biking'; 
                                    'sailing' = 'other'; 
                                    'crosssountry skiing' = 'ski/snowboard/cross-country'; 
                                    'surfing' = 'other'; 
                                    'skating' = 'other'; 
                                    'mountaineering' = 'climbing/hiking/mountaineering/skitour'; 
                                    'skitouring' = 'climbing/hiking/mountaineering/skitour'; 
                                    'paragliding' = 'climbing/hiking/mountaineering/skitour'; 
                                    'sport climbing/bouldering' = 'climbing/hiking/mountaineering/skitour'; 
                                    'snowboarding' = 'ski/snowboard/cross-country'; 
                                    'other water' = 'other'; 
                                    'ice climbing' = 'climbing/hiking/mountaineering/skitour'; 
                                    'other mountain' = 'climbing/hiking/mountaineering/skitour'; 
                                    'swimming' = 'other'; 
                                    'figeln' = 'climbing/hiking/mountaineering/skitour'; 
                                    'ski jumping' = 'ski/snowboard/cross-country'; 
                                    'bobsledding' = 'sledding'; 
                                    'other winter' = 'sledding'"), 
           sport_type = factor(sport_type, 
                               c('ski/snowboard/cross-country', 'sledding', 
                                 'climbing/hiking/mountaineering/skitour', 'biking', 'other')))
  
# Counts of injured body regions ------
  
  insert_msg('Counts of injured body regions')
  
  ptsd$cleared$injured_count <- ptsd$cleared[globals$injury_vars] %>% 
    map(as.numeric) %>% 
    map(~.x - 1) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(injured_count = ifelse(!is.na(injured_count), 
                                  injured_count, 
                                  ifelse(!is.na(injury_severity_ais), 
                                         1, NA)))
  
# additional severity readouts ------
  
  insert_msg('Additional severity readouts')
  
  ## they are: need for a surgery defined as a presence of surgery diagnosis
  ## number of surgical diagnoses

  ptsd$cleared <- ptsd$cleared %>% 
    mutate(surgery_done = ifelse(is.na(surgery_diagnosis), 'no', 'yes'), 
           surgery_done = factor(surgery_done, c('no', 'yes')), 
           surgery_complexity = stri_replace(surgery_diagnosis, 
                                             regex = '/$', 
                                             replacement = ''), 
           surgery_complexity = stri_split(surgery_complexity, fixed = '/'), 
           surgery_complexity = map_dbl(surgery_complexity, length), 
           surgery_complexity = ifelse(surgery_done == 'no', 
                                       0, surgery_complexity), 
           surgery_complexity = cut(surgery_complexity, 
                                    c(-Inf, 0, 1, Inf), 
                                    c('none', '1', '2+')))
  
# PCL5-DSM5 item factor level setup -------
  
  insert_msg('DSM5 levels')
  
  for(i in paste0('dsm5_q', 1:20)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('not at all', 
                             'a little bit', 
                             'moderately', 
                             'quite a bit', 
                             'extremely')))
    
  }
  
  ## total score and cluster sub-score calculation
  
  ptsd$dsm5_numeric <- ptsd$cleared %>% 
    select(starts_with('dsm')) %>% 
    map_dfc(~as.numeric(.x) - 1)
  
  ptsd$dsm5_numeric$dsm5_total <- ptsd$dsm5_numeric[, 1:20] %>% 
    reduce(`+`)

  ptsd$dsm5_numeric$dsm5_B <- ptsd$dsm5_numeric[, 1:5] %>% 
    reduce(`+`)
  
  ptsd$dsm5_numeric$dsm5_C <- ptsd$dsm5_numeric[, 6:7] %>% 
    reduce(`+`)
  
  ptsd$dsm5_numeric$dsm5_D <- ptsd$dsm5_numeric[, 8:14] %>% 
    reduce(`+`)
  
  ptsd$dsm5_numeric$dsm5_E <- ptsd$dsm5_numeric[, 15:20] %>% 
    reduce(`+`)
  
  ptsd$cleared <- cbind(ptsd$cleared, 
                        ptsd$dsm5_numeric[c('dsm5_total', 
                                            'dsm5_B', 
                                            'dsm5_C', 
                                            'dsm5_D', 
                                            'dsm5_E')])
  
  ## class definition
  
  ptsd$dsm5_bi <- ptsd$dsm5_numeric %>% 
    map_dfc(~as.numeric(.x >= 2))
  
  ptsd$dsm5_bi$dsm5_B_class <- ptsd$dsm5_bi[, 1:5] %>% 
    reduce(`+`)
  
  ptsd$dsm5_bi$dsm5_B_class <- 
    ifelse(ptsd$dsm5_bi$dsm5_B_class > 0, 'positive', 'negative') %>% 
    factor
  
  ptsd$dsm5_bi$dsm5_C_class <- ptsd$dsm5_bi[, 6:7] %>% 
    reduce(`+`)
  
  ptsd$dsm5_bi$dsm5_C_class <- 
    ifelse(ptsd$dsm5_bi$dsm5_C_class > 0, 'positive', 'negative') %>% 
    factor
  
  ptsd$dsm5_bi$dsm5_D_class <- ptsd$dsm5_bi[, 8:14] %>% 
    reduce(`+`)
  
  ptsd$dsm5_bi$dsm5_D_class <- 
    ifelse(ptsd$dsm5_bi$dsm5_D_class > 1, 'positive', 'negative') %>% 
    factor
  
  ptsd$dsm5_bi$dsm5_E_class <- ptsd$dsm5_bi[, 15:20] %>% 
    reduce(`+`)
  
  ptsd$dsm5_bi$dsm5_E_class <- 
    ifelse(ptsd$dsm5_bi$dsm5_E_class > 1, 'positive', 'negative') %>% 
    factor
  
  ptsd$cleared <- cbind(ptsd$cleared, 
                        ptsd$dsm5_bi[c('dsm5_B_class', 
                                       'dsm5_C_class', 
                                       'dsm5_D_class', 
                                       'dsm5_E_class')]) %>% 
    mutate(dsm5_total_class = binarize(dsm5_total, 31, 'negative;positive'), 
           dsm5_total_class = factor(dsm5_total_class), 
           dsm5_cluster_class = ptsd$dsm5_bi[c('dsm5_B_class', 
                                               'dsm5_C_class', 
                                               'dsm5_D_class', 
                                               'dsm5_E_class')] %>% 
             map(~as.numeric(.x) - 1) %>% 
             reduce(`+`), 
           dsm5_cluster_class = ifelse(dsm5_cluster_class > 0, 
                                       'positive', 'negative'), 
           dsm5_cluster_class = factor(dsm5_cluster_class))
  
  ptsd$dsm5_numeric <- NULL
  ptsd$dsm5_bi <- NULL
  
# PSS4 item factor level setup -----
  
  insert_msg('PSS4 levels')
  
  for(i in paste0('pss4_q', 1:4)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('never', 
                             'almost never', 
                             'sometimes', 
                             'fairly often', 
                             'very often')))
    
  }
  
  ## calculating the score, 
  ## stratification be the median split
  
  ptsd$pss4_numeric <- ptsd$cleared %>% 
    select(starts_with('pss4')) %>% 
    mutate(pss4_q1 = as.numeric(pss4_q1) - 1, 
           pss4_q2 = as.numeric(fct_rev(pss4_q2)) - 1, 
           pss4_q3 = as.numeric(fct_rev(pss4_q3)) - 1, 
           pss4_q4 = as.numeric(pss4_q4) - 1) %>% 
    as_tibble
  
  ptsd$cleared$pss4_total <- ptsd$pss4_numeric %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(pss4_total_class = cut(pss4_total, 
                                  c(-Inf, median(ptsd$cleared$pss4_total, na.rm = TRUE), Inf), 
                                  c('low stress', 'high stress')))
  
  ptsd$pss4_numeric <- NULL
  
# PTGI item factor level setup ------
  
  insert_msg('PTGI factor level setup')
  
  for(i in paste0('ptgi_q', 1:21)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('none', 
                             'very small', 
                             'small', 
                             'moderate', 
                             'great', 
                             'very great')))
    
  }
  
  ## calculating the total score
  ## and the factor sub-scores
  
  ptsd$ptgi_numeric <- ptsd$cleared %>% 
    select(starts_with('ptgi')) %>% 
    map_dfc(~as.numeric(.x) - 1)
  
  ptsd$ptgi_numeric$ptgi_total <- ptsd$ptgi_numeric[, 1:21] %>% 
    reduce(`+`)
  
  ptsd$ptgi_numeric$ptgi_fctI <- ptsd$ptgi_numeric[, c(6, 8, 9, 15, 16, 20, 21)] %>% 
    reduce(`+`)
  
  ptsd$ptgi_numeric$ptgi_fctII <- ptsd$ptgi_numeric[, c(3, 7, 11, 14, 17)] %>% 
    reduce(`+`)
  
  ptsd$ptgi_numeric$ptgi_fctIII <- ptsd$ptgi_numeric[, c(4, 10, 12, 19)] %>% 
    reduce(`+`)
  
  ptsd$ptgi_numeric$ptgi_fctIV <- ptsd$ptgi_numeric[, c(5, 18)] %>% 
    reduce(`+`)
  
  ptsd$ptgi_numeric$ptgi_fctV <- ptsd$ptgi_numeric[, c(1, 2, 13)] %>% 
    reduce(`+`)
  
  ptsd$cleared <- cbind(ptsd$cleared, 
                        ptsd$ptgi_numeric[c('ptgi_total', 
                                            'ptgi_fctI', 
                                            'ptgi_fctII', 
                                            'ptgi_fctIII', 
                                            'ptgi_fctIV', 
                                            'ptgi_fctV')])
  
  ptsd$ptgi_numeric <- NULL
  
# DIAX/CIDI item factor setup -----
  
  insert_msg('DIAX/CIDI items')
  
  for(i in paste0('cidi_q', 1:8)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('no', 
                             'whitnessed', 
                             'yes')))
    
  }
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(cidi_extra = factor(cidi_extra, c('no', 'yes')))
  
  ptsd$cidi_numeric <- ptsd$cleared %>% 
    select(starts_with('cidi_q'), cidi_extra) %>% 
    map_dfc(car::recode, 
            "'no' = 0; 'yes' = 1; 'whitnessed' = 1", 
            as.factor = FALSE)
  
  ptsd$cleared$cidi_total <- ptsd$cidi_numeric %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(cidi_class = ifelse(cidi_total > 0, 'positive', 'negative'), 
           cidi_class = factor(cidi_class), 
           traumatic_event = car::recode(cidi_class, 
                                         "'negative' = 'no'; 
                                         'positive' = 'yes'"), 
           traumatic_number = cut(cidi_total, 
                                  c(-Inf, 0, 1, 2, Inf), 
                                  c('none', '1', '2', '3+')))
  
  ptsd$cidi_numeric <- NULL
  
# SOC9l score calculation ----
  
  insert_msg('SOC-9L score calculation')
  
  ptsd$cleared$soc9l_total <- ptsd$cleared %>% 
    select(starts_with('soc9l')) %>%
    reduce(`+`)
  
# BRCS level setting, score and classes -----
  
  insert_msg('BRCS level setup and score')
  
  for(i in paste0('brcs_q', 1:4)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('not at all', 
                             'a little bit', 
                             'a bit', 
                             'quite a bit', 
                             'very well')))
    
  }
  
  ## score calculation, class definition
  
  ptsd$cleared$brcs_total <- ptsd$cleared %>% 
    select(starts_with('brcs')) %>%
    map_dfc(as.numeric) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(brcs_total_class = cut(brcs_total, 
                                  c(-Inf, 13, 16, Inf), 
                                  c('low', 'medium', 'high')))
  
# PHQD events/somatization module, levels and score ------
  
  insert_msg('PDQD events, levels and score')
  
  for(i in paste0('phqd_events_q', 1:13)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('not affected', 
                             'a bit', 
                             'a lot')))
    
  }
  
  ## score calculation, classification with the cutoff >10, Kroenke et al
  
  ptsd$cleared$phq_events_total <- ptsd$cleared %>% 
    select(starts_with('phqd_events')) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(phq_events_total_class = cut(phq_events_total, 
                                        c(-Inf, 10, Inf), 
                                        c('negative', 'positive')))
  
# PHQD psychical satisfaction, depression and anxiety ------
  
  insert_msg('PDQD psychical satisfaction, levels and score')
  
  for(i in paste0('phqd_psych_satisf_q', 1:16)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('not at all', 
                             'some days', 
                             'more than half of days', 
                             'almost every day')))
    
  }
  
  ## score calculation: total
  
  ptsd$cleared$phqd_psych_satisf_total <- ptsd$cleared %>% 
    select(starts_with('phqd_psych_satisf')) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    reduce(`+`)
  
  ## score calculation: PHQ9 and stratification with the > 10 cutoff (Manea et al.)
  
  ptsd$cleared$phq9_total <- ptsd$cleared %>% 
    select(all_of(paste0('phqd_psych_satisf_q', 1:9))) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(phq9_total_class = cut(phq9_total, 
                                  c(-Inf, 10, Inf), 
                                  c('negative', 'positive')))
  
  ## score calculation, GAD-7 and stratification with the > 10 cutoff (Spitzer et al.)
  
  ptsd$cleared$gad7_total <- ptsd$cleared %>% 
    select(all_of(paste0('phqd_psych_satisf_q', 10:16))) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(gad7_total_class = cut(gad7_total, 
                                  c(-Inf, 10, Inf), 
                                  c('negative', 'positive')))
  
# PHQD panic and anxiety attack scores ------
  
  insert_msg('PHQD anxiety and anxiety attack scores')
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(phqd_panic_q1 = factor(phqd_panic_q1, c('no', 'yes')))
  
  for(i in paste0('phqd_panic_q', 2:4)) {

    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := as.character(.data[[i]]), 
             !!i := ifelse(as.character(phqd_panic_q1) == 'no', 
                           'no', .data[[i]]), 
             !!i := factor(.data[[i]], c('no', 'yes')))
    
  }

  for(i in paste0('phqd_anx_attack_q', 1:11)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := as.character(.data[[i]]), 
             !!i := ifelse(as.character(phqd_panic_q1) == 'yes', 
                           .data[[i]], 'no'), 
             !!i := factor(.data[[i]], 
                           c('no', 'yes')))
    
  }
  
  ## score calculation
  
  ptsd$cleared$phqd_panic_total <- ptsd$cleared %>% 
    select(starts_with('phqd_panic')) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    reduce(`+`)
  
  ptsd$cleared$phqd_anx_total <- ptsd$cleared %>% 
    select(starts_with('phqd_anx')) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    reduce(`+`)
  
  ## defining the panic
  ## as proposed in https://de.wikipedia.org/wiki/PHQ-Panikmodul
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(phqd_panic_total_class = ifelse(phqd_panic_total == 4 & phqd_anx_total > 3, 
                                     'yes', 'no'), 
           phqd_panic_total_class = factor(phqd_panic_total_class, 
                                           c('no', 'yes')))
  
# CAGE score calculation ------
  
  insert_msg('Cage score calculation')
  
  for(i in paste0('cage_q', 1:4)) {
    
    ptsd$cleared <- ptsd$cleared %>% 
      mutate(!!i := factor(.data[[i]], 
                           c('no', 'yes')))
    
  }
  
  ## score calculation
  
  ptsd$cleared$cage_total <- ptsd$cleared %>% 
    select(starts_with('cage')) %>% 
    map_dfc(~as.numeric(.x) - 1) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(cage_total_class = binarize(cage_total, 1, 'negative;positive'))
  
# RS13 score calculation -----
  
  insert_msg('RS13 score calculation')

  ptsd$cleared$rs13_total <- ptsd$cleared %>% 
    select(starts_with('rs13')) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(rs13_total_class = cut(rs13_total, 
                                  c(-Inf, 66, 72, Inf), 
                                  c('low', 'moderate', 'high')))
  
# EUROHIS-QOL-8 score calculation -----
  
  insert_msg('EUROHIS-QOL8 score calculation')
  
  ## total score calculation (mean of the domains)
  ## subscores: each question separately
  ##
  ## the item score are inverted, i.e. very poor: 1 and excellent: 5
  
  ptsd$cleared$eurohis_total <- ptsd$cleared %>% 
    select(starts_with('eurohis')) %>% 
    map(function(x) 6 - x) %>% 
    reduce(`+`)
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(eurohis_total = eurohis_total/8, 
           eurohis_qol = 6 - eurohis_q1, 
           eurohis_health = 6 - eurohis_q2,
           eurohis_energy = 6 - eurohis_q3, 
           eurohis_finances = 6 - eurohis_q4, 
           eurohis_activity = 6 - eurohis_q5, 
           eurohis_selfesteem = 6 - eurohis_q6, 
           eurohis_relationship = 6 - eurohis_q7, 
           eurohis_housing = 6 - eurohis_q8)
  
# Coding interactions for modeling -------
  
  insert_msg('Coding for interactions')
  
  ## appending the table with interactions (expert opinion)
  ## may be optionally investigated by linear modeling

  ptsd$cleared <- ptsd$cleared %>% 
    mutate(support_injury = car::recode(as.character(injury_sev_strata), 
                                        "'1' = '1'; 
                                        '2' = '2+'; 
                                        '3+' = '2+'"),
           support_injury = factor(support_injury, c('1', '2+')), 
           support_injury = interaction(support_injury, 
                                        psych_support_post_accident), 
           support_rescue = car::recode(as.character(accident_rescue), 
                                        "'self' = 'self'; 
                                        'partner/third party' = 'non-self'; 
                                        'rescue team' = 'non-self'"), 
           support_rescue = factor(support_rescue, 
                                   c('self', 'non-self')), 
           support_rescue = interaction(support_rescue, 
                                        psych_support_post_accident), 
           injury_rescue = interaction(injury_sev_strata, 
                                       accident_rescue), 
           injury_support_need = car::recode(as.character(injury_sev_strata), 
                                             "'1' = '1'; 
                                              '2' = '2+'; 
                                              '3+' = '2+'"), 
           injury_support_need = factor(injury_support_need, c('1', '2+')), 
           injury_support_need = interaction(injury_support_need, 
                                             psych_support_need), 
           rescue_support_need = car::recode(as.character(accident_rescue), 
                                             "'self' = 'self'; 
                                              'partner/third party' = 'non-self'; 
                                             'rescue team' = 'non-self'"), 
           rescue_support_need = factor(rescue_support_need, 
                                        c('self', 'non-self')), 
           rescue_support_need = interaction(rescue_support_need, 
                                             psych_support_need), 
           support_aftermath = interaction(accident_aftermath, 
                                           psych_support_post_accident), 
           aftermath_support_need = interaction(accident_aftermath, 
                                                psych_support_need))
  
# Approximate BMI -------
  
  insert_msg('Approximate BMI')
  
  ## the values are quite discretely distributed - wont be included in modeling
  ## and cohort characteristic
  
  ptsd$cleared <- ptsd$cleared %>% 
    mutate(approx_weight = stri_split(weight_class, fixed = '-'), 
           approx_weight = map(approx_weight, as.numeric), 
           approx_weight = map_dbl(approx_weight, mean), 
           approx_height = stri_split(height_class, fixed = '-'), 
           approx_height = map(approx_height, as.numeric), 
           approx_height = map_dbl(approx_height, mean), 
           approx_bmi = approx_weight/(approx_height/100)^2, 
           weight_class = cut(approx_bmi, 
                              c(-Inf, 25, 30, Inf), 
                              c('normal', 'overweight', 'obese')))
  
# Generating the variable lexicon -----
  
  insert_msg('Clearing the variable lexicon')
  
  ## generic variables
  
  ptsd$var_lexicon <- left_join(ptsd$var_coding[c('new_var', 'old_var')], 
                                ptsd$var_labels, 
                                by = 'old_var') %>% 
    set_names(c('variable', 
                'old_variable', 
                'question_text'))
  
  ## variables defined during wrangling
  
  ptsd$var_lexicon <- full_join(read_excel('./data/battery_vars.xlsx'), 
                                ptsd$var_lexicon, 
                                by = 'variable') %>% 
    mutate(axis_lab = ifelse(!is.na(unit), 
                             paste(label, unit, sep = ', '), 
                             label))

  ptsd$var_lexicon <- ptsd$var_lexicon %>% 
    filter(variable %in% names(ptsd$cleared))

# Creating a table with the participants with complete mental health battery ------
  
  insert_msg('Filtering: complete mental helath battery')
  
  ## IDs of participants who completed the interview
  
  ptsd$interview_ID <- ptsd$cleared %>% 
    filter(!is.na(survey_date)) %>% 
    .$ID
  
  ## mental health variables used for clustering
  ## PSS4 (stress) with poor consistency is removed from further analysis
  
  ptsd$mental_variables <- ptsd$var_lexicon %>% 
    filter(type == 'response', 
           format == 'numeric') %>% 
    .$variable
  
  ptsd$mental_variables <- 
    ptsd$mental_variables[ptsd$mental_variables != 'pss4_total']
  
  ## participants with the complete mental health variable panel
  
  ptsd$complete_ID <- ptsd$cleared %>% 
    select(ID, all_of(ptsd$mental_variables)) %>% 
    filter(complete.cases(.)) %>% 
    .$ID
  
  ptsd$dataset <- ptsd$cleared %>% 
    filter(ID %in% ptsd$complete_ID) %>% 
    map_dfc(function(x) if(is.factor(x)) droplevels(x) else x)
  
# Patching some missing information on injuries for the included participants -----
  
  insert_msg('Patching missing injury information')
  
  ## based on self report, surgery diagnoses and similar
  
  ptsd$injury_patch <- read_tsv('./data/injury_patch.tsv') %>% 
    mutate(injury_sev_strata = cut(injury_severity_ais, 
                                   c(0, 1, 2, 3, 10), 
                                   c('0', '1', '2', '3+'), 
                                   right = FALSE), 
           injury_sev_strata = droplevels(injury_sev_strata))
  
  for(i in globals$injury_vars) {
    
    ptsd$injury_patch <- ptsd$injury_patch %>% 
      mutate(!!i := car::recode(.data[[i]], "'0' = 'no'; '1' = 'yes'"), 
             !!i := factor(.data[[i]], c('no', 'yes')))
    
  }
  
  ptsd$injury_patch$injured_count <- ptsd$injury_patch[globals$injury_vars] %>% 
    map(as.numeric) %>% 
    map(~.x - 1) %>% 
    reduce(`+`)
  
  ptsd$injury_patch <- ptsd$injury_patch %>% 
    mutate(injured_count = ifelse(!is.na(injured_count), 
                                  injured_count, 
                                  ifelse(!is.na(injury_severity_ais), 
                                         1, NA)))
  
  ptsd$injury_patch <- ptsd$injury_patch %>% 
    select(ID, any_of(names(ptsd$dataset)))
  
  ## patching
  
  ptsd$dataset <- 
    ptsd$dataset[c('ID', names(ptsd$dataset)[!names(ptsd$dataset) %in% names(ptsd$injury_patch)])]
  
  ptsd$dataset <- left_join(ptsd$dataset, 
                            ptsd$injury_patch, 
                            by = 'ID')
  
# Training and test datasets based on randomization results -----
  
  insert_msg('Training and test datasets')
  
  ## randomization be numeric psychometric outcomes
  
  if(file.exists('./cache/randomization.RData')) {
    
    insert_msg('Loading cached randomization results')
    
    load('./cache/randomization.RData')
    
  } else {
    
    source_all('./import scripts/partition.R')
    
  }
  
  ptsd$rand_scheme <- part$best_scheme
  
  ptsd$dataset <- left_join(ptsd$dataset, 
                            ptsd$rand_scheme, 
                            by = 'ID')
  
# diagnosed mental comordidities -------
  
  insert_msg('Diagnosed mental comorbidity')
  
  ptsd$mental_types <- read_excel('./data/psych_patch.xlsx') %>% 
    mutate(ID = stri_replace(ID, regex = '\n$', replacement = ''))
  
  ptsd$mental_types[, -1] <- ptsd$mental_types[, -1] %>% 
    map_dfc(factor, c('no', 'yes'))
  
# END -----
  
  ptsd$cleared <- as_tibble(ptsd$cleared)
  
  ptsd$var_labels <- NULL

  ptsd <- compact(ptsd)
  
  rm(i)
  
  insert_tail()