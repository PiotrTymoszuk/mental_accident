# Estimates of prevalence of mental disorder symptoms in the entire analysis 
# cohort and in literature reports. 
# Confidence intervals (BCA) are obtained by bootstrap.
#
# The literature reports are:
#
# 1) Koenen et al. DOI: 10.1017/S0033291717000708 with evaluation of 26 World 
# Health Organization World Mental Health Surveys in 18+ adults.
#
# 2) Kessler et al. DOI: 10.1080/20008198.2017.1353383, World Mental Health 
# Surveys.
# 
# 3) Kilpartick et al. 2013. DOI: 10.1002/jts.21848, US adult sample, survey
#
# 4) Darves-Bornoz et al. 2008. DOI: 10.1002/jts.20357, European survey
#
# 5) Hauffa et al. 2011. DOI: 10.1097/NMD.0b013e3182392c0d, German population

  insert_head()
  
# container --------
  
  lit_data <- list()
  
# parallel backend --------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# data -------
  
  insert_msg('Data')
  
  ## our own cohort: defining patients with all four PCL5 domains
  ## screened positive
  
  lit_data$data$cohort <- ptsd$dataset %>% 
    select(any_of(lit_globals$mental_symptoms))
  
  lit_data$data$cohort$dsm5_all_class <- 
    lit_data$data$cohort[c('dsm5_B_class', 'dsm5_C_class', 
                           'dsm5_D_class', 'dsm5_E_class')] %>% 
    map(~.x == 'positive') %>% 
    reduce(`*`) %>% 
    car::recode("0 = 'negative'; 1 = 'positive'") %>% 
    factor(c('negative', 'positive'))
  
  ## Koenen et al 2017: total n = 71085, PTSD+ (lifetime): n = 4103
  ## traumatic events: n = 51795, PTSD+ trauma-exposed (lifetime): n = 2901
  ## re-crating the data set for trauma-negative and -positive participants
  ## Source of the counts: Table 1. Warning: the numbers for trauma-exposed
  ## and the total sample are the same: likely a flaw. Similarly, the 
  ## percetages provided in the abstract do not match the Table 1 numbers
  
  lit_data$data$koenen_2017$negative <- 
    tibble(traumatic_event = rep('no', 71085 - 51795), 
           ptsd_lifetime = c(rep('positive', 4103 - 2901), 
                             rep('negative', 71085 - 51795 - (4103 - 2901))))
  
  lit_data$data$koenen_2017$positive <- 
    tibble(traumatic_event = rep('yes', 51795), 
           ptsd_lifetime = c(rep('positive', 2901), 
                             rep('negative', 51795 - 2901)))
  
  lit_data$data$koenen_2017 <- lit_data$data$koenen_2017 %>% 
    reduce(rbind) %>%
    mutate(traumatic_event = factor(traumatic_event, c('no', 'yes')), 
           ptsd_lifetime = factor(ptsd_lifetime, c('negative', 'positive')))
  
  ## Kessler et al. 2017. total: n = 68894, 70.4% with lifetime traumatic events
  
  lit_data$data$kessler_2017 <- 
    tibble(traumatic_event = c(rep('yes', round(70.4 * 68894/100)), 
                               rep('no', 68894 - round(70.4 * 68894/100)))) %>% 
    mutate(traumatic_event = factor(traumatic_event, c('no', 'yes')))
  
  ## Kilpatrick et al 2013: n = 2953, lifetime traumatic events: 89.7%, n = 2647
  ## DSM-5/same-event criterion, lifetime PTSD: 8.3%, n = 245. 
  
  lit_data$data$kilpatrick_2013 <- 
    tibble(traumatic_event = c(rep('no', 2953 - 2647), 
                               rep('yes', 2647)), 
           ptsd_lifetime = c(rep('negative', 2953 - 245), 
                             rep('positive', 245))) %>% 
    mutate(traumatic_event = factor(traumatic_event, c('no', 'yes')), 
           ptsd_lifetime = factor(ptsd_lifetime, c('negative', 'positive')))
  
  ## Darvez_Bornos el al. 2008: different numbers for trauma and PTSD
  
  lit_data$data$darvez_bornos_2008_trauma <- 
    tibble(traumatic_event = c(rep('no', 2951), 
                               rep('yes', 5845))) %>% 
    mutate(traumatic_event = factor(traumatic_event, c('no', 'yes')))
  
  lit_data$data$darvez_bornos_2008_ptsd <- 
    tibble(ptsd_year = c(rep('negative', 5645), 
                         rep('positive', 200))) %>% 
    mutate(ptsd_year = factor(ptsd_year, c('negative', 'positive')))
  
  ## Hauffa et al. 2011: n = 2510 in total
  
  lit_data$data$hauffa_2011 <- 
    tibble(traumatic_event = c(rep('yes', 597),
                               rep('no', 2510 - 597)), 
           ptsd_lifetime = c(rep('positive', 73), 
                             rep('negative', 2510 - 73))) %>% 
    mutate(traumatic_event = factor(traumatic_event, c('no', 'yes')), 
           ptsd_lifetime = factor(ptsd_lifetime, c('negative', 'positive')))
    
# Percentages of symptoms and psychometric classes --------
  
  insert_msg('Percentages of mental symptoms')
  
  set.seed(12345)
  
  lit_data$stats <- lit_data$data %>% 
    future_map(boot_estimates, 
               B = 1000,
               .options = furrr_options(seed = TRUE)) %>% 
    compress(names_to = 'source')
  
# END -------
  
  plan('sequential')
  
  insert_tail()