# Estimates of prevalence of mental disorder symptoms in the entire analysis 
# cohort and in literature reports. 
# Confidence intervals (BCA) are obtained by bootstrap.
#
# We are also gathering resilience scoring data from surveys employing 
# the RS13 tool.
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
#
# 6) Mikutta et al. 2022. DOI: 10.3389/FPSYT.2022.780498
#
# 7) Leonard et al. 2021, DOI: 10.1186/S13049-021-00912-3
#
# 8) Chernova et al. 2021, DOI: 10.3389/FPSYT.2021.766261/FULL, Tyrolean 
# population during the CoV pandemic
#
# 9) Leppert et al. 2008, the seminal paper on the RS13 scale. Normative values 
# for the German population are used.


  insert_head()
  
# container --------
  
  lit_data <- list()
  
# parallel backend --------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# data of mental disorder symptom prevalence -------
  
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
  ## percentages provided in the abstract do not match the Table 1 numbers
  
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
  
  ## Mikutta et al. 2022, n = 465, n = 331 (71%) with traumatic event 
  ## n = 65 with at least one domain of PCL-5 positive, n = 3 
  ## with all domains positive
  
  lit_data$data$mikutta_2022 <- 
    tibble(traumatic_event = c(rep('yes', 331), rep('no', 465 - 331)), 
           dsm5_cluster_class = c(rep('positive', 65), 
                                  rep('negative', 465 - 65)), 
           dsm5_all_class = c(rep('positive', 3), 
                              rep('negative', 465 - 3)), 
           dsm5_B_class = c(rep('positive', 38), 
                            rep('negative', 465 - 38)), 
           dsm5_C_class = c(rep('positive', 21), 
                            rep('negative', 465 - 21)), 
           dsm5_D_class = c(rep('positive', 12), 
                            rep('negative', 465 - 12)), 
           dsm5_E_class = c(rep('positive', 29), 
                            rep('negative', 465 - 29))) %>% 
    mutate(traumatic_event = factor(traumatic_event, c('no', 'yes')))
  
  lit_data$data$mikutta_2022[c('dsm5_cluster_class', 
                               'dsm5_all_class', 
                               'dsm5_B_class', 
                               'dsm5_C_class', 
                               'dsm5_D_class', 
                               'dsm5_E_class')] <- 
    lit_data$data$mikutta_2022[c('dsm5_cluster_class', 
                                 'dsm5_all_class', 
                                 'dsm5_B_class', 
                                 'dsm5_C_class', 
                                 'dsm5_D_class', 
                                 'dsm5_E_class')] %>% 
    map_dfc(factor, c('negative', 'positive'))
  
  ## Leonard 2021, n = 55 in total, n = 6 with PTSD according to IES-R
  
  lit_data$data$leonard_2021 <- 
    tibble(dsm5_all_class = c(rep('positive', 6), 
                              rep('negative', 55 - 6))) %>% 
    mutate(dsm5_all_class = factor(dsm5_all_class, c('negative', 'positive')))
    
# Percentages of symptoms and psychometric classes --------
  
  insert_msg('Percentages of mental symptoms')
  
  set.seed(12345)
  
  lit_data$stats <- lit_data$data %>% 
    future_map(boot_estimates, 
               B = 1000,
               .options = furrr_options(seed = TRUE)) %>% 
    compress(names_to = 'source')
  
# Stats of resilience scoring -------
  
  insert_msg('Stats of resilience scoring')
  
  ## own cohort
  
  lit_data$rs_stats$cohort <- 
    tibble(mean = mean(ptsd$data$rs13_total), 
           sd = sd(ptsd$data$rs13_total), 
           min = min(ptsd$data$rs13_total), 
           max = max(ptsd$data$rs13_total), 
           median = median(ptsd$data$rs13_total), 
           q25 = quantile(ptsd$data$rs13_total, 0.25), 
           q75 = quantile(ptsd$data$rs13_total, 0.75), 
           n_total = nrow(ptsd$dataset))
  
  ## Chernova 2021
  
  lit_data$rs_stats$chernova_2021 <- 
    tibble(mean = 71.7, 
           sd = 12.3, 
           min = NA, 
           max = NA, 
           median = NA, 
           q25 = NA, 
           q75 = NA, 
           n_total = 1045)
  
  ## Leppert 2008
  
  lit_data$rs_stats$leppert_2008 <- 
    tibble(mean = 70, 
           sd = 12, 
           min = 13, 
           max = 91, 
           median = 72, 
           q25 = 63, 
           q75 = 79, 
           n_total = 2617)
  
  ## Mikutta 2022
  
  lit_data$rs_stats$mikutta_2022 <- 
    tibble(mean = 74.8, 
           sd = 10, 
           min = NA, 
           max = NA, 
           median = NA, 
           q25 = NA, 
           q75 = NA, 
           n_total = 334)
  
  lit_data$rs_stats <- lit_data$rs_stats %>% 
    compress(names_to = 'source')
  
  
# END -------
  
  plan('sequential')
  
  insert_tail()