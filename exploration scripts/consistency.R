# Consistency of the assessment battery components measured by
# McDonald's omega

  insert_head()
  
# container -----
  
  cons <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# globals ------
  
  insert_msg('Globals')
  
  ## a list with variables
  
  cons$var_list <- list(dsm5_total = paste0('dsm5_q', 1:20), 
                        ptgi_total = paste0('ptgi_q', 1:21), 
                        rs13_total = paste0('rs13_q', 1:13), 
                        brcs_total = paste0('brcs_q', 1:4), 
                        cage_total = paste0('cage_q', 1:4), 
                        pss4_total = paste0('pss4_q', 1:4), 
                        phq9_total = paste0('phqd_psych_satisf_q', 1:9), 
                        gad7_total = paste0('phqd_psych_satisf_q', 10:16), 
                        phqd_panic_total = paste0('phqd_panic_q', 1:4), 
                        phq_events_total = paste0('phqd_events_q', 1:13), 
                        soc9l_total = paste0('soc9l_q', 1:9), 
                        eurohis_total = paste0(c('eurohis_qol', 
                                                 'eurohis_health', 
                                                 'eurohis_energy', 
                                                 'eurohis_finances', 
                                                 'eurohis_activity', 
                                                 'eurohis_selfesteem', 
                                                 'eurohis_relationship', 
                                                 'eurohis_housing')))
  
  ## variable labels
  
  cons$var_labs <- cons$var_list %>% 
    map(~ifelse(stri_detect(.x, regex = '_q\\d+$'), 
                stri_extract(.x, regex = 'q\\d+$') %>% 
                  stri_replace(fixed = 'q', replacement = 'Item '), 
                exchange(.x,  
                         dict = ptsd$var_lexicon, 
                         value = 'label') %>% 
                  stri_replace(fixed = ' score', replacement = '')))
  
  ## variable pairs for correlation analysis
  
  cons$pair_list <- cons$var_list %>% 
    map(combn, m = 2, simplify = FALSE)
  
  ## analysis tables
  
  cons$analysis_tbl <- cons$var_list %>% 
    map(~ptsd$dataset[c('ID', .x)]) %>% 
    map(~filter(.x, complete.cases(.x)))

  cons$analysis_tbl$pss4_total <- ## inverting negative items 2 and 3 of PSS4
    cons$analysis_tbl$pss4_total %>% 
    mutate(pss4_q1 = as.numeric(pss4_q1) - 1, 
           pss4_q2 = as.numeric(fct_rev(pss4_q2)) - 1, 
           pss4_q3 = as.numeric(fct_rev(pss4_q3)) - 1, 
           pss4_q4 = as.numeric(pss4_q4) - 1)
  
  cons$analysis_tbl <- cons$analysis_tbl %>% 
    map(~map_dfc(.x, function(var) if(is.factor(var)) as.numeric(var) - 1 else var))

  ## long format conversion
  
  cons$analysis_tbl_long <- 
    map2(cons$analysis_tbl, 
         cons$var_list, 
         ~pivot_longer(data = .x, 
                       cols = all_of(.y), 
                       names_to = 'item', 
                       values_to = 'score'))

# Serial correlation: Spearman -------  
  
  insert_msg('Serial correlation by Spearman')

  cons$corr_results <- 
    future_map2(cons$analysis_tbl, 
                cons$pair_list, 
                function(data, pairs) pairs %>% 
                  map_dfr(~correlate_variables(data, 
                                               variables = .x, 
                                               what = 'correlation', 
                                               type = 'spearman', 
                                               pub_styled = FALSE, 
                                               adj_method = 'BH')), 
                .options = furrr_options(seed = TRUE))
  
  cons$corr_results <- cons$corr_results %>% 
    map(mutate, 
        variable1 = ifelse(stri_detect(variable1, regex = '_q\\d+$'), 
                           stri_extract(variable1, regex = 'q\\d+$') %>% 
                             stri_replace(fixed = 'q', replacement = 'Item '), 
                           exchange(variable1, 
                                    dict = ptsd$var_lexicon, 
                                    value = 'label') %>% 
                             stri_replace(fixed = ' score', replacement = '')), 
        variable2 = ifelse(stri_detect(variable2, regex = '_q\\d+$'), 
                           stri_extract(variable2, regex = 'q\\d+$') %>% 
                             stri_replace(fixed = 'q', replacement = 'Item '), 
                           exchange(variable2, 
                                    dict = ptsd$var_lexicon,
                                    value = 'label') %>% 
                             stri_replace(fixed = ' score', replacement = '')), 
        significant = ifelse(p_adjusted < 0.05, 'p < 0.05', 'ns'))
  
# Correlation coefficient plots ------
  
  insert_msg('Bubble plots with correlation coeffs')
  
  cons$corr_plots <- 
    list(data = cons$corr_results, 
         plot_title = exchange(names(cons$corr_results),
                               dict = ptsd$var_lexicon, 
                               value = 'label'), 
         lims = cons$var_labs) %>% 
    pmap(function(data, plot_title, lims) data %>% 
           ggplot(aes(x = variable1, 
                      y = variable2, 
                      size = abs(estimate), 
                      fill = estimate)) + 
           geom_point(shape = 21) + 
           geom_text(aes(label = ifelse(significant != 'ns', 
                                        signif(estimate, 2), 
                                        NA), 
                         color = estimate), 
                     size = 2.75, 
                     hjust = 0.5, 
                     vjust = 1.9) +
           scale_x_discrete(limits = lims) + 
           scale_y_discrete(limits = lims) + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'white', 
                                high = 'firebrick', 
                                midpoint = 0.5, 
                                limits = c(0, 1), 
                                name = expression(rho), 
                                oob = scales::squish) + 
           scale_color_gradient2(low = 'steelblue', 
                                 mid = 'black', 
                                 high = 'firebrick', 
                                 midpoint = 0.5, 
                                 limits = c(0, 1), 
                                 name = expression(rho), 
                                 oob = scales::squish) + 
           scale_size_continuous(range = c(1, 5), 
                                 limits = c(0, 1), 
                                 name = expression('abs(' * rho * ')')) + 
           globals$common_theme +
           theme(axis.title = element_blank(), 
                 axis.text.x = element_text(hjust = 1, angle = 90)) + 
           labs(title = plot_title, 
                subtitle = paste("Spearman's Correlation between the items, n =", 
                                 data$n[[1]])))

# PCA to identify optimal number of components of each score -------
  
  insert_msg('PCA and scree plots')
  
  ## PCA objects: maximal possible number of dimensions
  
  cons$pca_obj <- cons$analysis_tbl %>% 
    map(column_to_rownames, 'ID') %>% 
    map(~reduce_data(.x, 
                     kdim = ncol(.x) - 1, 
                     red_fun = 'pca'))
  
  ## scree plots
  
  cons$pca_scree <- cons$pca_obj %>% 
    map(plot, 
        type = 'scree', 
        cust_theme = globals$common_theme) %>% 
    map2(., 
         exchange(names(cons$pca_obj),
                  dict = ptsd$var_lexicon, 
                  value = 'label'), 
         ~.x + labs(title = .y))
  
  ## numbers of factors: manual entry based on the bend of the scree plot
  ## this is just the first shot, adjustments are based on the bend of the 
  ## variance curve of the factor analysis
  
  cons$pca_factor_n <- 
    c(dsm5_total = 4, 
      ptgi_total = 4, 
      rs13_total = 3, 
      brcs_total = 1, 
      cage_total = 1, 
      pss4_total = 1, 
      phq9_total = 4, 
      gad7_total = 3, 
      phqd_panic_total = 1, 
      phq_events_total = 4, 
      soc9l_total = 3, 
      eurohis_total = 4)
  
# Factor analysis ---------
  
  insert_msg('Factor analysis')
  
  ## factor analysis objects
  
  cons$fa_obj <- cons$analysis_tbl %>% 
    map(column_to_rownames, 'ID') %>% 
    map2(., cons$pca_factor_n, 
         ~reduce_data(.x, kdim = .y, red_fun = 'fa'))
  
  ## plots of the loadings with safely: one-component FA cannot be displayed
  
  cons$fa_loadings_plots <- cons$fa_obj %>% 
    map(safely(plot), 
        type = 'loadings', 
        cust_theme = globals$common_theme) %>% 
    map(~.x$result) %>% 
    compact
  
  cons$fa_loadings_plots <- cons$fa_loadings_plots %>% 
    map2(., 
         exchange(names(.),
                  dict = ptsd$var_lexicon, 
                  value = 'label'), 
         ~.x + labs(title = .y))
  
# Calculation of McDonald's omega -------
  
  insert_msg('Calculation of omegas')
  
  ## the warnings concern tools with multiple zeros 
  ## (CAGE is a good example for that)
  
  cons$omega_obj <- cons$analysis_tbl %>% 
    map(column_to_rownames, 'ID') %>% 
    map2(., cons$pca_factor_n, 
         ~omega(.x, 
                nfactors = .y, 
                fm = 'ml', 
                plot = FALSE))
  
  ## extracting the total omegas
  
  cons$omega_tbl <- cons$omega_obj %>% 
    map_dbl(~.x$omega.tot) %>% 
    compress(names_to = 'scale', 
             values_to = 'omega')
  
# Plotting the total omegas --------
  
  insert_msg('Plotting the omegas')
  
  cons$omega_plot <- cons$omega_tbl %>% 
    ggplot(aes(x = omega, 
               y = reorder(scale, omega))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             fill = 'steelblue') + 
    geom_text(aes(label = signif(omega, 2)), 
              size = 2.75, 
              hjust = -0.8) + 
    scale_y_discrete(labels = exchange(cons$omega_tbl$scale, 
                                       dict = ptsd$var_lexicon, 
                                       value = 'label')) + 
    scale_x_continuous(limits = c(0, 1)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Consistency of the assessment battery', 
         subtitle = "McDonald's omega", 
         x = "McDonald's \u03A9") + 
    expand_limits(x = 0.63)

# END -----
  
  plan('sequential')
  
  insert_tail()