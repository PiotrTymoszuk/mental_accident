# Modeling effects of the  interaction between the injury severity 
# and psychological support after the accident on flashbacks 
# (logistic regression), PTSD and PTG scoring (negative binomial GLM)

  insert_head()
  
# container ------
  
  mod <- list()
  
# analysis globals ------
  
  ## analysis responses
  
  mod$responses <- 
    c('dsm5_total', 
      'dsm5_B', 
      'dsm5_C', 
      'dsm5_D', 
      'dsm5_E', 
      'ptgi_total', 
      'ptgi_fctI', 
      'ptgi_fctII', 
      'ptgi_fctIII', 
      'ptgi_fctIV', 
      'ptgi_fctV', 
      'unwilling_flashback')
  
  ## explanatory variables
  
  mod$variables <- 
    c('injury_sev_strata', 
      'psych_support_post_accident', 
      'accident_rescue', 
      'age', 
      'sex', 
      'psych_comorbidity', 
      'accident_aftermath', 
      'traumatic_event')
  
  ## formulas
  
  mod$formulas <- 
    mod$responses %>% 
    paste('~', paste(mod$variables, collapse = ' + ')) %>% 
    map(as.formula) %>% 
    set_names(mod$responses)
  
  ## modeling table
  
  mod$analysis_tbl <- ptsd$dataset %>% 
    select(ID, 
           all_of(mod$responses), 
           all_of(mod$variables)) %>% 
    mutate(age = cut(age, 
                     c(-Inf, 30, 65, Inf), 
                     c('young', 'middle', 'elderly')), 
           accident_rescue = car::recode(as.character(accident_rescue), 
                                         "'self' = 'self/partner'; 
                                         'tour partner' = 'self/partner'; 
                                         'third party' = 'self/partner'; 
                                         'rescue team' = 'professional'"), 
           accident_rescue = factor(accident_rescue, 
                                    c('self/partner', 'professional'))) %>% 
    filter(complete.cases(.))
  
# Construction of the full models -------
  
  insert_msg('Construction of the full models')
  
  mod$full_models <- 
    list(formula = mod$formulas, 
         mod_fun = list(glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm.nb, 
                        glm), 
         family = list(NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL, 
                       NULL,
                       NULL, 
                       NULL, 
                       NULL, 
                       'binomial')) %>% 
    pmap(make_lm, 
         data = mod$analysis_tbl)
  
# Backward elimination ------
  
  insert_msg('Backward elimination')
  
  mod$final_models <- mod$full_models %>% 
    map(step, step_fun = stepAIC)
  
# model assumptions and fit stats -------
  
  insert_msg('Assumptions and stats')
  
  ## assumptions
  
  mod$assumptions <- mod$final_models %>% 
    map(summary, 
        'assumptions', 
        type.residuals = 'deviance')
  
  ## plots of residuals
  
  mod$resid_plots <- mod$final_models %>% 
    map(plot, 
        type.residuals = 'deviance', 
        cust_theme = globals$common_theme)
  
  ## fit stats and captions for the Forest plots
  
  mod$fit <- mod$final_models %>% 
    map_dfr(summary, 'fit') %>% 
    mutate(plot_cap = paste0('R\u00B2 = ', signif(raw_rsq, 2), 
                             ', n = ', n_complete))
  
# cross-validation with Caret -------
  
  insert_msg('Cross-validation with caret')
  
  set.seed(1234)
  
  registerDoParallel(cores = 7)
  
  mod$caret_obj <- mod$final_models[1:11] %>% 
    map(formula) %>% 
    map(train, 
        data = mod$analysis_tbl, 
        method = 'glm.nb', 
        trControl = trainControl(method = 'cv', 
                                 number = 10, 
                                 savePredictions = 'final', 
                                 returnData = TRUE, 
                                 returnResamp = 'final'), 
        tuneGrid = data.frame(link = 'log')) 
  
  mod$caret_obj['unwilling_flashback'] <- 
    mod$final_models['unwilling_flashback'] %>% 
    map(formula) %>% 
    map(train, 
        data = mod$analysis_tbl, 
        method = 'glm', 
        trControl = trainControl(method = 'cv', 
                                 number = 10, 
                                 savePredictions = 'final', 
                                 returnData = TRUE, 
                                 returnResamp = 'final', 
                                 classProbs = TRUE), 
        family = 'binomial') 

  stopImplicitCluster()
  
  ## caretex objects to get additional fit stats
  
  mod$caret_obj <- mod$caret_obj %>% 
    map(as_caretx)

  mod$caret_obj$dsm5_total %>% summary
  
# Training and cross-validation fit stats --------
  
  insert_msg('Training and CV fit stats')
  
  ## fails for the PTGI IV factor: seems no to have enough different
  ## predicted values
  
  mod$cv_stats <- mod$caret_obj %>% 
    map(safely(summary)) %>% 
    map(~.x$result) %>% 
    compact %>% 
    map(~map(.x, ~.x[c(3, 4), ])) %>% 
    map(compress, names_to = 'dataset') %>% 
    compress(names_to = 'response') %>% 
    mutate(estimate = ifelse(estimate < 0, 0, estimate))
  
# cross-validation fit stat plots -----
  
  insert_msg('Plotting the CV stats')
  
  mod$cv_plots <- 
    list(x = mod$cv_stats %>% 
           dlply('statistic'), 
         y = c('Model error', 
               'Model explanatory performance'), 
         z = c('RMSE', 'R\u00B2')) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = estimate, 
                      y = response, 
                      fill = dataset)) + 
           geom_bar(stat = 'identity', 
                    color = 'gray20', 
                    position = position_dodge(0.9)) + 
           scale_fill_manual(values = c(train = 'steelblue', 
                                        cv = 'darkolivegreen4'), 
                             labels = c(train = 'training', 
                                        cv = '10-fold CV')) + 
           scale_y_discrete(limits = rev(mod$responses), 
                            labels = rev(mod$responses) %>% 
                              exchange(dict = ptsd$var_lexicon, 
                                       key = 'variable', 
                                       value = 'label')) +
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = z))
  
# Model inference -----
  
  insert_msg('Model inference')
  
  ## computing exp estimates
  ## explanatory variable values
  
  mod$inference <- mod$final_models %>% 
    map(summary) %>% 
    map(mutate, 
        n = ifelse(is.na(n), n_complete, n), 
        exp_estimate = exp(estimate), 
        exp_lower = exp(lower_ci), 
        exp_upper = exp(upper_ci), 
        plot_lab = paste0(signif(exp_estimate, 2), 
                          ' [', signif(exp_lower, 2), 
                          ' - ', signif(exp_upper, 2), ']'), 
        var_label = exchange(variable, 
                             dict = ptsd$var_lexicon, 
                             key = 'variable', 
                             value = 'label'))
  
# Forest plots ------
  
  insert_msg('Forest plots')
  
  mod$forest_plots <- 
    list(x = mod$inference, 
         plot_title = names(mod$inference) %>% 
           exchange(dict = ptsd$var_lexicon, 
                    key = 'variable', 
                    value = 'label') %>% 
           stri_capitalize, 
         plot_subtitle = mod$fit$plot_cap, 
         x_lab = c(rep('log \u03B2, 95% CI', 11), 
                   'log OR, 95% CI')) %>% 
    pmap(plot_forest, 
         variable = 'var_label', 
         cust_theme = globals$common_theme)
  
  ## a manual hack to get the exp estimates printed in the plot
  
  for(i in names(mod$fore)) {
    
    mod$forest_plots[[i]]$data <- 
      mod$forest_plots[[i]]$data %>% 
      mutate(exp_estimate = exp(estimate), 
             exp_lower = exp(lower_ci), 
             exp_upper = exp(upper_ci), 
             estimate_lab = paste0(signif(exp_estimate, 2), 
                                   ' [', signif(exp_lower, 2), 
                                   ' - ', signif(exp_upper, 2), ']'))
  }
  
# END ------
  
  rm(i)
  
  insert_tail()