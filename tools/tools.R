# A medley of functional project tools

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(glue)
  library(ggtext)
  library(stringi)
  library(rstatix)
  library(rmarkdown)
  library(bookdown)
  library(knitr)
  library(clustTools)
  library(proxy)
  library(flextable)
  
# data import and transformation -----
  
  recode_var <- function(data, 
                         old_var, 
                         new_var, 
                         ID_var = 'Passwort', 
                         trans_fun = 'my_identity', 
                         args = NULL) {
    
    ## renames a variable, if time and id variable names provided, they're included
    ## in  the output as well
    ## the transformation function enables
    ## args specifies the arguments to the transforming function
    
    trans_call <- call2(trans_fun, 
                        data[[old_var]], 
                        !!!args)
    
    new_data <- data %>% 
      transmute(!!ID_var := .data[[ID_var]], 
                !!new_var := eval(trans_call))

  }
  
  repl_string <- function(vector,
                          regex, 
                          replacement = '', 
                          out_format = c('character', 'numeric', 'factor', 'date'), ...) {
    
    ## removes the specified regex from the text vectos
    
    out_format <- match.arg(out_format[1], 
                            c('character', 'numeric', 'factor', 'date'))
    
    vector <- as.character(vector) %>% 
      stri_replace(regex = regex, 
                   replacement = replacement)
    
    switch(out_format, 
           character = as.character(vector), 
           numeric = as.numeric(vector), 
           factor = factor(vector), 
           date = as.Date(as.character(vector, 
                                       tryFormats = c("%Y-%m-%d", 
                                                      "%Y/%m/%d", 
                                                      "%d.%m.%Y"))))
    
  }
  
  rm_spaces <- function(vector, 
                        out_format = c('character', 'numeric', 'factor', 'date'), ...) {
    
    ## removes extra trailing spaces
    
    out_format <- match.arg(out_format[1], 
                            c('character', 'numeric', 'factor', 'date'))
    
    repl_string(vector = vector, 
                regex = '\\s+$', replacement = '', 
                out_format = out_format)
    
  }
  
  rm_string <- function(vector, 
                       pattern, 
                       out_format = c('character', 'numeric', 'factor', 'date'), ...) {
    
    
    repl_string(vector = rm_spaces(vector, 'character'), 
                regex = pattern, 
                replacement = '', 
                out_format = out_format)
    
  }
  
  my_date <- function(vector, format = '%d.%m.%Y', ...) {
    
    vector %>% 
      rm_spaces(out_format = 'character') %>% 
      as.Date(format = format)
    
  }
  
  recode_vec <- function(vector, 
                         recodes, 
                         out_format = c('character', 'numeric', 'factor', 'date'), ...) {
    
    vector <- car::recode(rm_spaces(vector, 'character'), 
                          recodes = recodes)
    
    switch(out_format, 
           character = as.character(vector), 
           numeric = as.numeric(vector), 
           factor = factor(vector), 
           date = as.Date(as.character(vector, 
                                       tryFormats = c("%Y-%m-%d", 
                                                      "%Y/%m/%d", 
                                                      "%d.%m.%Y"))))
    
  }
  
  recode_yn <- function(vector, reverse = FALSE, as.factor = TRUE, ...) {
    
    ## recodes a 0/1 vector no/yes or the other way round
    
    if(reverse) {
      
      new_vec <- recode_vec(vector, 
                            "'yes' = 1;
                             'no' = 0",
                            as.factor = as.factor)
      
      #if(!as_factor) {
        
       # new_vec <- as.numeric(new_vec)
        
      #}
      
    } else {
      
      new_vec <- recode_vec(vector, 
                            "1 = 'yes';
                             0 = 'no'", 
                            as.factor = as.factor)
      
    }
    
    #if(as_factor) {
      
     # new_vec <- factor(new_vec)
      
    #}
    
    return(new_vec)
    
  }
  
  binarize <- function(vector, cutoff, labels = "'yes';'no'") {
    
    return(cut(vector, 
               c(-Inf, cutoff, Inf), 
               unlist(stri_split_fixed(labels, ';'))))
    
    
  }
  
  my_identity <- function(x, ...) {
    
    identity(x)
    
  }
  
  my_factor <- function(x, levels, ...) {
    
    factor(rm_spaces(x, 'character'), 
           levels =  unlist(stri_split_fixed(levels, ';')))
    
  }
  
  pss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")
  
# randomization ------

  partition_similarity <- function(data, test_id, 
                                   method = 'cosine', 
                                   to_matrix = TRUE, 
                                   cross_only = TRUE) {
    
    ## computes similarity between the training and test subset
    
    data_lst <- list(train = data[!rownames(data) %in% test_id, ], 
                     test = data[test_id, ]) 
    
    if(to_matrix) {
      
      data_lst <- data_lst %>% 
        map(as.matrix)
      
    }
    
    ## distances
    
    if(!cross_only) {
      
      return(tibble(within_train = mean(proxy::dist(data_lst[[1]], 
                                                    method = method), 
                                        na.rm = TRUE), 
                    within_test = mean(proxy::dist(data_lst[[1]], 
                                                   method = method), 
                                       na.rm = TRUE), 
                    cross = mean(proxy::dist(data_lst[[1]], 
                                             data_lst[[2]], 
                                             method = method), 
                                 na.rm = TRUE)))
      
    } else {
      
      return(tibble(within_train = NA, 
                    within_test = NA, 
                    cross = mean(proxy::dist(data_lst[[1]], 
                                             data_lst[[2]], 
                                             method = method), 
                                 na.rm = TRUE)))
      
    }
    
  }
  
# plot axis labels with n numbers ------
  
  label_n <- function(data, split_factor, sep = '\nn = ') {
    
    split_expr <- enexpr(split_factor)
    
    counts <- count(data, !!split_expr)
    
    map2_chr(counts[[1]], counts[[2]], paste, sep = sep)
    
  }

# variable:label translation, color setup -----

  set_colors_ <- function(color_no, seed = 123) {
    
    ## picks n colors at random from the standard palette
    
    set.seed(seed)
    
    return(colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] %>% 
             sample(size = color_no))
    
  }
  
  symptom_ax_labs <- function(test_data, variables) {
    
    ax_labs <- test_data %>% 
      filter(.data[['variable']] %in% variables) %>% 
      mutate(var_lab = exchange(variable, ptsd$var_lexicon), 
             var_lab = stri_replace(var_lab, 
                                    fixed = ' (', 
                                    replacement = '<br>('), 
             var_lab = stri_replace(var_lab, 
                                    fixed = 'clinically relevant', 
                                    replacement = 'clinically relevant<br>'), 
             var_lab = stri_replace(var_lab, 
                                    fixed = ' during', 
                                    replacement = '<br>during'), 
             var_lab = stri_replace(var_lab, 
                                    fixed = 'at least one PCL-5 domain positive', 
                                    replacement = 'at least one domain'), 
             var_lab = paste(var_lab, plot_cap, sep = '<br>'), 
             var_lab = ifelse(p_adjusted < 0.05, 
                              paste0('<b>', var_lab, '</b>'), 
                              var_lab))
    
    set_names(ax_labs$var_lab, ax_labs$variable)
    
  }
  
# Frequency computation and plots ------
  
  count_binary <- function(data) {
    
    ## counting of binary factors
    
    data <-  data %>% 
      map_dfc(~as.numeric(.x) - 1) %>% 
      filter(complete.cases(.))
    
    data %>% 
      colSums %>% 
      compress(names_to = 'variable',
               values_to = 'n') %>% 
      mutate(n_total = nrow(data), 
             fraction = n/n_total, 
             percent = fraction * 100)
    
  }
  
  plot_freq_bars <- function(data, 
                             freq_var = 'percent', 
                             cat_var = 'variable', 
                             split_factor = NULL, 
                             plot_title = NULL, 
                             plot_subtitle = NULL, 
                             x_lab = '% of cohort', 
                             bar_color = 'steelblue', 
                             fill_scale = scale_fill_brewer(), 
                             color_scale = scale_color_brewer(), 
                             show_freqs = TRUE, ...) {
    
    ## plots a simple bar plot of frequencies
    
    if(is.null(split_factor)) {
      
      freq_plot <- data %>% 
        ggplot(aes(x = .data[[freq_var]], 
                   y = .data[[cat_var]])) + 
        geom_bar(stat = 'identity', 
                 color = 'gray20', 
                 fill = bar_color)
      
    } else {
      
      freq_plot <- data %>% 
        ggplot(aes(x = .data[[freq_var]], 
                   y = .data[[cat_var]], 
                   fill = .data[[split_factor]])) + 
        geom_bar(stat = 'identity', 
                 color = 'gray20', 
                 position = position_dodge(0.9)) + 
        fill_scale
      
    }
    
    freq_plot <- freq_plot + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title,
           subtitle = plot_subtitle, 
           x = x_lab)
    
    if(show_freqs) {
      
      if(!is.null(split_factor)) {
        
        freq_plot <- freq_plot + 
          geom_text(aes(label = signif(.data[[freq_var]], 2), 
                        color = .data[[split_factor]]), ...) + 
          color_scale
        
      } else {
        
        freq_plot <- freq_plot + 
          geom_text(aes(label = signif(.data[[freq_var]], 2)), ...)
        
      }
      
    }
    
    return(freq_plot)
    
  }
  
  plot_ptsd_freq <- function(data, 
                             test_data, 
                             variables = c('dsm5_cluster_class', 
                                           'dsm5_B_class', 
                                           'dsm5_C_class', 
                                           'dsm5_D_class', 
                                           'dsm5_E_class'), 
                             ptsd_labs = TRUE, 
                             split_factor = 'psych_comorbidity', 
                             plot_title = 'PCL-5 DSM-5 clusters', 
                             plot_subtitle = NULL, 
                             x_lab =  '% of mental illness strata', 
                             fill_scale = scale_fill_brewer(), 
                             color_scale = scale_color_brewer()) {
    
    
    ## plots frequency of participants screened positive for the PTSD
    ## clusters split by a factor
    
    ## variables and frequencies

    frequency <-  data %>% 
      dlply(split_factor, 
            select, 
            all_of(variables)) %>% 
      map(count_binary) %>% 
      compress(names_to = split_factor) %>% 
      mutate(!!split_factor := factor(.data[[split_factor]], 
                                      levels(data[[split_factor]])))
    
    ## plot
    
    bar_plot <- 
      plot_freq_bars(data = frequency, 
                     freq_var = 'percent', 
                     cat_var = 'variable', 
                     split_factor = split_factor, 
                     plot_title = plot_title, 
                     plot_subtitle = plot_subtitle, 
                     x_lab = x_lab, 
                     fill_scale = fill_scale, 
                     color_scale = color_scale, 
                     show_freqs = TRUE, 
                     position = position_dodge(0.9), 
                     size = 2.5, 
                     hjust = -0.4)
    
    if(!ptsd_labs) {
      
      return(bar_plot)
      
    }
    
    ## Y axis labels
    
    ax_labs <- test_data %>% 
      filter(.data[['variable']] %in% variables) %>% 
      mutate(ax_lab = variable %>% 
               stri_extract(regex = 'B|C|D|E'), 
             ax_lab = ifelse(is.na(ax_lab), 'PTSD+', ax_lab), 
             ax_lab = paste(ax_lab, plot_cap, sep = '<br>'), 
             ax_lab = ifelse(p_adjusted < 0.05, 
                             paste0('<b>', ax_lab, '</b>'), 
                             ax_lab))
    
    ax_labs <- set_names(ax_labs$ax_lab, 
                         ax_labs$variable)
    
    bar_plot + 
      scale_y_discrete(limits = rev(variables), 
                       labels = ax_labs) + 
      theme(axis.text.y = element_markdown())
    
  }
  
  plot_body_freq <- function(data, 
                             test_data, 
                             split_factor, 
                             plot_title = 'Injured body regions', 
                             x_lab = '% of strata', 
                             fill_scale = scale_fill_brewer(), 
                             color_scale = scale_color_brewer()) {
    
    
    ## variables and frequencies
    
    variables <- 
      c('injury_head', 'injury_face', 'injury_neck', 'injury_chest', 
        'injury_abdomen', 'injury_spine', 'injury_upper_limbs', 
        'injury_lower_limbs', 'injury_external_other')
    
    frequency <- data %>% 
      dlply(split_factor, 
            select, 
            all_of(variables)) %>% 
      map(count_binary) %>% 
      compress(names_to = split_factor) %>% 
      mutate(!!split_factor := factor(.data[[split_factor]], 
                                      levels(data[[split_factor]])))
    
    ## labels with effect sizes and significance
    
    ax_labs <- test_data %>% 
      filter(.data[['variable']] %in% variables) %>% 
      mutate(ax_lab = exchange(variable, 
                               dict = ptsd$var_lexicon, 
                               key = 'variable', 
                               value = 'label'), 
             ax_lab = stri_replace(ax_lab, 
                                   regex = '\\s{1}injury$', 
                                   replacement = ''), 
             ax_lab = paste(ax_lab, plot_cap, sep = '<br>'), 
             ax_lab = ifelse(p_adjusted < 0.05, 
                             paste0('<b>', ax_lab, '</b>'), 
                             ax_lab))
    
    ax_labs <- set_names(ax_labs$ax_lab, 
                         ax_labs$variable)
    
    ## plot
    
    plot_freq_bars(data = frequency, 
                   freq_var = 'percent', 
                   cat_var = 'variable', 
                   split_factor = split_factor, 
                   plot_title = plot_title, 
                   x_lab = x_lab, 
                   fill_scale = fill_scale, 
                   color_scale = color_scale, 
                   show_freqs = TRUE, 
                   position = position_dodge(0.9), 
                   size = 2.5, 
                   hjust = -0.4) + 
      scale_y_discrete(limits = rev(variables), 
                       labels = ax_labs) + 
      theme(axis.text.y = element_markdown())
    
  }
  
# Factor effect testing and violin plot panels -------
  
  format_fct_test <- function(test_data) {
    
    test_data %>% 
      mutate(plot_cap = paste(eff_size, significance, sep = ', '), 
             plot_type = ifelse(test == 'Chi-squared test', 
                                'stack', 'violin'), 
             y_lab = ifelse(test == 'Chi-squared test', 
                            '% of strata', 
                            exchange(variable, 
                                     dict = ptsd$var_lexicon, 
                                     key = 'variable', 
                                     value = 'axis_lab')))
    
  }
  
  plot_fct <- function(data, 
                       test_data, 
                       factor = 'age', 
                       fill_scale = scale_fill_brewer()) {
    
    ## draws a series of violin or stack plots
    
    list(variable = test_data[['variable']], 
         plot_title = exchange(test_data[['variable']], 
                               dict = ptsd$var_lexicon, 
                               key = 'variable', 
                               value = 'label') %>% 
           stri_capitalize, 
         plot_subtitle = test_data[['plot_cap']], 
         type = test_data[['plot_type']], 
         y_lab = test_data[['y_lab']]) %>% 
      pmap(plot_variable, 
           data, 
           split_factor = factor, 
           scale = 'percent', 
           cust_theme = globals$common_theme, 
           x_n_labs = TRUE, 
           txt_size = 2.25) %>% 
      map(~.x + 
            fill_scale + 
            theme(axis.title.x = element_blank())) %>% 
      set_names(test_data[['variable']])
    
  }
  
  plot_fct_panels <- function(data, 
                              test_data, 
                              factor = 'age', 
                              fill_scale = scale_fill_brewer()) {
    
    ## generates violin plot panels
    ## for the QoL, PTSD and PTG domains
    ## split by the given factor
    
    ## plotting variables
    
    variables <- 
      list(eurohis = c('eurohis_qol', 
                       'eurohis_health', 
                       'eurohis_energy', 
                       'eurohis_finances', 
                       'eurohis_activity', 
                       'eurohis_selfesteem', 
                       'eurohis_relationship', 
                       'eurohis_housing'), 
           dsm = c('dsm5_B', 
                   'dsm5_C', 
                   'dsm5_D', 
                   'dsm5_E'), 
           ptgi = c('ptgi_fctI', 
                    'ptgi_fctII', 
                    'ptgi_fctIII', 
                    'ptgi_fctIV', 
                    'ptgi_fctV'))
    
    ## X axis labels with effect sizes and significance
    
    ax_labs <- variables %>% 
      exchange(dict = ptsd$var_lexicon, 
               key = 'variable', 
               value = 'label') %>% 
      map(stri_replace, 
          regex = '\\s{1}score$', 
          replacement = '') %>% 
      map(stri_replace, 
          regex = '.*\\s{1}.*\\s{1}', 
          replacement = '')
    
    stats <- variables %>% 
      map(~filter(test_data, variable %in% .x)) %>% 
      map2(., variables, 
           ~mutate(.x, variable = factor(variable, .y))) %>% 
      map(arrange, variable) %>% 
      map(~.x$plot_cap)
    
    ax_labs <- 
      map2(ax_labs, stats, 
           paste, sep = '<br>')

    ax_labs <- ax_labs %>% 
      map(~ifelse(stri_detect(.x, fixed = 'ns ('), 
                  .x, 
                  paste0('<b>', .x, '</b>')))
    
    ## plot panels
    
    plots <- 
      list(variables = variables, 
           plot_title = c('EUROHIS QOL domains', 
                          'PCL-5 DSM-5 clusters', 
                          'PTGI factors')) %>% 
      pmap(draw_violin_panel, 
           data = data, 
           split_factor = factor, 
           distr_geom = 'violin', 
           point_alpha = 0.85, 
           point_hjitter = 0.1, 
           point_wjitter = 0.05, 
           point_size = 1, 
           cust_theme = globals$common_theme, 
           x_lab = 'Score', 
           plot_subtitle = data %>% 
             label_n(.data[[factor]], sep = ': n = ') %>% 
             paste(collapse = ', '), 
           scale = 'width', 
           dodge_w = 0.9)
    
    list(x = plots, 
         y = variables, 
         z = ax_labs) %>% 
      pmap(function(x, y, z) x + 
             fill_scale + 
             scale_y_discrete(limits = rev(y), 
                              labels = rev(z)) + 
             theme(axis.text.y = element_markdown()))
    
  }
  
# Ribbon plots ------
  
  split_ribbon <- function(plot_lst, 
                           lex_lst, 
                           variables, 
                           title_prefix) {
    
    ## splits ribbon plots of the psychometric scores
    
    for(i in names(plot_lst)) {
      
      plot_lst[[i]]$data <- 
        plot_lst[[i]]$data %>% 
        filter(variable %in% variables)
      
    }
    
    list(x = plot_lst, 
         y = paste(title_prefix, 
                   c('training', 'test'), 
                   sep = ', '), 
         z = lex_lst) %>% 
      pmap(function(x, y, z) x + 
             scale_y_discrete(limits = rev(variables), 
                              labels = function(x) exchange(x, dict = z)) + 
             labs(title = y))
    
  }
  
# Classifiers ------
  
  ptb_tuner <- function(data, lev = NULL, model = NULL) {
    
    ## a function for tuning the caret's train
    ## to detect the PTS cluster with the biggest reliability
    
    new_dat <- 
      dplyr::transmute(data, 
                       obs = ifelse(obs == 'PTS', 'yes', 'no'), 
                       pred = ifelse(pred == 'PTS', 'yes', 'no'), 
                       accuracy = obs == pred, 
                       true_pos = (obs == 'yes' & pred == 'yes'), 
                       false_pos = (obs == 'no' & pred == 'yes'), 
                       true_neg = (obs == 'no' & pred == 'no'), 
                       false_neg = (obs == 'yes' & pred == 'no'))
    
    stats <- purrr::map_dfc(new_dat[c('accuracy', 
                                      'true_pos', 'false_pos', 
                                      'true_neg', 'false_neg')], 
                            sum)
    
    stats <- dplyr::mutate(stats, 
                           accuracy = accuracy/nrow(new_dat), 
                           sens = true_pos/(true_pos + false_neg), 
                           spec = true_neg/(true_neg + false_pos))
    
    unlist(as.vector(stats))
    
  }
  
  plot_overall_stats <- function(data, 
                                 y_var, 
                                 title_prefix = 'Mental cluster prediction,', 
                                 plot_subtitle = NULL) {
    
    list(x = c('kappa', 'correct_rate', 'brier_score'), 
         y = paste(title_prefix, 
                   c("\u03BA", 'accuracy', 'Brier score')), 
         z = c("Cohen's \u03BA", 'Accuracy', 'Brier score')) %>% 
      pmap(function(x, y, z) data %>% 
             ggplot(aes(x = .data[[x]], 
                        y = reorder(.data[[y_var]], .data[[x]]), 
                        fill = dataset)) + 
             geom_bar(stat = 'identity', 
                      color = 'black', 
                      position = position_dodge(0.9)) + 
             scale_fill_manual(values = globals$part_colors, 
                               labels = globals$part_labels,
                               name = 'Data subset') + 
             scale_y_discrete(labels = class_globals$predictor_labs) + 
             globals$common_theme + 
             theme(axis.title.y = element_blank()) + 
             labs(title = y, 
                  x = z)) %>% 
      set_names(c('kappa', 'correct_rate', 'bs'))
    
  }
  
  plot_cluster_stats <- function(data, 
                                 y_var, 
                                 title_prefix = 'Cluster detection', 
                                 y_lab = 'Algorithm') {
    
    list(x = c('Se', 'Sp', 'correct_rate'), 
         y = paste(title_prefix, 
                   c('sensitivity', 'specificity', 'accuracy')), 
         z = c('Sensitivity', 'Specificity', 'Accuracy')) %>% 
      pmap(function(x, y, z) data %>% 
             ggplot(aes(x = clust_id, 
                        y = reorder(.data[[y_var]], .data[[x]]), 
                        fill = .data[[x]], 
                        size = .data[[x]])) + 
             geom_point(shape = 21) + 
             geom_text(aes(label = signif(.data[[x]], 2), 
                           color = .data[[x]]), 
                       size = 2.75,
                       hjust = 0.5, 
                       vjust = -1.8, 
                       show.legend = FALSE) + 
             scale_size_area(max_size = 4.5, 
                             limits = c(0, 1), 
                             name = z) + 
             scale_color_gradient2(low = 'steelblue', 
                                   mid = 'black', 
                                   high = 'firebrick', 
                                   midpoint = 0.5, 
                                   limits = c(0, 1)) + 
             scale_fill_gradient2(low = 'steelblue', 
                                  mid = 'black', 
                                  high = 'firebrick', 
                                  midpoint = 0.5, 
                                  limits = c(0, 1), 
                                  name = z) + 
             scale_y_discrete(labels = class_globals$predictor_labs) + 
             guides(fill = 'legend', 
                    size = 'legend') + 
             globals$common_theme + 
             facet_grid(. ~ dataset, 
                        labeller = as_labeller(globals$part_labels)) + 
             labs(title = y, 
                  x = 'Mental cluster', 
                  y = y_lab)) %>% 
      set_names(c('sensitivity', 'specificity', 'accuaracy'))
    
    
  }
  
  make_confusion_caps <- function(stats, models, 
                                  split_factor = 'method', 
                                  label_vector = class_globals$algo_labs) {
    
    ## makes ready-to-use captions for confusion heat maps
    
    caps <- stats %>% 
      mutate(!!split_factor := factor(.data[[split_factor]], names(models)), 
             dataset = factor(dataset, c('train', 'cv', 'test')), 
             plot_cap = paste0('Accuracy = ', signif(correct_rate, 2), 
                               ', \u03BA = ', signif(kappa, 2), 
                               ', BS = ', signif(brier_score, 2))) %>% 
      blast(all_of(split_factor)) %>% 
      map(blast, dataset) %>% 
      map(map, ~.x$plot_cap)
    
    for(i in names(caps)) {
      
      caps[[i]] <- 
        map2(caps[[i]], 
             c(nrow(class_globals$analysis_tbl$training), 
               nrow(class_globals$analysis_tbl$training), 
               nrow(class_globals$analysis_tbl$test)), 
             paste, sep = ', n = ')
      
    }
    
    titles <- 
      label_vector[names(caps)] %>% 
      map(function(x) globals$part_labels[names(caps[[1]])] %>% 
            map(~paste(x, .x, sep = ', ')))
    
    list(caps = caps, 
         titles = titles)
    
  }
  
  get_importance <- function(models) {
    
    ## retrieves and formats importance measures
    
    var_regex <- paste(sort(class_globals$variables$full, 
                            decreasing = TRUE), 
                       collapse = '|')
    
    imp_stats <- models %>% 
      map(varImp) %>% 
      map(~.x$importance) %>% 
      map(as.data.frame) %>% 
      map(rownames_to_column, 'parameter') %>% 
      map(mutate, 
          variable = stri_extract(parameter, 
                                  regex = var_regex), 
          level = stri_replace(parameter, 
                               regex = var_regex, 
                               replacement = ''), 
          var_label = exchange(variable, 
                               ptsd$var_lexicon), 
          plot_lab = ifelse(level == '', 
                            var_label, 
                            paste(var_label, level, sep = ': '))) %>% 
      map(as_tibble)
    
    ## for the svmRadial and DA algorithms: I'm calculating the overall 
    ## importance as a mean of importance for particular clusters
    
    ovr_sets <- imp_stats %>% 
      map(names) %>% 
      map_lgl(function(x) !'Overall' %in% x)
    
    imp_stats[c(ovr_sets)] <- imp_stats[c(ovr_sets)] %>% 
      map(~mutate(.x, 
                  Overall = reduce(.x[c('neutral', 'PTG', 'PTS')], 
                                   `+`), 
                  Overall = Overall/3))

    return(imp_stats)
    
  }
  
  plot_importance <- function(imp_stats, top = 20) {
    
    list(x = imp_stats %>% 
           map(top_n, top, Overall), 
         y = paste('Variable importance,', 
                   class_globals$algo_labs[names(full_class$importance$stats)]), 
         z = class_globals$algo_colors[names(full_class$importance$stats)]) %>% 
      pmap(function(x, y, z) x %>% 
             ggplot(aes(x = Overall, 
                        y = reorder(plot_lab, Overall))) + 
             geom_bar(stat = 'identity', 
                      fill = z, 
                      color = 'black') + 
             scale_y_discrete(labels = function(x) stri_replace(x, 
                                                                regex = ':\\s{1}(yes|no)$', 
                                                                replacement = '')) + 
             globals$common_theme + 
             theme(axis.title.y = element_blank()) + 
             labs(title = y, 
                  x = 'Overall variable importance'))
    
  }
  
  plot_kappa_bs <- function(data, 
                            plot_title = NULL, 
                            plot_subtitle = NULL, 
                            palette = class_globals$algo_colors, 
                            labels = class_globals$algo_labs) {
    
    ## plotting data frames and metadata
    
    n_numbers <- 
      class_globals$analysis_tbl[c("training", "training", "test")] %>% 
      map_dbl(nrow)
    
    data <- data %>% 
      blast(dataset)
    
    ## plots
    
    list(x = data, 
         y = globals$part_labels[names(data)], 
         z = n_numbers) %>% 
      pmap(function(x, y, z) x %>% 
             ggplot(aes(x = kappa, 
                        y = 2 - brier_score, 
                        fill = method, 
                        size = correct_rate)) + 
             geom_vline(xintercept = 0, 
                        linetype = 'dashed') + 
             geom_hline(yintercept = 0.66, 
                        linetype = 'dashed') + 
             geom_point(shape = 21, 
                        color = 'black') + 
             geom_text_repel(aes(label = labels[method], 
                                 color = method), 
                             size = 2.75, 
                             show.legend = FALSE, 
                             box.padding = 0.4) + 
             scale_fill_manual(values = palette, 
                               labels = labels, 
                               name = 'Algorithm') + 
             scale_color_manual(values = palette, 
                                labels = labels, 
                                name = 'Algorithm') + 
             scale_size_area(max_size = 4.5) + 
             globals$common_theme + 
             labs(title = y, 
                  subtitle = paste('n =', z), 
                  x = "Cohen's \u03BA", 
                  y = '2 - Brier score'))
    
    
  }
  
  plot_ptb_roc <- function(predictions, 
                           stats, 
                           cluster = 'PTS', 
                           rev_levels = FALSE, 
                           title_prefix = 'PTS cluster', 
                           palette = class_globals$algo_colors, 
                           labels = class_globals$algo_labs, 
                           numeric = FALSE, 
                           x_pos = 0.42, 
                           y_offset = 0.057, 
                           txt_size = 2.75, ...) {
    
    ## plots ROC curves for detection of the PTS cluster in the 
    ## training, CV and test subsets
    
    ## stats: total observations and observations in the PTS cluster
    
    total_n <- predictions[[1]] %>% 
      map(~.x$data) %>% 
      map_dbl(nrow)
    
    pts_n <- predictions[[1]] %>% 
      map(~.x$data) %>% 
      map(filter,.outcome == cluster) %>% 
      map_dbl(nrow)
    
    if(rev_levels) {
      
      n_caps <- 
        map2(total_n, pts_n, 
             ~paste0('total: n = ', .x, 
                     ', cluster: n = ', .x - .y))
      
    } else {
      
      n_caps <- 
        map2(total_n, pts_n, 
             ~paste0('total: n = ', .x, 
                     ', ', title_prefix, ': n = ', .y))
      
    }
    
    
    
    ## stats: sensitivity and specificity
    
    roc_stats <- stats %>% 
      filter(clust_id == cluster)
    
    if(rev_levels) {
      
      roc_stats <- roc_stats %>% 
        mutate(plot_lab = paste0('Se = ', signif(Sp, 2), 
                                 ', Sp = ', signif(Se, 2)))
      
    } else {
      
      roc_stats <- roc_stats %>% 
        mutate(plot_lab = paste0('Se = ', signif(Se, 2), 
                                 ', Sp = ', signif(Sp, 2)))
      
    }
    
    roc_stats <- roc_stats %>% 
      mutate(color = palette[method], 
             method = labels[method], 
             plot_lab = paste(method, plot_lab, sep = ': ')) %>% 
      blast(dataset) %>% 
      map(~mutate(.x, 
                  y_pos = (0:(nrow(.x) - 1)) * y_offset))
    
    ## plotting data
    
    if(rev_levels) levs <- c(cluster, 'rest') else levs <- c('rest', cluster)
    
    data <- predictions %>% 
      map(map, ~.x$data) %>% 
      transpose %>% 
      map(compress, names_to = 'method') %>% 
      map(mutate,
          .outcome = ifelse(.outcome == cluster, cluster, 'rest'), 
          .outcome = factor(.outcome, levs), 
          .fitted = ifelse(.fitted == cluster, cluster, 'rest'), 
          .fitted = factor(.fitted, levs))
    
    ## ROC plots
    
    if(numeric) plot_var <- cluster else plot_var <- '.fitted'
    
    roc_lst <- 
      list(x = data, 
           y = paste(title_prefix, 
                     globals$part_labels[names(data)], 
                     sep = ', '), 
           v = n_caps) %>% 
      pmap(function(x, y, v) x %>% 
             ggplot(aes(m = as.numeric(.data[[plot_var]]) - 1, 
                        d = as.numeric(.outcome) - 1, 
                        color = method)) + 
             geom_roc(...) + 
             scale_color_manual(values = palette, 
                                labels = labels, 
                                name = 'Algorithm') + 
             style_roc() + 
             geom_abline(slope = 1, 
                         intercept = 0, 
                         linetype = 'dashed')  +
             globals$common_theme + 
             labs(title = y, 
                  subtitle = v, 
                  x = '1 - Sp', 
                  y = 'Se'))
    
    for(i in 1:nrow(roc_stats[[1]])) {
      
      roc_lst <- 
        map2(roc_lst, 
             roc_stats, 
             ~.x + 
               annotate('text', 
                        label = .y[['plot_lab']][[i]], 
                        x = x_pos, 
                        y = .y[['y_pos']][[i]], 
                        color = .y[['color']][[i]], 
                        size = txt_size, 
                        hjust = 0, 
                        vjust = 0))
      
    }
    
    roc_lst
    
  }

# Labellers --------
  
  psych_labeller <- 
    c('pss4_total' = 'stress, PSS4', 
      'gad7_total' = 'anxiety, GAD-7', 
      'phq9_total' = 'depression, PHQ-9', 
      'phq_events_total' = 'somatization, PHQ-15', 
      'phqd_panic_total' = 'panic, PHQ-panic', 
      'soc9l_total' = 'lack of SOC, SOC-9L', 
      'rs13_total' = 'resilience, RS-13', 
      'eurohis_total' = 'EUROHIS-QOL8 mean', 
      'dsm5_total' = 'PCL-5 sum', 
      'ptgi_total' = 'PTGI sum')
  
  
# varia -----
  
  vec_sum <- function(vec_list, na.rm = TRUE) {
    
    transpose(as.list(vec_list)) %>% 
      map(reduce, c) %>% 
      map_dbl(sum, na.rm = na.rm)
    
  }
  
  format_summ_tbl <- function(data, 
                              rm_n = TRUE, 
                              rm_mean = TRUE, 
                              rm_median_txt = TRUE, 
                              dict = ptsd$var_lexicon, 
                              out_value = 'axis_lab') {
    
    ## formats a summary table with descriptive stats
    
    data <- data %>% 
      map_dfc(stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
      map_dfc(stri_replace, regex = '\\nno:.*$', replacement = '') %>% 
      map_dfc(stri_replace_all, fixed = '% (', replacement = '% (n = ') %>% 
      map_dfc(stri_replace, fixed = 'Median =', replacement = 'median:') %>% 
      map_dfc(stri_replace, fixed = 'Mean =', replacement = 'mean:') %>% 
      map_dfc(stri_replace, fixed = 'Range', replacement = 'range') %>% 
      map_dfc(stri_replace, fixed = 'Complete: ', replacement = '') %>% 
      map_dfc(stri_replace, 
              regex = 'negative.*\\npositive:\\s{1}', 
              replacement = '') %>% 
      mutate(variable = exchange(variable, 
                                 dict = dict, 
                                 value = out_value))
    
    if(rm_n) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = '\\nCompl.*$', replacement = '')
        
    }
    
    if(rm_mean) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = 'mean.*\\n', replacement = '')
      
    }
    
    if(rm_median_txt) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = '^median:\\s{1}', replacement = '')
      
    }
    
    data
    
  }
  
  re_adjust <- function(data, method = 'BH') {
    
    ## adjusts for multiple testing e.g. with the Benjamini-Hochberg method
    
    if(method != 'none') {
      
      data <- data %>% 
        mutate(p_adjusted = p.adjust(p_value, method = method))
      
    }
    
    data %>% 
      mutate(significance = ifelse(p_adjusted < 0.001, 
                                   'p < 0.001', 
                                   ifelse(p_adjusted >= 0.05, 
                                          paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                          paste('p =', signif(p_adjusted, 2)))))
    
  }
  
  stri_capitalize <- function(str) {
    
    ## capitalizes the first letter of a string
    
    first_letter <- stri_extract(str, regex = '^\\w{1}')
    
    stri_replace(str, regex = '^\\w{1}', replacement = toupper(first_letter))
    
  }

  my_word <- function(...) {
    
    form <- word_document2(number_sections = FALSE, 
                           reference_docx = 'ms_template.docx')
    
    form$pandoc$lua_filters <- c(form$pandoc$lua_filters, 
                                 'scholarly-metadata.lua', 
                                 'author-info-blocks.lua')
    
    form
    
  }
  
  get_num_stats <- function(data, 
                            variable, 
                            signif_digits = 2, 
                            sep = ' to ', 
                            collapse = NULL) {
    
    median <- median(data[[variable]], na.rm = TRUE) %>% 
      signif(signif_digits)
    
    iqr <- quantile(data[[variable]], c(0.25, 0.75), na.rm = TRUE) %>% 
      signif(signif_digits) %>% 
      paste(collapse = sep)
    
    if(is.null(collapse)) return(list(median = median, iqr = iqr) )
    
    paste(median, iqr, sep = collapse)

  }
  
  get_percent <- function(data, variable, signif_digits = 2) {
    
    count <- table(data[[variable]])
    
    complete <- sum(table(data[[variable]]))
    
    percent <- signif(count/complete * 100, signif_digits)
    
    list(count = count, 
         complete = complete, 
         percent = percent)
  }
  
  digit2string <- function(x) {
    
    x <- as.integer(x)
    
    if(x > 10) return(x)
    
    digits <- 
      c('one', 'two', 'three', 'four', 
        'five', 'six', 'seven', 'eight', 'nine', 'ten')
    
    digits[x]
    
  }

# END -----