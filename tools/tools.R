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
      
      freq_plot <- freq_plot + 
        geom_text(aes(label = signif(.data[[freq_var]], 2)), ...)
      
    }
    
    return(freq_plot)
    
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
           x_n_labs = TRUE) %>% 
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
           paste, sep = '\n')
    
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
           point_alpha = 0.25, 
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
                              labels = rev(z)))
    
  }
  
# Markdown and knitr -------
  
  my_word <- function(...) {
    
    form <- word_document2(number_sections = FALSE, 
                           reference_docx = 'ms_template.docx')
    
    form$pandoc$lua_filters <- c(form$pandoc$lua_filters, 
                                 'scholarly-metadata.lua', 
                                 'author-info-blocks.lua')
    
    form
    
  }
  
# varia -----
  
  vec_sum <- function(vec_list, na.rm = T) {
    
    transpose(as.list(vec_list)) %>% 
      map(reduce, c) %>% 
      map_dbl(sum, na.rm = na.rm)
    
  }
  
  complete_cases <- function(data, id_var = 'ID') {
    
    ### selects the individuals with the complete variable record
    
    dlply(data, id_var) %>% 
      map_dfr(function(x) if(any(!complete.cases(x))) NULL else x)
    
    
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
  
  mm_inch <- function(x) 0.0393700787 * x
  
  embolden_scale <- function(x, 
                             highlight,  
                             color = 'black', 
                             family = '', 
                             translate = FALSE, 
                             dict = globals$var_lexicon, ...) {
    
    if(!translate) {
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{x}</b>"), 
                    x))
      
    } else {
      
      labels <- translate_var(x, dict = dict, ...)
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{labels[x]}</b>"), 
                    labels[x]))
      
      
    }
    
  }
  
  stri_capitalize <- function(str) {
    
    ## capitalizes the first letter of a string
    
    first_letter <- stri_extract(str, regex = '^\\w{1}')
    
    stri_replace(str, regex = '^\\w{1}', replacement = toupper(first_letter))
    
  }

# END -----