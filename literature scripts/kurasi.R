# Comparison of: 
# 
# 1) distribution of sport disciplines in the cohort with with the 2023 stats 
# for the whole country
#
# 2) comparison of the age structure
#
# Some issues with the KURASI data set: 
# https://alpinesicherheit.at/wp-content/uploads/2024/01/230123_OeKAS_PA-Jahresrueckblick-2023_FINAL.pdf)
# The data for sport types and monthly counts include kids and teens, as well as 
# people from non-German-speaking countries (although they constitute a tiny 
# minority) and deaths (which are again far less than 1% of all accidents).

  insert_head()
  
# container -------
  
  lit_kurasi <- list()
  
# Variable lexicon -----
  
  insert_msg('Variable lexicon')
  
  lit_kurasi$var_lexicon <- ptsd$var_lexicon %>% 
    select(variable, label, axis_lab) %>% 
    rbind(tibble(variable = c('accident_month', 'sport_detail'), 
                 label = c('accident month', 'sport type, officiel classification'), 
                 axis_lab = c('accident month', 'sport type')))
  
# analysis data: sport type ------
  
  insert_msg('Analysis data: sport type')
  
  ## own estimates
  
  lit_kurasi$sport_type$own_data <- ptsd$dataset$sport_type
  
  ## the KURASI counts: excluding the category 'sonstige' with e.g. 
  ## hunting and communication accidents
  
  lit_kurasi$sport_type$kurasi_n_total <- 13681 - 648
  
  lit_kurasi$sport_type$kurasi_data[['ski/snowboard/cross-country']] <- 
    6177 + 238 + 444 + 44
  
  lit_kurasi$sport_type$kurasi_data[['sledding']] <- 327
  
  lit_kurasi$sport_type$kurasi_data[['climbing/hiking/mountaineering/skitour']] <- 
    2987 + 544 + 90 + 682 + 21
  
  lit_kurasi$sport_type$kurasi_data[['biking']] <- 1124
  
  lit_kurasi$sport_type$kurasi_data[['other']] <- 295 + 45 + 11 + 4
  
  ## aggregating the kurasi_data and merging with own estimates
  
  lit_kurasi$sport_type$kurasi_data <- 
    map2(names(lit_kurasi$sport$kurasi_data), 
         lit_kurasi$sport$kurasi_data, rep) %>% 
    reduce(c)
  
  lit_kurasi$sport_type$data <- 
    map2_dfr(c('cohort', 'Austria'), 
             lit_kurasi$sport_type[c("own_data", "kurasi_data")], 
             ~tibble(subset = .x, 
                     sport_type = factor(.y, levels(ptsd$dataset$sport_type))))
  
# analysis data: monthly distribution -------
  
  insert_msg('Data monthly distribution')
  
  ## own data
  
  lit_kurasi$accident_month$own_data <- 
    ptsd$dataset$accident_month
  
  ## KURASI data
  
  lit_kurasi$accident_month$kurasi_data <- 
    c(rep(1, 2107), 
      rep(2, 3055), 
      rep(3, 1654), 
      rep(4, 510), 
      rep(5, 390), 
      rep(6, 784), 
      rep(7, 1301), 
      rep(8, 1187), 
      rep(9, 943), 
      rep(10, 486), 
      rep(11, 192), 
      rep(12, 1079))
  
  ## the aggregated data
  
  lit_kurasi$accident_month$data <- 
    map2_dfr(c('cohort', 'Austria'), 
             lit_kurasi$accident_month[c("own_data", "kurasi_data")], 
             ~tibble(subset = .x, 
                     accident_month = .y)) %>% 
    mutate(accident_month = car::recode(accident_month, 
                                        "1 = 'Jan'; 2 = 'Feb'; 
                                        3 = 'Mar'; 4 = 'Apr'; 
                                        5 = 'May'; 6 = 'June'; 
                                        7 = 'July'; 8 = 'Aug'; 
                                        9 = 'Sept'; 10 = 'Oct'; 
                                        11 = 'Nov'; 12 = 'Dec'"), 
           accident_month = factor(accident_month, 
                                   c('Jan', 'Feb', 'Mar', 'Apr', 'May', 
                                     'June', 'July', 'Aug', 'Sept', 
                                     'Oct', 'Nov', 'Dec'))) %>% 
    mutate(accident_month = factor(accident_month, 
                                   c('Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 
                                     'May', 'June', 'July', 'Aug', 'Sept', 'Oct')), 
           season = ifelse(accident_month %in% c('Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr'), 
                           'winter', 'summer'), 
           season = factor(season, c('winter', 'summer')))
  
# Analysis data: age structure --------
  
  insert_msg('Age structure')
  
  ## only individuals > 20 years, for consistency with the KURASI report
  
  lit_kurasi$age_class$own_data <- ptsd$dataset %>% 
    filter(age > 20) %>% 
    mutate(age_class = cut(age, 
                           c(-Inf, 30, 40, 50, 60, 70, 80, 90, Inf), 
                           c('21 - 30', 
                             '31 - 40', 
                             '41 - 50', 
                             '51 - 60', 
                             '61 - 70', 
                             '71 - 80', 
                             '81 - 90', 
                             '91+'))) %>% 
    .$age_class
  
  lit_kurasi$age_class$kurasi_data <- 
    c(rep('21 - 30', 2033), 
      rep('31 - 40', 1691), 
      rep('41 - 50', 1890), 
      rep('51 - 60', 2386), 
      rep('61 - 70', 1559), 
      rep('71 - 80', 640), 
      rep('81 - 90', 223), 
      rep('91+', 10))
  
  lit_kurasi$age_class$data <- 
    map2_dfr(c('cohort', 'Austria'), 
             lit_kurasi$age_class[c("own_data", "kurasi_data")], 
             ~tibble(subset = .x, 
                     age_class = .y)) %>% 
    mutate(age_class = factor(age_class,
                              c('21 - 30', 
                                '31 - 40', 
                                '41 - 50', 
                                '51 - 60', 
                                '61 - 70', 
                                '71 - 80', 
                                '81 - 90', 
                                '91+')))
  
# Detailed sport type -------
  
  insert_msg('Detailed sport type')
  
  ## we're following the classification by KURASI
  
  lit_kurasi$sport_detail$kurasi_data[['hiking']] <- 2987
  
  lit_kurasi$sport_detail$kurasi_data[['biking']] <- 1124
  
  lit_kurasi$sport_detail$kurasi_data[['climbing']] <- 544 + 11 + 4
  
  lit_kurasi$sport_detail$kurasi_data[['air']] <- 295
  
  lit_kurasi$sport_detail$kurasi_data[['mountaineering']] <- 90
  
  lit_kurasi$sport_detail$kurasi_data[['water']] <- 45
  
  lit_kurasi$sport_detail$kurasi_data[['alpine skiing']] <- 6177 + 238
  
  lit_kurasi$sport_detail$kurasi_data[['ski touring']] <- 682 + 444
  
  lit_kurasi$sport_detail$kurasi_data[['sledding']] <- 327
  
  lit_kurasi$sport_detail$kurasi_data[['crosssountry']] <- 44
  
  lit_kurasi$sport_detail$kurasi_data[['ice climbing']] <- 21
  
  ## own data
  
  lit_kurasi$sport_detail$own_counts <- table(ptsd$dataset$sport_detail)
  
  lit_kurasi$sport_detail$own_data[['hiking']] <- lit_kurasi$sport_detail$own_counts["hiking"]
  
  lit_kurasi$sport_detail$own_data[['biking']] <- 
    sum(lit_kurasi$sport_detail$own_counts[c("biking", "MTB")])
  
  lit_kurasi$sport_detail$own_data[['climbing']] <- 
    sum(lit_kurasi$sport_detail$own_counts[c("rock climbing", "sport climbing/bouldering")])
  
  lit_kurasi$sport_detail$own_data[['air']] <- 
    lit_kurasi$sport_detail$own_counts["paragliding"]
  
  lit_kurasi$sport_detail$own_data[['mountaineering']] <- 
    lit_kurasi$sport_detail$own_counts["mountaineering"]
  
  lit_kurasi$sport_detail$own_data[['water']] <- 
    sum(lit_kurasi$sport_detail$own_counts[c("other water", "swimming", "surfing", "sailing")])
  
  lit_kurasi$sport_detail$own_data[['alpine skiing']] <- 
    sum(lit_kurasi$sport_detail$own_counts[c("alpine skiing", "snowboarding")])
  
  lit_kurasi$sport_detail$own_data[['ski touring']] <- 
    lit_kurasi$sport_detail$own_counts["skitouring"]
  
  lit_kurasi$sport_detail$own_data[['sledding']] <- 
    lit_kurasi$sport_detail$own_counts["sledding"]
  
  lit_kurasi$sport_detail$own_data[['crosssountry']] <- 
    lit_kurasi$sport_detail$own_counts['crosssountry skiing']
  
  lit_kurasi$sport_detail$own_data[['ice climbing']] <- 
    lit_kurasi$sport_detail$own_counts["ice climbing"]
  
  lit_kurasi$sport_detail$own_counts <- NULL
  lit_kurasi$sport_detail <- compact(lit_kurasi$sport_detail)
  
  ## data frame format
  
  lit_kurasi$sport_detail <- lit_kurasi$sport_detail %>% 
    map(~map2(names(.x), .x, rep)) %>% 
    map(reduce, c)

  lit_kurasi$sport_detail$data <- 
    map2_dfr(c('cohort', 'Austria'), 
             lit_kurasi$sport_detail[c("own_data", "kurasi_data")], 
             ~tibble(subset = .x, 
                     sport_detail = factor(.y))) %>% 
    mutate(season = ifelse(sport_detail %in% c('hiking', 
                                               'biking', 
                                               'climbing', 
                                               'air', 
                                               'mountaineering', 
                                               'water'), 
                           'summer', 'winter'), 
           season = factor(season, c('winter', 'summer')))
  
# Removal of the redundant data sets and incomplete cases ------
  
  insert_msg('Removal of redundant data sets')
  
  lit_kurasi$data <- 
    lit_kurasi[c("sport_type", "accident_month", "age_class", "sport_detail")] %>% 
    map(~.x$data) %>% 
    map(mutate, subset = factor(subset, c('cohort', 'Austria'))) %>% 
    map(~filter(.x, complete.cases(.x)))
  
  lit_kurasi <- lit_kurasi[c('var_lexicon', 'data')]
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  lit_kurasi$stats <- 
    map2(lit_kurasi$data, 
         names(lit_kurasi$data), 
         ~explore(.x, 
                  variables = .y, 
                  split_factor = 'subset', 
                  what = 'table', 
                  pub_styled = TRUE)) %>% 
    map_dfr(reduce, left_join, by = 'variable') %>% 
    set_names(c('variable', levels(lit_kurasi$data[[1]]$subset)))
  
# Testing for global differences in frequency -------
  
  insert_msg('Testing for global differences in distribution')
  
  lit_kurasi$test <- 
    map2_dfr(lit_kurasi$data, 
             names(lit_kurasi$data), 
             ~compare_variables(.x, 
                                variables = .y, 
                                split_factor = 'subset', 
                                what = 'eff_size',
                                types = 'cramer_v', 
                                ci = FALSE, 
                                exact = FALSE, 
                                pub_styled = TRUE)) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Post-hoc tests --------
  
  insert_msg('Post-hoc tests')
  
  ## pairwise Chi-square tests with Holm correction
  
  lit_kurasi$post_hoc <- lit_kurasi$data %>% 
    map(pairwise_chisq_test)

# Standard stack plots ------
  
  insert_msg('Standard stack plots')
  
  lit_kurasi$plots <- 
    list(x = lit_kurasi$data, 
         y = lit_kurasi$test$variable, 
         z = exchange(lit_kurasi$test$variable, 
                      lit_kurasi$var_lexicon), 
         v = lit_kurasi$test$plot_cap) %>% 
    pmap(function(x, y, z, v) x %>% 
           plot_variable(x, 
                         variable = y, 
                         split_factor = 'subset', 
                         type = 'stack', 
                         scale = 'percent', 
                         plot_title = stri_capitalize(z), 
                         plot_subtitle = v, 
                         cust_theme = globals$common_theme, 
                         x_n_labs = TRUE)) %>% 
    map(~.x + theme(axis.title.x = element_blank()))
  
  # styling 
  
  lit_kurasi$plots$sport_type <- lit_kurasi$plots$sport_type + 
    scale_fill_brewer(palette = 'Reds', 
                      labels = function(x) stri_replace_all(x, fixed = "/", replacement = '\n'))
  
# Panels for age and month distribution ------
  
  insert_msg('Panels for the sport type, age and month distribution')
  
  ## plotting data, computing the percents and total numbers
  ## stratification in winter and summer
  
  lit_kurasi$panel_data <- lit_kurasi$data
  
  lit_kurasi$panel_data[c("sport_type", "age_class")] <- 
    lit_kurasi$panel_data[c("sport_type", "age_class")] %>% 
    map2(., names(.), 
         ~count(.x, subset, .data[[.y]], .drop = FALSE))
  
  lit_kurasi$panel_data[c("accident_month", "sport_detail")] <- 
    lit_kurasi$panel_data[c("accident_month", "sport_detail")] %>% 
    map2(., names(.), 
         ~count(.x, subset, season, .data[[.y]], .drop = TRUE))

  lit_kurasi$panel_data <- lit_kurasi$panel_data %>% 
    map(group_by, subset) %>% 
    map(mutate, 
        n_total = sum(n), 
        percent = n/n_total * 100) %>% 
    map(ungroup)

  ## total n numbers
  
  lit_kurasi$panel_n_totals <- lit_kurasi$panel_data %>% 
    map(filter, !duplicated(subset)) %>% 
    map(blast, subset) %>% 
    map(map_dbl, ~.x$n_total[1])
  
  lit_kurasi$panel_n_labs <- lit_kurasi$panel_n_totals %>% 
    map(~map2_chr(names(.x), .x, paste, sep = '\nn = ')) %>% 
    map(set_names, names(lit_kurasi$panel_n_totals[[1]]))
  
  ## panels
  
  lit_kurasi$panels <- 
    list(x = lit_kurasi$panel_data, 
         y = names(lit_kurasi$panel_data), 
         z = exchange(names(lit_kurasi$panel_data), 
                      lit_kurasi$var_lexicon), 
         v = lit_kurasi$test$plot_cap, 
         w = lit_kurasi$panel_n_labs, 
         u = c('Mountain sport', 
               'Accident month', 
               'Age class, years', 
               'Mountain sport')) %>% 
    pmap(function(x, y, z, v, w, u) x %>% 
           ggplot(aes(x = .data[[y]], 
                      y = percent, 
                      fill = subset)) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    position = position_dodge(0.9)) + 
           geom_text(aes(label = signif(percent, 2),
                         color = subset), 
                     hjust = 0.5, 
                     vjust = -0.6, 
                     size = 2.75, 
                     position = position_dodge(0.9)) + 
           scale_fill_manual(values = c(cohort = 'indianred3', 
                                        Austria = 'steelblue'), 
                             labels = w, 
                             name = '') + 
           scale_color_manual(values = c(cohort = 'indianred3', 
                                         Austria = 'steelblue'), 
                              labels = w, 
                              name = '') + 
           globals$common_theme + 
           labs(title = stri_capitalize(z), 
                subtitle = v, 
                x = u, 
                y = '% of complete observations'))
  
  lit_kurasi$panels[c("accident_month", "sport_detail")] <- 
    lit_kurasi$panels[c("accident_month", "sport_detail")] %>% 
    map(~.x + 
          facet_grid(. ~ season, 
                     scales = 'free', 
                     space = 'free'))

  lit_kurasi$panels$sport_type <- lit_kurasi$panels$sport_type + 
    facet_grid(. ~ sport_type, 
               scales = 'free')
  
  lit_kurasi$panels$age_class <- lit_kurasi$panels$age_class + 
    facet_grid(. ~ age_class, 
               scales = 'free')

  lit_kurasi$panels[c("sport_type", "age_class")] <- 
    lit_kurasi$panels[c("sport_type", "age_class")] %>% 
    map(~.x + 
          theme(strip.background = element_blank(), 
                strip.text = element_blank()))
  
# Labeling significant results of post-hoc tests --------
  
  insert_msg('Post-hoc test significance')
  
  ## axis labels 
  
  lit_kurasi$panel_x_labels <- 
    map2(lit_kurasi$post_hoc, 
         names(lit_kurasi$post_hoc), 
         ~mutate(.x, 
                 ax_label = stri_replace_all(.data[[.y]], 
                                             fixed = '/', 
                                             replacement = '<br>'), 
                 ax_label = ifelse(p_adjusted < 0.05, 
                                   paste0('<b>', ax_label, '</b>'), 
                                   ax_label))) %>% 
    map2(., names(.), 
         ~set_names(.x$ax_label, .x[[.y]]))
  
  ## styling
  
  lit_kurasi$panels <- 
    list(x = lit_kurasi$panels, 
         y = lit_kurasi$panel_x_labels) %>% 
    pmap(function(x, y) x + 
           scale_x_discrete(labels = y) + 
           theme(axis.text.x = element_markdown()))

# END ------
  
  insert_tail()