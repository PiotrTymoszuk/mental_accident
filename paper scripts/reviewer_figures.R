# Figures to be attached to the rebuttal letter

  insert_head()
  
# container ------
  
  rev_figures <- list()
  
# Figure 1: effects of the observation time --------
  
  insert_msg('Effects of the observation time')
  
  rev_figures$time <- 
    time_psych$plots[c("dsm5_total", "ptgi_total", "gad7_total", 
                       "phq9_total", "rs13_total", "eurohis_total")] %>% 
    map(~.x +
          scale_x_discrete(labels = function(x) stri_replace(x, fixed = ' months', replacement = ''), 
                           name = 'admission - survey time, months') + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv') %>% 
    plot_grid(plot_grid(clust_bcg$plots$accident_year, 
                        ncol = 2),
              nrow = 2, 
              rel_heights = c(2, 1), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_rev_1_time_and_psychometry', 
              ref_name = 'time', 
              caption = paste('Effects of ward admission-survey time', 
                              'and of accident year on the major psychometric', 
                              'readouts and the mental cluster assignment.'), 
              w = 180, 
              h = 210)
  
# Figure 2: missingness of the mental battery variables ------
  
  insert_msg('Figure 2: missingness of mental battery variables')
  
  rev_figures$miss_mental <- miss_mental$plot %>% 
    as_figure(label = 'figure_rev_2_incomplete_mental_battery', 
              ref_name = 'miss_mental', 
              caption = paste('Percentages of missing observations for the', 
                              'mental health assessment battery variables.'), 
              w = 180, 
              h = 180)
  
# Saving the figures on the disc -----
  
  insert_msg('Saving figures on the disc')
  
  rev_figures %>% 
    walk(pickle.figure, 
         path = './paper/review figures', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()