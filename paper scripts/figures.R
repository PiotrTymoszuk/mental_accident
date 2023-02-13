# Report figures 

  insert_head()
  
# container list ------
  
  figures <- list()
  
# globals ------
  
  insert_msg('Globals setup')

 # report_fig$radial_panel <- 
 #   plot_grid(part_clust$radial_plot + 
  #              theme(plot.tag = element_blank(), 
  #                    legend.position = 'none'), 
   #           plot_grid(get_legend(part_clust$radial_plot), 
    #                    ggdraw() + 
     #                     draw_text(part_clust$n_tag %>% 
     #                                 stri_replace_all(fixed = ', ', 
      #                                                 replacement = '\n'), 
      #                              size = 8, 
      #                              hjust = 0, 
      #                              x = 0.2), 
     #                   nrow = 2), 
     #         ncol = 2, 
     #         rel_widths = c(0.7, 0.3))
    
# Figure 1: flow diagram --------
  
  insert_msg('Figure 1: flow diagram')
  
  figures$consort <- 
    plot_grid(ggdraw() + 
                draw_image('./aux files/consort.png')) %>% 
    as_figure(label = 'figure_1_flow_diagram', 
              ref_name = 'consort', 
              caption = paste('Flow diagram of the analysis', 
                              'inclusion process.'), 
              w = 90, 
              h = 2166/2106 * 90)
  
# Saving the figures on the disc -------
  
  insert_msg('Saving the figures on the disc')
  
  figures$radial_panel <- NULL
  
  figures <- compact(figures)
  
  figures %>% 
   walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail
  
  