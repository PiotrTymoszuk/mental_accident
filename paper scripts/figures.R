# Report figures 

  insert_head()
  
# container list ------
  
  figures <- list()
  
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

  figures %>% 
   walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail
  
  