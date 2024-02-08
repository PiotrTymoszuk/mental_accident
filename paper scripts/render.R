# Renders the manuscript and the supplementary material

  insert_head()
  
# reading the bibliography -------
  
  insert_msg('Reading the bibliography')
  
  alpine_bib <- read_bib('./paper/markdown/ptsd_biblio.bib')
  
# supplementary material ------
  
  insert_msg('Rendering the supplements')
  
  render('./paper/markdown/supplementary_material.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
         output_dir = './paper')

# paper -----
  
  insert_msg('Rendering the paper')

  render('./paper/markdown/manuscript.Rmd', 
         output_format = my_word(), 
         output_dir = './paper')
  
# point-to-point reply -------
  
  insert_msg('Rendering the rebittal letter')
  
  render('./paper/markdown/review_response.Rmd', 
         output_format = my_word(), 
         output_dir = './paper')
  
# END -----
  
  insert_tail()