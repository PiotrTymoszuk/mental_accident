# Clustering with the BNP (Bayesian non-parametric) EM model
# likely a bug in teh code (package is not at CRAN anymore)

  insert_head()
  
# tools -----
  
  library(BNPMIXcluster)
  
# container -----
  
  clust_bnp <- list()
  
# analysis variables and data -----
  
  insert_msg('Analysis globals')
  
  clust_bnp$variables <- clust_globals$variables
  
  clust_bnp$var_types <- 
    c(dsm5_total = 'c', 
      dsm5_B = 'c', 
      dsm5_C = 'c', 
      dsm5_D = 'c', 
      dsm5_E = 'c', 
      ptgi_total = 'c', 
      ptgi_fctI = 'c', 
      ptgi_fctII = 'c', 
      ptgi_fctIII = 'c', 
      ptgi_fctIV = 'c', 
      ptgi_fctV = 'c', 
      rs13_total = 'c', 
      soc9l_total = 'c', 
      phq9_total = 'c', 
      gad7_total = 'c', 
      phqd_panic_total = 'c', 
      phq_events_total = 'c', 
      eurohis_total = 'c', 
      eurohis_qol = 'o', 
      eurohis_health = 'o', 
      eurohis_energy = 'o', 
      eurohis_finances = 'o', 
      eurohis_activity = 'o', 
      eurohis_selfesteem = 'o', 
      eurohis_relationship = 'o', 
      eurohis_housing = 'o')
  
  clust_bnp$analysis_tbl <- clust_globals$analysis_tbl$training
  
# Modeling -------
  
  insert_msg('Modeling')
  
  clust_bnp$models <- MIXclustering(Y = as.matrix(clust_bnp$analysis_tbl), 
                                    var_type = clust_bnp$var_types)
  
# END ------