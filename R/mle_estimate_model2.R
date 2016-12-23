insert_age_structure = FALSE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")
#dev.off() # clear all graphical devices and plots in the plotting area.
graphSettings() # seeting plotting parameters

#=============================================================
## Parameters estimates when only transmission rate is forced.
#=============================================================

if(!file.exists("data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates.RData")){ 
  
  # seasonal forcing of invasion rate alone.
  
  # DISTRICT OF SEGUENEGA ( Id = 1)
  
  is_a0Constant = TRUE;
  mle_non_epi_seguenNonEpiForc_beta0_2006 = mleYearSpecFit(district_id=1,
                                                           district_year_data = non_missing_data(seguen_2006), 
                                                           population_size = seguen_2006_population_size,
                                                           year_now = 2006, hc_vector= c(1:4,6),
                                                           a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                           addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                           # choose the appropriate method for algorithm
                                                           # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                           # use L-BFGS-B for the bbmle package with mle2
                                                           algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                           n_iter = 10000)
  
  
  
  
  
  mle_non_epi_seguenNonEpiForc_beta0_2007 = mleYearSpecFit(district_id=1,
                                                           district_year_data = non_missing_data(seguen_2007), 
                                                           population_size = seguen_2007_population_size,
                                                           year_now = 2007, hc_vector= c(1:3,5,6),
                                                           a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                           addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                           # choose the appropriate method for algorithm
                                                           # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                           # use L-BFGS-B for the bbmle package with mle2
                                                           algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                           n_iter = 10000)
  
  
  
  
  mle_non_epi_seguenNonEpiForc_beta0_2009 = mleYearSpecFit(district_id=1,
                                                           district_year_data = non_missing_data(seguen_2009), 
                                                           population_size = seguen_2009_population_size,
                                                           year_now = 2009, hc_vector= c(6),
                                                           a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                           addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                           # choose the appropriate method for algorithm
                                                           # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                           # use L-BFGS-B for the bbmle package with mle2
                                                           algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                           n_iter = 10000)
  
  
  
  
  mle_non_epi_seguenNonEpiForc_beta0_2010 = mleYearSpecFit(district_id=1,
                                                           district_year_data = non_missing_data(seguen_2010), 
                                                           population_size = seguen_2010_population_size,
                                                           year_now = 2010, hc_vector= c(1:6),
                                                           a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                           addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                           # choose the appropriate method for algorithm
                                                           # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                           # use L-BFGS-B for the bbmle package with mle2
                                                           algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                           n_iter = 10000)
  
  
  #===========================================================================================================
  
  
  # DISTRICT OF HOUNDE ( Id = 2)
  
  mle_non_epi_houndeForc_beta0_2004 = mleYearSpecFit(district_id=2,
                                                     district_year_data = non_missing_data(hounde_2004), 
                                                     population_size = hounde_2004_population_size,
                                                     year_now = 2004, hc_vector= c(4,7,13),
                                                     a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                     addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                     # choose the appropriate method for algorithm
                                                     # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                     # use L-BFGS-B for the bbmle package with mle2
                                                     algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                     n_iter = 10000)
  
  
  
  mle_non_epi_houndeForc_beta0_2006 = mleYearSpecFit(district_id=2,
                                                     district_year_data = non_missing_data(hounde_2006), 
                                                     population_size = hounde_2006_population_size,
                                                     year_now = 2006, hc_vector= c(1:14),
                                                     a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                     addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                     # choose the appropriate method for algorithm
                                                     # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                     # use L-BFGS-B for the bbmle package with mle2
                                                     algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                     n_iter = 10000)
  
  
  
  
  
  mle_non_epi_houndeForc_beta0_2007 = mleYearSpecFit(district_id=2,
                                                     district_year_data = non_missing_data(hounde_2007), 
                                                     population_size = hounde_2007_population_size,
                                                     year_now = 2007, hc_vector= c(2,3,15),
                                                     a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                     addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                     # choose the appropriate method for algorithm
                                                     # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                     # use L-BFGS-B for the bbmle package with mle2
                                                     algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                     n_iter = 10000)
  
  
  
  
  mle_non_epi_houndeForc_beta0_2008 = mleYearSpecFit(district_id=2,
                                                     district_year_data = non_missing_data(hounde_2008), 
                                                     population_size = hounde_2008_population_size,
                                                     year_now = 2008, hc_vector= c(1:4,6:11,15:18),
                                                     a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                     addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                     # choose the appropriate method for algorithm
                                                     # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                     # use L-BFGS-B for the bbmle package with mle2
                                                     algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                     n_iter = 10000)
  
  
  
  
  mle_non_epi_houndeForc_beta0_2009 =  mleYearSpecFit(district_id=2,
                                                      district_year_data = non_missing_data(hounde_2009), 
                                                      population_size = hounde_2009_population_size,
                                                      year_now = 2009, hc_vector= c(1,15:17),
                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                      # choose the appropriate method for algorithm
                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                      # use L-BFGS-B for the bbmle package with mle2
                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                      n_iter = 10000)
  
  
  #===========================================================================================================
  ## === DISTRICT OF K.VIGUE (id = 3) === ###
  mle_non_epi_lenaForc_beta0_2006 = mleYearSpecFit(district_id=3,
                                                   district_year_data = non_missing_data(lena_2006), 
                                                   population_size = lena_2006_population_size,
                                                   year_now = 2006, hc_vector= c(1:4),
                                                   a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                   addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                   # choose the appropriate method for algorithm
                                                   # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                   # use L-BFGS-B for the bbmle package with mle2
                                                   algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                   n_iter = 10000)
  
  
  
  
  
  mle_non_epi_lenaForc_beta0_2007 = mleYearSpecFit(district_id=3,
                                                   district_year_data = non_missing_data(lena_2007), 
                                                   population_size = lena_2007_population_size,
                                                   year_now = 2007, hc_vector= c(1,3),
                                                   a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                   addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                   # choose the appropriate method for algorithm
                                                   # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                   # use L-BFGS-B for the bbmle package with mle2
                                                   algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                   n_iter = 10000)
  
  
  
  #===========================================================================================================
  
  ## === DISTRICT OF K.VIGUE (id = 4) === ###
  
  mle_non_epi_kvigueForc_beta0_2008 = mleYearSpecFit(district_id=4,
                                                     district_year_data = non_missing_data(Kvigue_2008), 
                                                     population_size = kvigue_2008_population_size,
                                                     year_now = 2008, hc_vector= c(1,2),
                                                     a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                     addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                     # choose the appropriate method for algorithm
                                                     # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                     # use L-BFGS-B for the bbmle package with mle2
                                                     algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                     n_iter = 10000)
  
  mle_non_epi_kvigueForc_beta0_2010 = mleYearSpecFit(district_id=4,
                                                     district_year_data = non_missing_data(Kvigue_2010), 
                                                     population_size = kvigue_2010_population_size,
                                                     year_now = 2010, hc_vector= c(3,5),
                                                     a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                     addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                                     # choose the appropriate method for algorithm
                                                     # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                     # use L-BFGS-B for the bbmle package with mle2
                                                     algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                     n_iter = 10000)
  
  
  
  
  # save the results to a file
  save(mle_non_epi_seguenNonEpiForc_beta0_2006,
       mle_non_epi_seguenNonEpiForc_beta0_2007,
       mle_non_epi_seguenNonEpiForc_beta0_2009,
       mle_non_epi_seguenNonEpiForc_beta0_2010,
       mle_non_epi_houndeForc_beta0_2004,
       mle_non_epi_houndeForc_beta0_2006,
       mle_non_epi_houndeForc_beta0_2007,
       mle_non_epi_houndeForc_beta0_2008,
       mle_non_epi_houndeForc_beta0_2009,
       mle_non_epi_lenaForc_beta0_2006,
       mle_non_epi_lenaForc_beta0_2007,
       mle_non_epi_kvigueForc_beta0_2008,
       mle_non_epi_kvigueForc_beta0_2010,
       file="data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates.RData")
  
  
}else{
  cat("\n The file \"mle_beta0ForcEstimates.RData\" already exist \n in :",paste(getwd(),"data/processed_data/new_processed_february_2016/", sep="/"), 
      "\n Please delete it first in order to reestimate parameters")
} #end of if -else conditional statement for force_beta0 estimates. 

# Model with forcing of the transmission alone. 

mle_beta0ForcEstimates_matrice = rbind(
  mle_non_epi_seguenNonEpiForc_beta0_2006,
  mle_non_epi_seguenNonEpiForc_beta0_2007,
  mle_non_epi_seguenNonEpiForc_beta0_2009,
  mle_non_epi_seguenNonEpiForc_beta0_2010,
  mle_non_epi_houndeForc_beta0_2004,
  mle_non_epi_houndeForc_beta0_2006,
  mle_non_epi_houndeForc_beta0_2007,
  mle_non_epi_houndeForc_beta0_2008,
  mle_non_epi_houndeForc_beta0_2009,
  mle_non_epi_lenaForc_beta0_2006,
  mle_non_epi_lenaForc_beta0_2007,
  mle_non_epi_kvigueForc_beta0_2008,
  mle_non_epi_kvigueForc_beta0_2010
  
)

save(mle_beta0ForcEstimates_matrice,
     file = "data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")

