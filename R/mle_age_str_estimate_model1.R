insert_age_structure = TRUE; heterogenous_mixing=TRUE
source("R/run_first.R")
source("R/models_parameters.R")
#dev.off() # clear all graphical devices and plots in the plotting area.
graphSettings() # seeting plotting parameters


# check to see if the parameter estimates matrice exists from a previous run of this script.

start.time <- Sys.time()
if(!file.exists("data/processed_data/new_processed_february_2016/mle_age_str_age_str_a0ForcEstimates.RData")){ 
  
  # seasonal forcing of invasion rate alone.
  
  # DISTRICT OF SEGUENEGA ( Id = 1)
  
  nb_iteration = 40000 #! IMPORTANT TO SET THE NUMBER OF ITERATION ABOVE 30000 FOR OPTIMAL RESULTS
  is_a0Constant = FALSE 
  mle_age_str_non_epi_seguenNonEpiForc_a0_2006 = mleYearSpecFit_age_str(district_id=1,
                                                        district_year_data = non_missing_data(seguen_2006), 
                                                        population_size = seguen_2006_population_size,
                                                        year_now = 2006, hc_vector= c(1:4,6),
                                                        a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                        addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                        # choose the appropriate method for algorithm
                                                        # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                        # use L-BFGS-B for the bbmle package with mle2
                                                        algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                        n_iter = nb_iteration)
  
  
  
  
  
  mle_age_str_non_epi_seguenNonEpiForc_a0_2007 = mleYearSpecFit_age_str(district_id=1,
                                                        district_year_data = non_missing_data(seguen_2007), 
                                                        population_size = seguen_2007_population_size,
                                                        year_now = 2007, hc_vector= c(1:3,5,6),
                                                        a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                        addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                        # choose the appropriate method for algorithm
                                                        # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                        # use L-BFGS-B for the bbmle package with mle2
                                                        algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                        n_iter = nb_iteration)
  
  
  
  
  mle_age_str_non_epi_seguenNonEpiForc_a0_2009 = mleYearSpecFit_age_str(district_id=1,
                                                        district_year_data = non_missing_data(seguen_2009), 
                                                        population_size = seguen_2009_population_size,
                                                        year_now = 2009, hc_vector= c(6),
                                                        a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                        addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                        # choose the appropriate method for algorithm
                                                        # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                        # use L-BFGS-B for the bbmle package with mle2
                                                        algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                        n_iter = nb_iteration)
  
  
  
  
  mle_age_str_non_epi_seguenNonEpiForc_a0_2010 = mleYearSpecFit_age_str(district_id=1,
                                                        district_year_data = non_missing_data(seguen_2010), 
                                                        population_size = seguen_2010_population_size,
                                                        year_now = 2010, hc_vector= c(1:6),
                                                        a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                        addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                        # choose the appropriate method for algorithm
                                                        # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                        # use L-BFGS-B for the bbmle package with mle2
                                                        algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                        n_iter = nb_iteration)
  
  
  #===========================================================================================================
  
  
  # DISTRICT OF HOUNDE ( Id = 2)
  
  mle_age_str_non_epi_houndeForc_a0_2004 = mleYearSpecFit_age_str(district_id=2,
                                                  district_year_data = non_missing_data(hounde_2004), 
                                                  population_size = hounde_2004_population_size,
                                                  year_now = 2004, hc_vector= c(4,7,13),
                                                  a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                  addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                  # choose the appropriate method for algorithm
                                                  # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                  # use L-BFGS-B for the bbmle package with mle2
                                                  algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                  n_iter = nb_iteration)
  
  
  
  mle_age_str_non_epi_houndeForc_a0_2006 = mleYearSpecFit_age_str(district_id=2,
                                                  district_year_data = non_missing_data(hounde_2006), 
                                                  population_size = hounde_2006_population_size,
                                                  year_now = 2006, hc_vector= c(1:14),
                                                  a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                  addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                  # choose the appropriate method for algorithm
                                                  # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                  # use L-BFGS-B for the bbmle package with mle2
                                                  algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                  n_iter = nb_iteration)
  
  
  
  
  
  mle_age_str_non_epi_houndeForc_a0_2007 = mleYearSpecFit_age_str(district_id=2,
                                                  district_year_data = non_missing_data(hounde_2007), 
                                                  population_size = hounde_2007_population_size,
                                                  year_now = 2007, hc_vector= c(2,3,15),
                                                  a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                  addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                  # choose the appropriate method for algorithm
                                                  # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                  # use L-BFGS-B for the bbmle package with mle2
                                                  algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                  n_iter = nb_iteration)
  
  
  
  
  mle_age_str_non_epi_houndeForc_a0_2008 = mleYearSpecFit_age_str(district_id=2,
                                                  district_year_data = non_missing_data(hounde_2008), 
                                                  population_size = hounde_2008_population_size,
                                                  year_now = 2008, hc_vector= c(1:4,6:11,15:18),
                                                  a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                  addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                  # choose the appropriate method for algorithm
                                                  # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                  # use L-BFGS-B for the bbmle package with mle2
                                                  algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                  n_iter = nb_iteration)
  
  
  
  
  mle_age_str_non_epi_houndeForc_a0_2009 =  mleYearSpecFit_age_str(district_id=2,
                                                   district_year_data = non_missing_data(hounde_2009), 
                                                   population_size = hounde_2009_population_size,
                                                   year_now = 2009, hc_vector= c(1,15:17),
                                                   a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                   # choose the appropriate method for algorithm
                                                   # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                   # use L-BFGS-B for the bbmle package with mle2
                                                   algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                   n_iter = nb_iteration)
  
  
  #EstimNonEpiHoundeForc_a0_2010 = yearSpecFit(district_id=2, 
  #district_year_data = hounde_2010, 
  #year_now = 2010, hc_vector = c(5),
  #a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
  #addCarriageConstrain = TRUE, show_plot = TRUE, show_plot = TRUE, 
  #nloptrAlgorithm = "NLOPT_LN_COBYLA",
  #n_iter = nb_iteration)
  #===========================================================================================================
  ## === DISTRICT OF K.VIGUE (id = 3) === ###
  mle_age_str_non_epi_lenaForc_a0_2006 = mleYearSpecFit_age_str(district_id=3,
                                                district_year_data = non_missing_data(lena_2006), 
                                                population_size = lena_2006_population_size,
                                                year_now = 2006, hc_vector= c(1:4),
                                                a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                # choose the appropriate method for algorithm
                                                # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                # use L-BFGS-B for the bbmle package with mle2
                                                algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                n_iter = nb_iteration)
  
  
  
  
  
  mle_age_str_non_epi_lenaForc_a0_2007 = mleYearSpecFit_age_str(district_id=3,
                                                district_year_data = non_missing_data(lena_2007), 
                                                population_size = lena_2007_population_size,
                                                year_now = 2007, hc_vector= c(1,3),
                                                a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                # choose the appropriate method for algorithm
                                                # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                # use L-BFGS-B for the bbmle package with mle2
                                                algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                n_iter = nb_iteration)
  
  
  
  #===========================================================================================================
  
  ## === DISTRICT OF K.VIGUE (id = 4) === ###
  
  mle_age_str_non_epi_kvigueForc_a0_2008 = mleYearSpecFit_age_str(district_id=4,
                                                  district_year_data = non_missing_data(Kvigue_2008), 
                                                  population_size = kvigue_2008_population_size,
                                                  year_now = 2008, hc_vector= c(1,2),
                                                  a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                  addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                  # choose the appropriate method for algorithm
                                                  # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                  # use L-BFGS-B for the bbmle package with mle2
                                                  algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                  n_iter = nb_iteration)
  
  mle_age_str_non_epi_kvigueForc_a0_2010 = mleYearSpecFit_age_str(district_id=4,
                                                  district_year_data = non_missing_data(Kvigue_2010), 
                                                  population_size = kvigue_2010_population_size,
                                                  year_now = 2010, hc_vector= c(3,5),
                                                  a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                  addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                  # choose the appropriate method for algorithm
                                                  # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                  # use L-BFGS-B for the bbmle package with mle2
                                                  algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                  n_iter = nb_iteration)
  
  
  
  
  # save the results to a file
  save(mle_age_str_non_epi_seguenNonEpiForc_a0_2006,
       mle_age_str_non_epi_seguenNonEpiForc_a0_2007,
       mle_age_str_non_epi_seguenNonEpiForc_a0_2009,
       mle_age_str_non_epi_seguenNonEpiForc_a0_2010,
       mle_age_str_non_epi_houndeForc_a0_2004,
       mle_age_str_non_epi_houndeForc_a0_2006,
       mle_age_str_non_epi_houndeForc_a0_2007,
       mle_age_str_non_epi_houndeForc_a0_2008,
       mle_age_str_non_epi_houndeForc_a0_2009,
       mle_age_str_non_epi_lenaForc_a0_2006,
       mle_age_str_non_epi_lenaForc_a0_2007,
       mle_age_str_non_epi_kvigueForc_a0_2008,
       mle_age_str_non_epi_kvigueForc_a0_2010,
       file="data/processed_data/new_processed_february_2016/mle_age_str_a0ForcEstimates.RData")
  
  # Model with forcing of the invasion alone.
  
  mle_age_str_a0ForcEstimates_matrice = rbind(
    mle_age_str_non_epi_seguenNonEpiForc_a0_2006,
    mle_age_str_non_epi_seguenNonEpiForc_a0_2007,
    mle_age_str_non_epi_seguenNonEpiForc_a0_2009,
    mle_age_str_non_epi_seguenNonEpiForc_a0_2010,
    mle_age_str_non_epi_houndeForc_a0_2004,
    mle_age_str_non_epi_houndeForc_a0_2006,
    mle_age_str_non_epi_houndeForc_a0_2007,
    mle_age_str_non_epi_houndeForc_a0_2008,
    mle_age_str_non_epi_houndeForc_a0_2009,
    mle_age_str_non_epi_lenaForc_a0_2006,
    mle_age_str_non_epi_lenaForc_a0_2007,
    mle_age_str_non_epi_kvigueForc_a0_2008,
    mle_age_str_non_epi_kvigueForc_a0_2010)
  
  save(mle_age_str_a0ForcEstimates_matrice,
       file = "data/processed_data/new_processed_february_2016/mle_age_str_a0ForcEstimates_matrice.RData")
  
}else{
  stop("\n COSTUM MESSAGE: The file \"mle_age_str_a0ForcEstimates.RData\" already exist in :",paste(getwd(),"data/processed_data/new_processed_february_2016/", sep="/"), 
       "\n PLEASE DELETE OR RENAME IT FIRST BEFORE RE_ESTIMATING PARAMETERS.")
} #end of if -else conditional statement for force_a0 estimates. 

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

