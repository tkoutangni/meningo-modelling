# model resimulation for health center were objective function return NAN
insert_age_structure = FALSE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")
#dev.off() # clear all graphical devices and plots in the plotting area.
graphSettings() # seeting plotting parameters

# MODEL WITH NO AGE STRUCTURE
# Model 1 # purposfully resimulate some hc with model 1 to check if code modification has an impact on previous prediction
start.time <- Sys.time()
nb_iteration = 1
is_a0Constant = FALSE
mle_non_epi_seguenNonEpiForc_a0_2007 = mleYearSpecFit(district_id=1,
                                                      district_year_data = non_missing_data(seguen_2007), 
                                                      population_size = seguen_2007_population_size,
                                                      year_now = 2007, hc_vector= c(1,3,6),
                                                      a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                      # choose the appropriate method for algorithm
                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                      # use L-BFGS-B for the bbmle package with mle2
                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                      n_iter = nb_iteration)


# mle_non_epi_seguenNonEpiForc_a0_2010 = mleYearSpecFit(district_id=1,
#                                                       district_year_data = non_missing_data(seguen_2010), 
#                                                       population_size = seguen_2010_population_size,
#                                                       year_now = 2010, hc_vector= c(2,5),
#                                                       a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
#                                                       addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
#                                                       # choose the appropriate method for algorithm
#                                                       # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
#                                                       # use L-BFGS-B for the bbmle package with mle2
#                                                       algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
#                                                       n_iter = nb_iteration)
# 
# 
# mle_non_epi_houndeForc_a0_2006 = mleYearSpecFit(district_id=2,
#                                                 district_year_data = non_missing_data(hounde_2006), 
#                                                 population_size = hounde_2006_population_size,
#                                                 year_now = 2006, hc_vector= c(10),
#                                                 a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
#                                                 addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
#                                                 # choose the appropriate method for algorithm
#                                                 # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
#                                                 # use L-BFGS-B for the bbmle package with mle2
#                                                 algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
#                                                 n_iter = nb_iteration)
# 
# 
# mle_non_epi_houndeForc_a0_2008 = mleYearSpecFit(district_id=2,
#                                                 district_year_data = non_missing_data(hounde_2008), 
#                                                 population_size = hounde_2008_population_size,
#                                                 year_now = 2008, hc_vector= c(2),
#                                                 a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
#                                                 addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
#                                                 # choose the appropriate method for algorithm
#                                                 # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
#                                                 # use L-BFGS-B for the bbmle package with mle2
#                                                 algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
#                                                 n_iter = nb_iteration)
# 
# 
# mle_non_epi_kvigueForc_a0_2010 = mleYearSpecFit(district_id=4,
#                                                 district_year_data = non_missing_data(Kvigue_2010), 
#                                                 population_size = kvigue_2010_population_size,
#                                                 year_now = 2010, hc_vector= c(3),
#                                                 a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
#                                                 addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
#                                                 # choose the appropriate method for algorithm
#                                                 # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
#                                                 # use L-BFGS-B for the bbmle package with mle2
#                                                 algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
#                                                 n_iter = nb_iteration)

#####################################################################################
#####################################################################################
# MODEL 2
is_a0Constant = FALSE
mle_non_epi_seguenNonEpiForc_beta0_2007 = mleYearSpecFit(district_id=1,
                                                      district_year_data = non_missing_data(seguen_2007), 
                                                      population_size = seguen_2007_population_size,
                                                      year_now = 2007, hc_vector= c(1,3,6),
                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                      # choose the appropriate method for algorithm
                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                      # use L-BFGS-B for the bbmle package with mle2
                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                      n_iter = nb_iteration)


mle_non_epi_seguenNonEpiForc_beta0_2010 = mleYearSpecFit(district_id=1,
                                                      district_year_data = non_missing_data(seguen_2010), 
                                                      population_size = seguen_2010_population_size,
                                                      year_now = 2010, hc_vector= c(2,5),
                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                      # choose the appropriate method for algorithm
                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                      # use L-BFGS-B for the bbmle package with mle2
                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                      n_iter = nb_iteration)


mle_non_epi_houndeForc_beta0_2006 = mleYearSpecFit(district_id=2,
                                                district_year_data = non_missing_data(hounde_2006), 
                                                population_size = hounde_2006_population_size,
                                                year_now = 2006, hc_vector= c(10),
                                                a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                # choose the appropriate method for algorithm
                                                # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                # use L-BFGS-B for the bbmle package with mle2
                                                algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                n_iter = nb_iteration)


mle_non_epi_houndeForc_beta0_2008 = mleYearSpecFit(district_id=2,
                                                district_year_data = non_missing_data(hounde_2008), 
                                                population_size = hounde_2008_population_size,
                                                year_now = 2008, hc_vector= c(2),
                                                a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                # choose the appropriate method for algorithm
                                                # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                # use L-BFGS-B for the bbmle package with mle2
                                                algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                n_iter = nb_iteration)


mle_non_epi_kvigueForc_beta0_2010 = mleYearSpecFit(district_id=4,
                                                district_year_data = non_missing_data(Kvigue_2010), 
                                                population_size = kvigue_2010_population_size,
                                                year_now = 2010, hc_vector= c(3),
                                                a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                # choose the appropriate method for algorithm
                                                # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                # use L-BFGS-B for the bbmle package with mle2
                                                algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                n_iter = nb_iteration)

#####################################################################################
#####################################################################################
# MODEL 3
is_a0Constant = FALSE
mle_non_epi_houndeForc_a0_beta0_2006 = mleYearSpecFit(district_id=2,
                                                      district_year_data = non_missing_data(hounde_2006), 
                                                      population_size = hounde_2006_population_size,
                                                      year_now = 2006, hc_vector= c(14),
                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                      # choose the appropriate method for algorithm
                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                      # use L-BFGS-B for the bbmle package with mle2
                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                      n_iter = nb_iteration)


mle_non_epi_houndeForc_a0_beta0_2008 = mleYearSpecFit(district_id=2,
                                                      district_year_data = non_missing_data(hounde_2008), 
                                                      population_size = hounde_2008_population_size,
                                                      year_now = 2008, hc_vector= c(2),
                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                      # choose the appropriate method for algorithm
                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                      # use L-BFGS-B for the bbmle package with mle2
                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                      n_iter = nb_iteration)


mle_non_epi_houndeForc_a0_beta0_2009 =  mleYearSpecFit(district_id=2,
                                                       district_year_data = non_missing_data(hounde_2009), 
                                                       population_size = hounde_2009_population_size,
                                                       year_now = 2009, hc_vector= c(15,17),
                                                       a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                       addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                       # choose the appropriate method for algorithm
                                                       # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                       # use L-BFGS-B for the bbmle package with mle2
                                                       algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                       n_iter = nb_iteration)

mle_non_epi_kvigueForc_a0_beta0_2010 = mleYearSpecFit(district_id=4,
                                                      district_year_data = non_missing_data(Kvigue_2010), 
                                                      population_size = kvigue_2010_population_size,
                                                      year_now = 2010, hc_vector= c(3),
                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                      # choose the appropriate method for algorithm
                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                      # use L-BFGS-B for the bbmle package with mle2
                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                      n_iter = nb_iteration)

save(mle_non_epi_kvigueForc_a0_beta0_2010, 
     mle_non_epi_houndeForc_a0_beta0_2009,
     mle_non_epi_houndeForc_a0_beta0_2008,
     mle_non_epi_houndeForc_a0_beta0_2006,
     mle_non_epi_kvigueForc_beta0_2010,
     mle_non_epi_houndeForc_beta0_2008,
     mle_non_epi_houndeForc_beta0_2006,
     mle_non_epi_seguenNonEpiForc_beta0_2010,
     mle_non_epi_seguenNonEpiForc_beta0_2007,
     mle_non_epi_seguenNonEpiForc_a0_2007, 
     file = "R/some_hc_mle_estimates_no_age_str.RData")


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# END SIMULATION OF MODEL WITHOUT AGE STRUCTURE
# 


