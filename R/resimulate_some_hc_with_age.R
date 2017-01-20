# model resimulation for health center were objective function return NAN
insert_age_structure = TRUE; heterogenous_mixing=TRUE
source("R/run_first.R")
source("R/models_parameters.R")
#dev.off() # clear all graphical devices and plots in the plotting area.
graphSettings() # seeting plotting parameters


#MODEL WITH AGE STRUCTURE
start.time <- Sys.time()
nb_iteration = 40000
#MODEL 2
is_a0Constant = TRUE
mle_age_str_non_epi_seguenNonEpiForc_beta0_2006 = mleYearSpecFit_age_str(district_id=1,
                                                         district_year_data = non_missing_data(seguen_2006), 
                                                         population_size = seguen_2006_population_size,
                                                         year_now = 2006, hc_vector= c(3,6),
                                                         a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                         addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                         # choose the appropriate method for algorithm
                                                         # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                         # use L-BFGS-B for the bbmle package with mle2
                                                         algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                         n_iter = nb_iteration)


mle_age_str_non_epi_seguenNonEpiForc_beta0_2009 = mleYearSpecFit_age_str(district_id=1,
                                                         district_year_data = non_missing_data(seguen_2009), 
                                                         population_size = seguen_2009_population_size,
                                                         year_now = 2009, hc_vector= c(6),
                                                         a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                         addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                         # choose the appropriate method for algorithm
                                                         # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                         # use L-BFGS-B for the bbmle package with mle2
                                                         algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                         n_iter = nb_iteration)

mle_age_str_non_epi_seguenNonEpiForc_beta0_2010 = mleYearSpecFit_age_str(district_id=1,
                                                         district_year_data = non_missing_data(seguen_2010), 
                                                         population_size = seguen_2010_population_size,
                                                         year_now = 2010, hc_vector= c(2,3,5),
                                                         a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                         addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                         # choose the appropriate method for algorithm
                                                         # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                         # use L-BFGS-B for the bbmle package with mle2
                                                         algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                         n_iter = nb_iteration)

mle_age_str_non_epi_houndeForc_beta0_2006 = mleYearSpecFit_age_str(district_id=2,
                                                   district_year_data = non_missing_data(hounde_2006), 
                                                   population_size = hounde_2006_population_size,
                                                   year_now = 2006, hc_vector= c(4,5,7,10),
                                                   a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                   addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                   # choose the appropriate method for algorithm
                                                   # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                   # use L-BFGS-B for the bbmle package with mle2
                                                   algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                   n_iter = nb_iteration)

mle_age_str_non_epi_houndeForc_beta0_2008 = mleYearSpecFit_age_str(district_id=2,
                                                   district_year_data = non_missing_data(hounde_2008), 
                                                   population_size = hounde_2008_population_size,
                                                   year_now = 2008, hc_vector= c(8,18),
                                                   a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                   addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                   # choose the appropriate method for algorithm
                                                   # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                   # use L-BFGS-B for the bbmle package with mle2
                                                   algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                   n_iter = nb_iteration)
##################################################################
##################################################################
# MODEL 3 
is_a0Constant = FALSE

mle_age_str_non_epi_seguenNonEpiForc_a0_beta0_2006 = mleYearSpecFit_age_str(district_id=1,
                                                                            district_year_data = non_missing_data(seguen_2006), 
                                                                            population_size = seguen_2006_population_size,
                                                                            year_now = 2006, hc_vector= c(2),
                                                                            a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                                            addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                                            # choose the appropriate method for algorithm
                                                                            # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                                            # use L-BFGS-B for the bbmle package with mle2
                                                                            algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                                            n_iter = nb_iteration)


mle_age_str_non_epi_houndeForc_a0_beta0_2004 = mleYearSpecFit_age_str(district_id=2,
                                                                      district_year_data = non_missing_data(hounde_2004), 
                                                                      population_size = hounde_2004_population_size,
                                                                      year_now = 2004, hc_vector= c(13),
                                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                                      # choose the appropriate method for algorithm
                                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                                      # use L-BFGS-B for the bbmle package with mle2
                                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                                      n_iter = nb_iteration)


mle_age_str_non_epi_houndeForc_a0_beta0_2006 = mleYearSpecFit_age_str(district_id=2,
                                                                      district_year_data = non_missing_data(hounde_2006), 
                                                                      population_size = hounde_2006_population_size,
                                                                      year_now = 2006, hc_vector= c(4),
                                                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                                      # choose the appropriate method for algorithm
                                                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                                      # use L-BFGS-B for the bbmle package with mle2
                                                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                                      n_iter = nb_iteration)


## === DISTRICT OF K.VIGUE (id = 3) === ###
mle_age_str_non_epi_lenaForc_a0_beta0_2006 = mleYearSpecFit_age_str(district_id=3,
                                                                    district_year_data = non_missing_data(lena_2006), 
                                                                    population_size = lena_2006_population_size,
                                                                    year_now = 2006, hc_vector= c(1),
                                                                    a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                                    addCarriageConstrain = FALSE, show_plot = TRUE, verbose = FALSE,
                                                                    # choose the appropriate method for algorithm
                                                                    # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                                                    # use L-BFGS-B for the bbmle package with mle2
                                                                    algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                                                    n_iter = nb_iteration)

save(mle_age_str_non_epi_seguenNonEpiForc_beta0_2006,
     mle_age_str_non_epi_seguenNonEpiForc_beta0_2009,
     mle_age_str_non_epi_seguenNonEpiForc_beta0_2010,
     mle_age_str_non_epi_houndeForc_beta0_2006,
     mle_age_str_non_epi_houndeForc_beta0_2008,
     mle_age_str_non_epi_seguenNonEpiForc_a0_beta0_2006,
     mle_age_str_non_epi_houndeForc_a0_beta0_2004,
     mle_age_str_non_epi_houndeForc_a0_beta0_2006,
     mle_age_str_non_epi_lenaForc_a0_beta0_2006,
     file="R/some_hc_mle_estimates_with_age_str.RData")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
