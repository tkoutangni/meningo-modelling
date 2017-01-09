# experiemnts to improve models fits with likelihood estimation
# inquality constrain on carriage has been set purposefully to FALSE because even without 
# this constrain, the condition defined in the inequality is already met in most case.
# This also increase the ability of the model to performe a better fit.
insert_age_structure = FALSE ; heterogenous_mixing = FALSE
source("R/run_first.R"); source("R/models_parameters.R")

start.time <- Sys.time()
mle_toto_30K = mleYearSpecFit(district_id=1,
               district_year_data = non_missing_data(seguen_2006), 
               population_size = seguen_2006_population_size,
               year_now = 2006, hc_vector= c(2:3),
               a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
               addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
               # choose the appropriate method for algorithm
               # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
               # use L-BFGS-B for the bbmle package with mle2
               algorithm = "NLOPT_LN_BOBYQA",  useMLE=F, useLSQ = F,
               n_iter = 1)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#=====================================================================================================
# NLOPT_LN_BOBYQA provides better fit of pick values with likelihood estimation
# but probleme is it tend to inflate epsilon_b up to 1 to achieve such a perfect fit.
start.time <- Sys.time()
mle_toto_30K_modif13 = mleYearSpecFit(district_id=1,
                                      district_year_data = non_missing_data(seguen_2006), 
                                      population_size = seguen_2006_population_size,
                                      year_now = 2006, hc_vector= c(1:4,6),
                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                      # choose the appropriate method for algorithm
                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                      # use L-BFGS-B for the bbmle package with mle2
                                      algorithm = "NLOPT_LN_BOBYQA",  useMLE=F, useLSQ = F,
                                      n_iter = 30000)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#=====================================================================================================
# age structure model estimation with maximum likelihood start here
insert_age_structure = TRUE ; heterogenous_mixing = TRUE
source("R/run_first.R"); source("R/models_parameters.R")

start.time <- Sys.time()
mle_age_str_30K_modif1 = mleYearSpecFit_age_str(district_id=1,
                                      district_year_data = non_missing_data(seguen_2006), 
                                      population_size = seguen_2006_population_size,
                                      year_now = 2006, hc_vector= c(1:4,6),
                                      a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                      addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
                                      # choose the appropriate method for algorithm
                                      # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
                                      # use L-BFGS-B for the bbmle package with mle2
                                      algorithm = "NLOPT_LN_COBYLA",  useMLE=F, useLSQ = F,
                                      n_iter = 1)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


