insert_age_structure = TRUE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")
#dev.off() # clear all graphical devices and plots in the plotting area.
graphSettings() # seeting plotting parameters

# a function that take as argument a simulation result from sim.SCIRS_harmonic_age()
# and return a data_frame with row sums of Carriers and Susceptibles respectively as well as the time vector

is_a0Constant = FALSE;
#mle_non_epi_seguenNonEpiForc_a0_beta0_2006_age =
toto = mleYearSpecFit(
        district_id = 1,
        district_year_data = non_missing_data(seguen_2006),
        population_size = seguen_2006_population_size,
        year_now = 2006, hc_vector = c(1:4,6),
        a0ForcingOnly =
                FALSE, beta0ForcingOnly = FALSE,
        addCarriageConstrain = FALSE, show_plot = TRUE, verbose = TRUE,
        # choose the appropriate method for algorithm
        # use NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA for LSQ fit with nloptr
        # use L-BFGS-B for the bbmle package with mle2
        algorithm = "NLOPT_LN_COBYLA",  useMLE =
                F, useLSQ = T,
        n_iter = 5000
)
