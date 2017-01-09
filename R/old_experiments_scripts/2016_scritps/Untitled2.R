# test simulating homogenous model and age structured model.
# homogenous model simulation
# insert_age_structure must be  TRUE in models_parameter
homogenous_model = sim.SCIRS_harmonic(inits, vparameters, 5*year)
# age structured model simulation
# insert_age_structure must be  TRUE in models_parameter
age_structured_model = sim.SCIRS_harmonic_age(inits, vparameters, 5*year)
# plot one variable at a time.
#op <- par(mar=c(5, 6, 4, 2) + 0.1, mfrow = c(2, 1))
op<-par(mar=c(5, 6, 4, 2), mfrow = c(2, 1), oma = c(0.1, 3, 0.1, 0.1))
par(op)
plotModelVariable(homogenous_model$time, homogenous_model$newI, perPop = 1)
drawCostumXaxisTicks(homogenous_model)
title(main='Unforced model with homogenous mixing\n Population = 1 Million', ylab="New cases")


plotModelVariable(age_structured_model$time, age_structured_model$newI1*4, perPop = 1)
drawCostumXaxisTicks(age_structured_model)
title(main='Unforced model with heterogenous mixing\n Population = 1 Million', ylab='New cases')


#test = read.csv(file = "../data/raw_data/data_fev_2015/census_data_burkina_200#4-2013.csv", header = TRUE, sep = ',', quote="", skip = 1)

# Model calibration with homogenous mixing assumption.

is_a0Constant = FALSE
toto = yearSpecFit(district_id=1,
district_year_data = seguen_2006,
year_now = 2006, hc_vector= c(1),
a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE,
addCarriageConstrain = FALSE, show_plot = TRUE,
nloptrAlgorithm = "NLOPT_LN_BOBYQA",  # NLOPT_LN_COBYLA , NLOPT_LN_BOBYQA
n_iter = 1000)


toto_mle_estimate = mleYearSpecFit(district_id = 1, district_year_data = seguen_2006, year_now = 2006, hc_vector = c(1),a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, addCarriageConstrain = FALSE, algorithm = NULL, show_plot = TRUE,n_iter = NULL)




