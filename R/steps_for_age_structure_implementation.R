
# steps for the age structured model.
# - simulate the model with the sim.SCIRS_harmonic_age() function starting with the initial gess_params
# - with each param_guess, simulate the model with age structured model, then sum the variable SCIR across all age group to obtain a single vector per model variables 
# - compare the total_newI vector  with the data
# - When the best param_guess is found (after minimizing the objective function)
# - use that best param_guess to simulate the age structured model, 
# - sum the estimates of NewI accrossball age groups and plot the result against the current health center year data (OK)
# 
# - Store the values of the best param_guess into a data frame similar to fit_out but with more colums
# (including initial suceptibles "Susc0_1 -  Susc0_4" and carriers "CarrierProp_1 -  CarrierProp_4" in the different age groups).
# 
# Files to modify
# - file containing the sim.SCIRS_harmonic_age() function : need to review how the inits vector is update
#  with the new guesses of Susc0_1 -  Susc0_4 and CarrierProp_1 -  CarrierProp_4 (OK)
# - File containg the algo to fit the model: modify guess_parms and the parset_names and so fit_out data-frame  and so fourth
# - population_size should be a vector of nage length as each estimate in each age group get devided by the population size of the age group.
# 
# 
# Important après avoir réusie la simulation du model structurer sur l'age dans les formations sanitaires
# du district seguen en 2006. Veuiller à ce que dans les autres formation année
# la taille de la population N choisi corresponde à la formation année et que la distribution d'age dans cette année donnée soit appliqué à N.
