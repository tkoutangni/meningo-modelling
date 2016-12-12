insert_age_structure = FALSE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")

# replace NA at the begining and end of the data time series by 0
# 
seguen_2006[1,]<-0
seguen_2006[dim(seguen_2006)[1],]<-0

# Probleme 1: Currently my code is designed to fit the weekly proportion of new cases in the population not weekly counts of new cases.

#Solution: Change my code files where appropriate to make the models fit weekly case counts
# instead of weekly cases proportion. This mean model states variables no more sum to 1
# They rather sum to the total population size of the health center year.
# Many lines of code were needed to be changed especially in the parameters file , the fitting 
# algorithm file. and the model equations file.

# Result of implementation of the solution to probleme 1 produced the correct output expected

# Fitting the modified model with no age structure to one health center year for experimental purpose
# The modifications included in the initial model scripts include:
# Simulating the actual new cases counts instead of proportion of new cases.
# The simulated counts are then devided by the actual population size of the healh center
# before model calibration. 

# model calibration with seasonality of invasion parameter only.

N = seguen_2006_population_size$`csps bema`
is_a0Constant = FALSE; insert_age_structure = FALSE; heterogenous_mixing=FALSE;
seguenNonEpiForc_a0_2006_bema = yearSpecFit(district_id=1,
                                       district_year_data = seguen_2006, 
                                       year_now = 2006, hc_vector= c(1),
                                       a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                       addCarriageConstrain = TRUE, show_plot = TRUE,
                                       # NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA
                                       nloptrAlgorithm = "NLOPT_LN_COBYLA",  
                                       n_iter = 1000)

# Calibration with seasonality of transmission alone
is_a0Constant = TRUE; insert_age_structure = FALSE; heterogenous_mixing=FALSE;
seguenNonEpiForc_a0_2006_bema = yearSpecFit(district_id=1,
                                            district_year_data = seguen_2006, 
                                            year_now = 2006, hc_vector= c(1),
                                            a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                            addCarriageConstrain = TRUE, show_plot = TRUE,
                                            # NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA
                                            nloptrAlgorithm = "NLOPT_LN_COBYLA",  
                                            n_iter = 1000)

# Calibration with seasonality of both transmission and invasion parameters.
is_a0Constant = TRUE; insert_age_structure = FALSE; heterogenous_mixing=FALSE;
seguenNonEpiForc_a0_2006_bema = yearSpecFit(district_id=1,
                                            district_year_data = seguen_2006, 
                                            year_now = 2006, hc_vector= c(1),
                                            a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                            addCarriageConstrain = TRUE, show_plot = TRUE,
                                            # NLOPT_LN_BOBYQA or NLOPT_LN_COBYLA
                                            nloptrAlgorithm = "NLOPT_LN_COBYLA",  
                                            n_iter = 1000)

# Probleme 2: Currently my fitting algorithm is designed to use the least squared approach
# for parameter estimation by minimazing the error rate between model predictions and observed data.
# The reviewers suggested to use a Maximum likelihood approach to fitting the model instead as this
# is a more robust approach to parameter estimations.
# 

# Solution to probleme 2: Modify the current fitting alogorithm, namely the objective function 
# to minimise a negative log likelihood function (maximizing the likelihood)
# Assuming that the obeserved data is generated from a poisson distribution
# with parameter lambda = the model prediction of new cases. 
# This solution is implemented but has not successfully calibrate the model yet.
# The modified fitting algo is named mleyearSpecFit.

# Calibrate model with a maximum likelihood based curve fitting
is_a0Constant = FALSE; insert_age_structure = FALSE; heterogenous_mixing=FALSE;
Forc_a0_2006_bema_max_likelihood_estimate = mleYearSpecFit(district_id=1,
               district_year_data = seguen_2006, 
               year_now = 2006, hc_vector= c(1),
               a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
               addCarriageConstrain = FALSE, show_plot = TRUE,
               # choose one of the optim algorithm
               algorithm = "L-BFGS-B",  
               n_iter = NULL)


# Problem 3: Once I'm able to fit the model with no age structure using 
# maximum likelihood estimation, the next problem to solve is to calibrate the model including
# age structure using the already working fitting algorithm.


## Probleme 4 : To calibrate the age structured model with incidence data that is not per age group  
## The callibration script need to account for the fact that the vector of initial guesses of parameter values contains more parameters than when calibrating the model with no age structure. Namely the initial suceptibles and carriers are to be guessed for the 4 age groups during the calibration. 
## 
# Possible solutions to probleme 4:
# 1 - Find the distribution of cases in burkina faso population including the 4 age groups of interest
# Apply that distribution to our local health center incidence data. This approach however is likely
# wrong as the age distribution of cases at national or district level may not apply to each specifique health centers.
# 
# 2 - Modifiy the calibration script so that initial number of Susceptibles and carriers are guessed in all for age groups at a time. then sum up the number of new cases predicted by the model with those initial values and compare tha sum to the observed incidence cases.

# These suggested solutions are not implemented yet.


        



