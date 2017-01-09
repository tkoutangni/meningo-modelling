#################################################################
# Loading functions, parameters, and data
#################################################################
#rm(list=ls()) # clean current work environment
#################################################################
source("R/rpackages.R") # must load first ! important
source("R/myFunctions.R",verbose = FALSE) # must load second!!.
# MODELS PARAMETERS ARE LOADED INTO WORKSPACE WHEN NEEDED
#source("R/models_parameters.R") # load models parameters
# SCIRS model with seasonal forcing, and immunity from cariage and disease.
source("R/SCIRS_harmonic.R")      # Immunity from C and I
source("R/SCIRS_harmonic_age.R")  # age structured and Immunity from C and I

##### ===================== Data scripts =====================
source("R/GeneralDataLoading.R") # load data
source("R/data.cleaning.R")      # clean data
source("R/data_simple_mooving_week_average.R")
source("R/data_cleaning_continiue.R")



##### =============== Models fitting algo ===========
#source("R/yearSpecificFittingAlgo_nov_2015.R",verbose = FALSE)
source("R/mleYearSpecFit.R")
source("R/mleYearSpecFitAgeStructure.R")

##### Get age distribution of the burkina faso population according
##### to years.
source("R/get_population_age_data.R") 

### end loading.
cat('\n Finished loading functions, parameters, and data \n')

source("R/plot_all_non_epi_data.R"); print(hyperendemic_data_plots) # visualize observed data

cat('\n Finished ploting the health centers data \n')
cat(" \n================================================ \n")
cat('\n Model estimation can be run now for each of the 3 models \n')

# other tested models.

#source("SCIS_harmonic.R")         # SCIS model.
#source("SCIRS_harmonic_I.R")      # Immunity from I Only.
#source("SCIRS_harmonic_CI_alt.R") # immunity from C and I with
# a small proportion of susceptibles moving to I without passing through C.

