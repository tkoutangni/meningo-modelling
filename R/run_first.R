#################################################################
# Loading functions, parameters, and data
#################################################################
#rm(list=ls()) # clean current work environment
#################################################################
source("R/myFunctions.R",verbose = FALSE) # must load first.
source("R/rpackages.R") # must load second
source("R/models_parameters.R") # load models parameters
# SCIRS model with seasonal forcing, and immunity from cariage and disease.
source("R/SCIRS_harmonic.R")      # Immunity from C and I
source("R/SCIRS_harmonic_age.R")  # age structured and Immunity from C and I

##### ===================== Data scripts =====================
source("R/GeneralDataLoading.R") # load data
source("R/data.cleaning.R")      # clean data
source("R/data_simple_mooving_week_average.R")

##### =============== Models fitting ===========
source("R/yearSpecificFittingAlgo_nov_2015.R",verbose = FALSE)

### end loading.
cat('\n Finished loading functions, parameters, and data \n')

# other tested models.

#source("SCIS_harmonic.R")         # SCIS model.
#source("SCIRS_harmonic_I.R")      # Immunity from I Only.
#source("SCIRS_harmonic_CI_alt.R") # immunity from C and I with
# a small proportion of susceptibles moving to I without passing through C.

