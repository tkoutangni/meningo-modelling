


##################################################################################
# setting up some initial conditions for the SCIRS_harmonic model
##################################################################################
##################################################################################
# Initial values for numerical simulation of the model.
# 
insert_age_structure = insert_age_structure # change to TRUE to run age structured
#model
# include heterogenous mixing? with the WAIFW matrice estimated by tartof et al?
heterogenous_mixing = heterogenous_mixing # turn this to TRUE to include actual
# heterogenous-mixing with the estimated waifw

nage = 4 # 4 age groupes  defined as <5 years; 5-12 years, 13-19, and 20+ years
# the estimate WAIFW matrix by tartoff et al in dry season with minor epidemics
npop = seguen_2006_population_size$`csps bema`
#npop = 1e+05
#population size (if proportion , model state variables should add up to 1
#fimmune   = 0.2 # fraction of population immune

if(insert_age_structure){
    age_group_fraction = c(0.1871386, 0.1554741, 0.2324747, 0.4249126) # obtained from age distribution of burkina faso population  according to the 4 age groups defined by Tartof et al. (see below for details)
    # age groups : <5 years; 5-12 years, 13-19, and 20+ years
    names(age_group_fraction)<- c("<5 years","5-12 years","13-19 years", "20+ years")
    f = age_group_fraction
    #f = c(0.25,0.25, 0.25, 0.25) # assuming age classes, represent each 25% of the population.
    N =  npop*f  # Actual proportion/number of the total population in each age class
    names(N)<- c("<5 years","5-12 years","13-19 years", "20+ years")
    nage = length(f)
    Carrier_0 = rep(1, nage) # initial proportion of assymp carriers in each age group
    Recov_0 = rep(0, nage)
    Ill_0 = rep(0, nage)
    Susc_0 = N - Carrier_0 - Recov_0 - Ill_0  # initial proportion of Susceptibles in each age
}else{
    N =  npop  
    Carrier_0 = 0.1*N
    Recov_0 = 0
    Ill_0 = 0
    Susc_0 = N - Carrier_0 - Recov_0 - Ill_0  
    
    #Carrier_0 = 0.03 
    #Susc_0    = (pop * (1 - fimmune) - Carrier_0)
    #Ill_0     = 0
    #Recov_0   = (fimmune * pop) 
}

# npop : The total population in proportion or number
# in the population (3% of the initial proportion of carriers)
# Carrier_0 : proportion of carriers initially in the population
# Susc_0 : proportion of susceptibles initially in the population
# Ill_0 : proportion of initial cases in the population.
# Recov_0 : assumes a proportion of the population has temporary natural imunity initial.
# (seroprevalence study can help in infering this initial value.)


# Put model variables initial values into a vector to be passed to SCIRS_harmonic function
inits = c(
    Susc = Susc_0,
    Carrier = Carrier_0,
    Ill = Ill_0,
    Recov = Recov_0
)

################################################
# Model Parameters dictionary.
# parameters units unless specified are day^{-1}
################################################

# beta0 : mean transmission rate.
# a0 : mean invasion rate (infered from koutangni et al.review)
# alpha : rate at which assymptomatic carriers become immune (infered from Tartof S)
# phi : Rate of loss of immunity (assumed same immunity from carriage and disease)
# considered immunity of 1 month to 15 years
# epsilon_a : seasonal forcing parameter for invasion param
# epsilon_b = seasonal forcing parameter for transmission param
# Susc_0 : initial suceptibles
# Carrier_0 : initial carriers
# gamma : meningococcal minigitis case fatality.
# mu : Natural death rate (1/life expectency is 1/54 years in burkina-faso)
# rho : Recovery rate from meningococcal meningitis (meningitis last a week on average)
# teta : timing when invasion or transmission peak relative to january 1st of the year.
# act_comp_mening_death : adjusting for birth rate by accounting for meningitis death.


#===================================
# Unknown/uncertain model parameters
#===================================

year = 365
week = 7
beta0 = 50 / year
a0 = 0.012 / year
alpha = 26 / year
phi = 12 / year
epsilon_a = 0
epsilon_b = 0

##############################################
# Fixed or Infered Parameters (from literature)
##############################################

gamma = 5.2 / year
mu    = (1 / 50) / year
rho   = 52 / year
act_comp_mening_death = 1 # dimensionless
teta  = 91


nage = 4 # 4 age groupes  defined as <5 years; 5-12 years, 13-19, and 20+ years
# the estimate WAIFW matrix by tartof et al in dry season with minor epidemics

#126.8	48.1	47.8	25.2
#144.7	41.7	66.6	24.4
#83.9	232.9	235.3	44.5
#49.6	89.5	76.0	138.1

# assuming at first that contact matrix is homogenous.

C = diag(rep(1,nage))


if(insert_age_structure & heterogenous_mixing){
    C = matrix(data = 0, nrow = nage, ncol = nage)
    #Now using the actual WAIFW matrice estimated by tartof et al.
    C[1,1] = 126.8
    C[1,2] = 48.1
    C[1,3] = 47.8
    C[1,4] = 25.2
    C[2,1] = 144.7
    C[2,2] = 41.7
    C[2,3] = 66.6
    C[2,4] = 24.4
    C[3,1] = 83.9
    C[3,2] = 232.9
    C[3,3] = 235.3
    C[3,4] = 44.5
    C[4,1] = 49.6
    C[4,2] = 89.5
    C[4,3] = 76.0
    C[4,4] = 138.1
}


# Define parameters vector to be passed to the SCIRS_harmonic function
# recall we are estimating intial suceptibles (Susc0) and carriers (CarrierProp)
# at the start of the year as well.
if(insert_age_structure){
    vparameters = c(
        gamma     = gamma,
        beta0     = beta0,
        a0        = a0,
        alpha     = alpha,
        epsilon_a = epsilon_a,
        epsilon_b = epsilon_b,
        mu        = mu,
        teta      = teta,
        rho       = rho,
        phi       = phi,
        act_comp_mening_death = act_comp_mening_death,
        Susc0 = Susc_0,
        CarrierProp = Carrier_0,
        C = list(C)# this is the WAIFW contact matrix as estimated by tartof et al.
    )
}else{
    vparameters = c(
        gamma     = gamma,
        beta0     = beta0,
        a0        = a0,
        alpha     = alpha,
        epsilon_a = epsilon_a,
        epsilon_b = epsilon_b,
        mu        = mu,
        teta      = teta,
        rho       = rho,
        phi       = phi,
        act_comp_mening_death = act_comp_mening_death,
        Susc0 = Susc_0,
        CarrierProp = Carrier_0
    )
}
#####################
# IMPORTANT !!!
#####################

# Should always set the value of "is_a0Constant" to TRUE when simulating the model with no seasonal forcing of invasion parameter. if not invasion is assumed constant over the year by default and a(t) = a0 (i.e., epsilon_a default to 0).
is_a0Constant = FALSE # Default to FALSE

# Initial parameters values guess for optimisation.
initialGuessValues = TRUE
if (initialGuessValues) {
    # params guess lower and upper bound for model calibration with hyperendemic data.
    #guess_lower_bound = c(beta0 =0.00001, alpha=1/year, phi=0.2/year, Susc0=0, CarrierProp=0, teta=91, epsilon_a=0,epsilon_b=0,a0=0.002/30)
    #guess_upper_bound = c(beta0 =+Inf, alpha=52/year, phi=12/year, Susc0=1, CarrierProp=1, teta=112, epsilon_a=1,epsilon_b=1,a0=0.012/30)
    
    # params for model calibration with hyperendemic data.
    guess_lower_bound = c(
        beta0 = 0.00001, alpha = 1 / year, phi = 0.2 / year, Susc0 = (N -(N-1)), CarrierProp =
                (N -(N-1)), teta = 91, epsilon_a = 0,epsilon_b = 0,a0 = 0.002/30 # 1e-12 
    )
    guess_upper_bound = c(
        beta0 = +Inf, alpha = 52 / year, phi = 12 / year, Susc0 = N, CarrierProp =
            N, teta = 112, epsilon_a = 1,epsilon_b = 1,a0 = 0.012/30 #1
    )
    
    initial_guess_parms = c(
        beta0 = 0.5,
        alpha = 12 / year,
        phi = 4 / year,
        Susc0 = 0.5*N,
        CarrierProp = 0.01*N,
        teta = 97,
        epsilon_a = 0,
        epsilon_b = 0,
        a0 = mean(c(0.002 / 30,0.012 / 30))  # 0.005/30.  # mean a0 value in non epidemic
        #a0 = 0.02 / 30
    )
} # end if


# old working initial conditions as of December 15, 2015
#initial_guess_parms=c(
#beta0 = 0.5,
#alpha = 12/year,
#phi = 4/year,
#Susc0=0.5,
#CarrierProp=0.5,
#teta=97,
#epsilon_a=0,
#epsilon_b=0,
#a0= mean(c(0.002/30,0.012/30))  # 0.005/30.
#)
