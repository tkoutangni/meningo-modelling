##################################################################################
##################################################################################
# An R script to solve ODE's of an SCIRS model with a harmonic disease invasion rate or 
# an harmonic transmission rate.
#  
#
# Author: Thibaut Koutangni --
#         thibautkoutangni@gmail.com
# Created: July 28, 2015
#
# Copyright Thibaut Koutangni, 2015
##################################################################################
##################################################################################
# this is a function which, given a value of S, C, I and R at time t
# calculates the time derivatives dS/dt; dC/dt; dI/dt et dR/dt of S C I and R
# vparameters is a vector/list containing the parameters of the model, like the
# recovery period (gamma), and the baseline transmission rate (beta0),
# the day of the year when transmission is maximal (teta) when seasonal forcing 
# function explitely include teta,the relative
# the degree of seasonal forcing (epsilon), the baseline disease invasion rate a0 etc...
#
# Unlike the unforced SCIRS model, the harmonic forcing of the transmission
# rate and the disease invasion rate makes this set of ODE's non-autonomous.  
# This means that the results of the model will depend on the time of introduction 
# of the meningococcus (new pathogenic strain for examplz), relative to the day of the 
# year when transmission is maximal!
#
# This function gets passed to the ODEsolve package
# Model assumes immunity from both assymptomatic carriage and disease.But it is still
# unclear whether carriers transmit the disease more than disease.Eventhough it is quite
# intuitive that diseased would transmit less than assymptomatic carriers due to disease
# severity (patient are bound to bed and thus are likely to transmit less). This is not explicitely 
# incorporated in the model.
##################################################################################

# Defining a new SCIRS model that outpout the number of incident cases rather than incidence rates
library(deSolve)
rm(list=ls())

par(cex.lab=1.2)
op <- par(fig=c(0,1,0,1),mfrow=c(2,2),
          mar=c(3,3,1,1),mgp=c(2,1,0))

source("newFunctions.R")

SCIRS_harmonic_new <- function(t,x,params){
    Susc    = x[1]  # the value of S at time t
    Carrier = x[2]  # the value of C at time t
    Ill     = x[3]  # the value of I at time t
    Recov   = x[4]  # the value of R at time t
    
    # Cross check to ensure that we always have values of S, C, I , R, S that make sens : non-negative counts at least
    
    if (Susc<0 | Carrier<0 | Ill<0 | Recov<0 | is.na(Susc) | is.na(Carrier) | is.na(Ill) | is.na(Recov)){
        #cat("\n Negative or missing values of one of the state variables is being returned.\n
            #Something is propabily going wrong! check your parameters values or the model fomulation")
        Susc = 0; Carrier = 0; Ill = 0; Recov = 0
        
        #cat("\n For the mean time you forced any returned negative or missing variable values to be set to 0")
    }
    
    # Specifying the model parameters
    gamma <- params["gamma"]
    beta0 <- params["beta0"]
    a0 <- params["a0"]
    alpha <- params["alpha"]
    epsilon_a <- params["epsilon_a"]
    epsilon_b <- params["epsilon_b"]
    mu <- params["mu"]
    teta <- params["teta"]
    rho <- params["rho"]
    phi <- params["phi"]
    act_comp_mening_death <- params["act_comp_mening_death"]
    popsize <- params["popsize"]
    fCarrier <- params["fCarrier"]
    fImmune<- params["fImmune"]
    
    
    # specifying the seasonal forcing function for the transmission and invasion rate.
    year = 365 # number of days in a year.
    
    # a is forced with a minimum 10*fold increase of a0 when epsilon = 0. When  forcing amplitude is 0; a = 10*a0 --> constant but scaled by a factor 10. 
    
    a  = a0 * ((4.5+45*epsilon_a)*cos(2*pi*(t-teta)/year)+(5.5+45*epsilon_a))
    
    #a is forced with a non-scaled cosine function. When forcing amplitude is 0;   a = a0 --> unforced and unscaled constant 
    
    #a    = a0*(1+epsilon_a*cos(2*pi*(t-teta)/year))
    
    # beta is forced with a non-scaled cosine function. When forcing amplitude is 0;  beta = beta0 --> unforced constant 
    
    beta    = beta0*(1+epsilon_b*cos(2*pi*(t-teta)/year))
    
    
    # Flow of individuals from one compartment to another
    f_SC=beta*Susc*(Carrier+Ill)
    f_CI=a*Carrier
    f_IR=rho*Ill
    f_RS=phi*Recov
    f_CR=alpha*Carrier
    
    
    # Now defining derivatives of S, C, I and R
    
    # b the birth rate = natural death rate + meningitis death rate . 
    # the term meningitis death rate was added to compensate the birth rate for population constancy
    # so that population size becomes popsize
    
    b = mu + act_comp_mening_death*(gamma*Ill) #act_comp_mening_death : 0 or 1 to activate compensation of meninge death throught extra births
    
    dSusc <- f_RS - f_SC/popsize - mu*Susc + b
    
    dCarrier <- f_SC/popsize - f_CI - f_CR - mu*Carrier
    
    dIll <- f_CI - f_IR - (mu+gamma)*Ill
    
    dRecov <- f_IR + f_CR - f_RS - mu*Recov
    
    out <- c(dSusc, dCarrier, dIll, dRecov)

    list(out, c(newI = as.numeric(f_CI))) # ,f_CR = f_CR, f_IR = f_IR, f_SC = f_SC, f_RS = f_RS
}


## Numerical simulation of the SCIRS_harmonic_new

## important : the time step of model output should always reflect time unit of model input parameters
## in our simulation we choose 1 day as time step for the simulation.

SCIRS_prediction <- function(params, times){ # the time vector will be determined by the time vector in the weekly cases report data set.
    # The Starting Value of S, C, I, and R respetively. The order of variables are important
    xstart<- c(
        Susc_0 = (popsize*(1 - as.numeric(params["fImmune"]))) - (popsize*(as.numeric(params["fCarrier"]))),
        Carrier_0 = popsize*(as.numeric(params["fCarrier"])), 
        Ill_0 = 0,
        Recov_0 = popsize*(as.numeric(params["fImmune"]))
    )
    
    pred <- as.data.frame(lsoda(func = SCIRS_harmonic_new, y = xstart, times = times, parms = params))
    names(pred) = c("time","S","C","I","R","newCases")
    #pred = pred[-1,] # remoove the starting value à t = 0
    #pred[,"newCases"] = pred[,"newCases"]*7 # output the number of new cases over a week instead of daily. This approximation assumes number of new cases per day is constant over a week.
    return(pred)
}



# Defining model parameters
#==============================
# Unknown/uncertain parameters
#==============================
# parameters units when appropriate are in in day^{-1} where appropriate
year = 365
# Unknown Paramters 
beta0=70/year        # in days^{-1} baseline or mean transmission rate.
a0=0.012/year        # in days^{-1} baseline invasion rate (infered from koutangni et al.review of case-carrier ratio): case-carrier ratio max value in endemic season
alpha=26/year        # rate at which assymptomatic carriers become immune. (from S.tartoff)
phi=12/year         # rate of loss of immunity(assumed duration of immunity is the same for carriers and diseased): considered immunity of 1 month to 15 years
epsilon_a =0        # assume no seasonal forcing of the invasion rate at first
epsilon_b =0        # assume no seasonal forcing of the transmission rate at first

# fixed parameters
#=============================
gamma = 5.2/year         # meningococcal minigitis case fatality.
mu    = (1/50)/year      # in days^{-1} Natural death rate (1/life expectency is 1/50 years in the belt)
rho   = 52/year          # in days^{-1} Recovery rate from meningococcal meningitis:assumed disease last a week (7 days)
act_comp_mening_death=1  # fixed: activate compensation for meninge death
teta  = 91               # assume Jan 1st is day 0, and this is when meningococcal is most transmissible
# day, relative to Jan 1st, that meningo is most transmissible
#=============================

popsize = 111540
#unknown initial conditions (to estimate)
fCarrier = 0.01 # initial proportion of carriers in the population.
fImmune = 0.2 # assumed pre-immune fraction in the population

##################################################################################
# setting up some initial conditions at time t=0 for the SCIRS_harmonic model
##################################################################################           
# proportion of susceptibles initially in the population
# proportion of cases initially in the population
# assumes initially, a proportion of the population is partially immune (either by recovering from carriage or from disease)
# in case we assume initially some people have immunity from either carriage or disease then care must be taking to decide the appropriate

# see model SCIRS_prediction function for xstart vector computation.

# defining model parameters vector.
params = c(
    gamma = gamma,
    beta0 = beta0,
    a0 = a0,
    alpha = alpha,
    epsilon_a = epsilon_a,
    epsilon_b = epsilon_b,
    mu = mu,
    teta = teta,
    rho = rho,
    phi = phi,
    act_comp_mening_death = act_comp_mening_death,
    popsize = popsize,
    fCarrier = fCarrier,
    fImmune = fImmune
)

toto = SCIRS_prediction(params,times = c(0,seq(1,1*year, 1)))
toto = toto[-1,]

#plot(toto[,"newCases"],type="l")

plot(floor(sumCasesWeekly(toto[-1,])),type="l")



