##################################################################################
##################################################################################
# R script to solve ODE's of an SCIRS model with a harmonic transmission and / or invasion rate.
# 
# Author: Thibaut Koutangni 
# Contact : thibautkoutangni@gmail.com
# Created: Oct 2014
# Copyright: Thibaut Koutangni, 2014.

##################################################################################
#installPackage("deSolve") # install package using my costum function "installPackage"
#require("deSolve") # package needed for numerically solving the ODEs
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

SCIRS_harmonic_age = function(t, x, vparameters){
    nage = length(x)/4 # because the population was stratified into 4 age groups:
    Susc    = as.numeric(x[1:nage])  # the value of S at time t
    Carrier = as.numeric(x[(nage+1):(2*nage)])  # the value of C at time t
    Ill     = as.numeric(x[(2*nage+1):(3*nage)])  # the value of I at time t
    Recov   = as.numeric(x[(3*nage+1):(4*nage)])  # the value of R at time t
    # Cross check to ensure that we always have sensical or positive values of model state
    # variables
    #stopifnot(Susc>=0, Carrier>=0, Ill>=0, Recov>=0)
    with(as.list(vparameters),{
        N = Susc + Carrier + Ill + Recov
        # seasonal forcing function to allow variation of invasion param.
        # of up to 100 -fold a0
        if(is_a0Constant){
            epsilon_a = 0
            a  = a0 * ((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a))
            # keeps a(t) constant by setting epsilon_a to 0 by default
        }else{
            a  = a0 * ((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a))
            # a(t) change in a cyclical way as epsilon_a is not null
            # We could have use :  a = a0*(1+epsilon_a*cos(2*pi*(t-teta)/year))
            # if we didn't want to explicitely impose variations of up to 100 fold 
            # on a(t)
        } #end if else
        
        # seasonal forcing transmission parameter
        beta = beta0*(1+epsilon_b*cos(2*pi*(t-teta)/year))
        # Unclear what the magnitude of variations of the transmission parameter
        # could be. So we don't impose on any range and would rather
        # calculate the fold change or increase post parameter estimation.
        # All we know is the expected absolute difference in the carriage prevalence 
        # between season, and point estimates of carriage prevalence in the dry and
        # wetseason (check Kristensen et al or leimkugel et al.)
        
        # Flow of individuals from one compartment to another
        # Defining the matrix of WAIFW
        
    
        f_SC = (beta*Susc)*(as.matrix(C) %*%(Carrier+Ill)/N)
        f_CI = a*Carrier
        f_IR = rho*Ill
        f_RS = phi*Recov
        f_CR = alpha*Carrier
        
        # b the birth rate = natural death rate + meningitis death rate . 
        # the term meningitis death rate was added to compensate the birth rate for
        # population constancy so that population size becomes 1 (in proportion)
        
        b = mu + act_comp_mening_death*(gamma*Ill) 
        # act_comp_mening_death : 0 or 1 (to compensate
        # meningitis death throught extra births) default to 1.
        
        # Now defining derivatives of S, C, I and R
        dSusc<- f_RS - f_SC - mu*Susc + b
        
        dCarrier<- f_SC - f_CI - f_CR - mu*Carrier
        
        dIll<- f_CI - f_IR - (mu+gamma)*Ill
        
        dRecov<- f_IR + f_CR - f_RS - mu*Recov
        
        out=c(dSusc, dCarrier, dIll, dRecov)
        
        list(out,
             c(
                 newI=f_CI,
                 f_CR=f_CR,
                 f_IR=f_IR,
                 f_SC=f_SC, 
                 f_RS=f_RS)
        )
    }) # en with funtion.
} # end SCIRS_harmonic_CI function

#####################################################
# Numerical simulation of the harmonic SCIRS Model.
#####################################################

# vt = seq((startyear-2004)*year, ((startyear-2004)+n_years)*year, by=week)  
# times at which explicit estimates for x should be returned.
# vt.date = as.Date("2004-01-01") + vt # we have data starting from 2004
# The first value in the vt times vector must be the initial time.

sim.SCIRS_harmonic_age <- function(inits, vparameters, nd){
    # @ parameters: 
    # inits is the initial conditions vector,
    # vparameters the vector of parameters of the model,
    # nd the number of days to simulated the model.
    day = 1       # default time step of the model integration
    ts = day 
    week = 7*ts   # for returnin results evry week
    year = 365*day
    nd = nd       # nd stands for number of days model will run.
    t_start = 0   # simulation starting time
    t_end = nd    # simulation end time.
    t_inc = week  # time increment state variable are computed daily but .
    
    t_range = seq(t_start, (t_end + t_inc), t_inc) # for weekly output
    #t_range = seq(t_start, (t_end + ts), ts) # for daily output
    
    if(sum(names(vparameters)%in%"Susc0")>0){
        # update the values of inital suceptibles and carriers
        # in the inits vector, with their respective estimated values (Susc0, CarrierProp)
        # and compute the initial recovered based on initial Susc0 and CarrierProp values
        # I will rename CarrierProp to Carrier0 through all script later for less confusion.
        
        inits["Carrier"] = as.numeric(vparameters["CarrierProp"]) 
        inits["Susc"] = as.numeric(vparameters["Susc0"])
        inits["Recov"] = N - as.numeric(inits["Susc"] - inits["Carrier"])
        vparameters = vparameters[1:11]
        
        # old script.
        #inits["Susc"] = as.numeric(vparameters["Susc0"])
        #inits["Carrier"] = (1-inits["Susc"])*as.numeric(vparameters["CarrierProp"])
        #inits["Recov"] = as.numeric(1-inits["Carrier"]-inits["Susc"])
        #print(inits)
        #vparameters=vparameters[1:11]
    } # end if statement.
    
    # Simulate now and return model predictions
    SCIRS_harmonic_age.out = as.data.frame(lsoda(y=inits, times=t_range, 
                                             func=SCIRS_harmonic_age, parms=vparameters))  
    # Gross approximation of weekly incidences from daily estimates of state  variables.
    # daily prediction of newI in each age groupe are somewhat similar within a given week.
    SCIRS_harmonic_age.out[,c(18:21)] = SCIRS_harmonic_age.out[,c(18:21)]*week 
    return(SCIRS_harmonic_age.out)
    # also tried an integral function to compute weekly cases, but the results are similar
    # that of the Gross approximation above.
}






