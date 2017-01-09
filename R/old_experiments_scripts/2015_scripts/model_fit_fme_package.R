library(FME)
library(nloptr)
source("loadFunctions.R")
source("GeneralDataLoading.R")
source("data.cleaning.R")

source("data_simple_mooving_week_average.R")

nbYearSimulated=1

# function to compute model mean squared residuals (model cost given the current parameters)
# take the following arguments:
# guess_parms : parameters to be optimised over or estimated (uknown parameters)
# parmset : a vector containing names of the parameters to be optimsed or estimated.

Objective <- function(guess_parms, parmset = names(guess_parms)) {
    
    current_parms_combination = PARMS_SCIRS_f
    names(guess_parms) = parmset
    current_parms_combination[parmset] <- guess_parms
    #print(guess_parms*year)
    
    out <- sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated*year)
    out <- subset(out, select = c(time,newI))
    #print(dim(out))
    #predict = out$newI[(dim(out)[1]-49):dim(out)[1]]
    predict = tail(out$newI,length(time.vector))
    #print(predict)
    #print(dim(predict))
    #predict = out$newI[out$time%in%(time.vector+1)] # take values of model output at time in days corresponding to obs data
    #out = data.frame(time = time.vector[-c(51:52)], incid = predict)
    out = data.frame(time = time.vector, incid = predict)
    matplot(cbind(obs.data$incid,out$incid), type="b",pch=20,las=1,col=c("black","blue"))
    print(length(obs.data$incid))
    print(length(out$incid))
    ## Compute and return model cost (sum of squared residuals)
    mc=modCost(obs = obs.data, model = out) #, weight="mean"
    #print(mc$model)
    return(mc$model)
    #return(list(predict,out))
}

time.vector = c(coredata(seguen_2010$julian_day)) # the time vector at with to compare model to data
obs.data = subset(seguen_2010,select=c(1,7))
#plot(obs.data[,1]*1e+05,type="b",pch=20,las=1,ylab="Incid",xlab="Months/Weeks")
obs.data = data.frame(time = coredata(obs.data[,2]), incid = coredata(obs.data[,1]))
#obs.data = obs.data[c(-1,-52),] # remoove the first row with Na



guess_parms=c(
    beta0 = 100/year,
    alpha = 50/year,
    phi = 4/year, 
    Susc0=0.77, 
    CarrierProp=0.13, 
    teta=205, 
    epsilon_a=0,
    epsilon_b=0
    #a0=0.012/year
)

#Optimisation using modFit 
useFME=FALSE
if(useFME){
    Fit <- modFit(p = guess_parms, f = Objective, method="L-BFGS-B", 
                  control=list(maxit=1000,trace=3, reltol=1e-4),
                  lower = c(beta0 = 30/year, alpha=1/year, phi=0.2/year, Susc0=0.2, CarrierProp=0, teta=70, epsilon_b=0), #a0=0.012/year
                  upper = c(beta0 =200/year, alpha=52/year, phi=52/year, Susc0=1, CarrierProp=1, teta=200, epsilon_b=1)
    )    
}else{
    Fit <- nloptr(eval_f= Objective, x0 = guess_parms, parmset=names(guess_parms),
                  opts=list("algorithm"="NLOPT_LN_BOBYQA", "print_level"=3, 
                            "xtol_rel"=1e-4, "maxeval"= 1000),
                  lb = c(beta0 = 30/year, alpha=1/year, phi=0.2/year, Susc0=0.2, CarrierProp=0, teta=20, epsilon_a=0,epsilon_b=0), # a0=0.012/year
                  ub = c(beta0 =200/year, alpha=52/year, phi=52/year, Susc0=1, CarrierProp=0.30, teta=220, epsilon_a=1,epsilon_b=1)  # a0=1.2/year 
    )
    
}

# print the estimated values of par  
Fit$par = Fit$solution
names(Fit$par) = names(guess_parms)

#Fit$solution
## Simulate model with estimated parameters values and compare the trajectory against data

fitted_parms <-PARMS_SCIRS_f # copying the initial parameters vector
fitted_parms[names(Fit$par)]<-Fit$par # replacing the unknown parameters values with estimated values

fitted_model <- sim.SCIRS_harmonic(inits,fitted_parms,nd=nbYearSimulated*year) # run model with those parameters
fitted_model <- subset(fitted_model, select = c(time,newI))
#fitted_model <- fitted_model$newI[fitted_model$time%in%(time.vector+1)]
fitted_model = tail(fitted_model$newI,length(time.vector))


# ploting
plot(obs.data[,2]*1e+05,las=1,pch=20, col="black",xlab="",type="o",ylim=c(0,(1.1*max(obs.data[-c(1,52),2]*1e+05))),ylab="",)
lines(fitted_model*1e+05,col="blue",lwd=3,type="b",pch=20)

title(main="Csps Bema, (SEGUENEGA District)",xlab="Calendar weeks (2010)")




