# This script is to test the fme package with simulated data at first
library(FME)

# function to compute model mean squared residuals (model cost given the current parameters)
# take the following arguments:
    # guess_parms : parameters to be optimised over or estimated (uknown parameters)
    # parmset : a vector containing names of the parameters to be optimsed or estimated.

Objective <- function(guess_parms, parmset = names(guess_parms)) {
    current_parms_combination = PARMS_SCIRS_f
    current_parms_combination[parmset] <- guess_parms
    #print(guess_parms*year)
    #tout <- seq(0, 50, by = 0.5)
    ## output times
    out <- sim.SCIRS_harmonic(inits,current_parms_combination,nd=20*year)
    out <- subset(out, select = c(time,newI.Carrier))
    names(out)[2]="incid" # rename the incidence column to 
    ## Compute and return model cost (sum of squared residuals)
    return(modCost(obs = simulated.data, model = out))
}


## simulating a data using a givien combination of parameters to test if algorithm work well
data_parms <- PARMS_SCIRS_f
simulated.data <- sim.SCIRS_harmonic(inits,data_parms,nd=20*year)
simulated.data <- subset(simulated.data, select = c(time,newI.Carrier))
set.seed(1234)
simulated.data <- simulated.data+(runif(dim(simulated.data)[1],min=1e-05,max=3e-04))
names(simulated.data)[2]="incid"

guess_parms=c(
    beta0 = 100/year, # theoriquement 50
    alpha = 50/year,  # theoriquement 26
    phi = 0.4/year    # theoriquement 12
)

#Optimisation using modFit start Here.

Fit <- modFit(p = guess_parms, f = Objective, method="Nelder-Mead", 
    control=list(maxit=1000,trace=3),
    lower = c(beta0 = 30/year, alpha=1/year, phi=0.2/year),
    upper = c(beta0 =200/year, alpha=52/year, phi=52/year)
)

# print the estimated values of par  
Fit$par

## Simulate model with estimated parameters values and compare the trajectory against data
fitted_parms <-PARMS_SCIRS_f # copying the initial parameters vector
fitted_parms[names(Fit$par)]<-Fit$par # replacing the unknown parameters values with estimated values

fitted_model <- sim.SCIRS_harmonic(inits,fitted_parms,nd=20*year) # run model with those parameters
fitted_model <- subset(fitted_model, select = c(time,newI.Carrier))

# ploting
plot(simulated.data*1e+05,las=1,pch=20, col="black",xlab="time",type="l",lwd=3)
lines(fitted_model*1e+05,col="blue",lwd=3)


#decomp_toto = decompose(simulated.data*1e+05,type="additive")
#plot(decomp_toto)
#plot(decomp_toto$x,main="")
#lines(decomp_toto$seasonal,col="blue")
#lines(decomp_toto$trend,col="red")
