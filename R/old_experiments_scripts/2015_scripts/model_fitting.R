## ========================================================================
## Models fitting and parameters estimation.
## =========================================================================
require(deSolve)
# source("R/loadFunctions.R")

local({
  load("all_workspace_vars.RData")  # loading my workspace with vars
  ls()
})

## definition of least_square function and computation
par$amp_a=0.5
par$amp_b=0.4

least_square_function <- function(par){
  parameters=PARMS_SICRS_CI_BD_WI_forced_1 # forcing the transmission rate only (Model1)
  for(i in 1:length(names(par))){
    parameters[[names(par)[i]]]=par[[i]]
  }
  
  print(parameters)
  incidence.model1=forced_SCIRS_CI(state=start.SCIRS_CI, parms=parameters, nbyears=7.961538) # same length
  # as available data. nbyears is not an integer value. 2011 is missing from data.
  least_squares = sum((incidence.model1[,6]-(weekly_inc_goubre))^2)/mean(incidence.model1[,6])
  print(least_squares)
  return(least_squares)
}

### Model fitting and parameters optimisation.

initialParameters=c(
  beta0=0.08,
  alpha=0.01
 
)

#lower_bounds=c(0,0)

res = optim(
  initialParameters, 
  least_square_function, 
  method="L-BFGS-B", 
  #lower=lower_bounds, 
  control=c(trace=3))

print(res)



##==========
## ploting the results after model fitting (using estimated params values)
paramters.model1<-par
paramters.model1$beta0=0.03651597
paramters.model1$alpha=0.01602163 
cbind(paramters.model1)

#plot(fs_goubre$week_index, fs_goubre$weekly_incid*1e+05, type="l", col="blue", xlab="Weeks from 2004-2011", ylab="Weekly Incidence per 100,000 pop")
#lines(fs_goubre$week_index, incidence.model1[,6]*1e+05, type="l")
#legend("topright", c("data","fitted model"), lty = c(1, 1), col=c("blue","black"),inset = .02)


###=================
# Model 2 forcing the rate of progression to disease alone
par$amp_a=0.5
par$amp_b=0

### Model fitting and parameters optimisation.

least_square_function <- function(par){
  parameters=par2 # forcing the rate of progression to disease only (Model2)
  for(i in 1:length(names(par))){
    parameters[[names(par)[i]]]=par[[i]]
  }
  
  print(parameters)
  incidence.model2=forced_SCIRS_CI(state=start.SCIRS_CI, parms=parameters, nbyears=7.961538) # same length
  # as available data. nbyears is not an integer value. 2011 is missing from data.
  least_squares = sum((incidence.model2[,6]-(weekly_inc_goubre))^2)/mean(incidence.model2[,6])
  print(least_squares)
  return(least_squares)
}

initialParameters=c(
  beta0=0.08,
  alpha=0.01
)

#lower_bounds=c(0,0)

res = optim(
  initialParameters, 
  least_square_function, 
  method="L-BFGS-B", 
  #lower=lower_bounds, 
  control=c(trace=3))

print(res)

## Plotting results of model fitting


paramters.model2<-par
paramters.model2$beta0=0.09281775 
paramters.model2$alpha=0.01328693 
cbind(paramters.model2)
incidence.model2=forced_SCIRS_CI(state=start.SCIRS_CI, parms=paramters.model2, nbyears=7.961538) # same length

#plot(fs_goubre$week_index, fs_goubre$weekly_incid*1e+05, type="l", col="blue", xlab="Weeks from 2004-2011", ylab="Weekly Incidence per 100,000 pop")
#lines(fs_goubre$week_index, incidence.model2[,6]*1e+05, type="l")
#legend("topright", c("data","fitted model"), lty = c(1, 1), col=c("blue","black"),inset = .02)


#================= Model 3

## definition of least_square function and computation
par$amp_a=0.5
par$amp_b=0.4
par$phi=0.5/365.25

least_square_function <- function(par){
  parameters=PARMS_SICRS_CI_BD_WI_forced_2 # forcing the transmission rate only (Model3)
  for(i in 1:length(names(par))){
    parameters[[names(par)[i]]]=par[[i]]
  }
  
  print(parameters)
  incidence.model1=forced_SCIRS_CI(state=start.SCIRS_CI, parms=parameters, nbyears=7.961538) # same length
  # as available data. nbyears is not an integer value. 2011 is missing from data.
  least_squares = sum((incidence.model1[,6]-(weekly_inc_goubre))^2)/mean(incidence.model1[,6])
  print(least_squares)
  return(least_squares)
}

### Model fitting and parameters optimisation.

initialParameters=c(
  beta0=0.08,
  alpha=0.01
  
)

#lower_bounds=c(0,0)

res = optim(
  initialParameters, 
  least_square_function, 
  method="L-BFGS-B", 
  #lower=lower_bounds, 
  control=c(trace=3))

print(res)


##====================================================
# estimation of R0 from data

install.packages("R0")
library(R0)
toto<-check.incid(hs.data.new1, t = NULL, date.first.obs = NULL, time.step = 7)

est.R0.AR(incid = fs_goubre$cas_meningite, pop.size =4503.688)

# Estimation de R0 à partir des données fité de chaque modèle.

est.R0.ML(incidence, import=newI, t=time, unknown.GT=TRUE, begin=1.000, end=10951.42, range=c(0.01,50))
