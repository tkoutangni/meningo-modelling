library(bbmle)
#library(FME)
#library(nloptr)
source("loadFunctions.R")
source("GeneralDataLoading.R")
source("data.cleaning.R")

source("data_simple_mooving_week_average.R")

##===================================================================================================
#loading an exemple of one year incidence data that will be used for testing purpose of the alorithm
# this data is to be delated from working directory when the algo run run correctly
##===================================================================================================
load("~/Projets/MeningoAfrica/meningoafrica-code/data/one_year_exemple_data.RData")
obs.data1 = one_year_exemple_data

a0Forcing=TRUE; beta0Forcing=TRUE  # decide which of the transmission or invasion rate to force.
## IMPORTANT TO COMMENT OR UNCOMMENT THE APPROPRIATE FORCING FUNCTION IN CHANGE THE FORCING
nbYearSimulated=1 # Must be at least equal to the number of years of the observed data time serie

# defining the objective function to minimize
Objective <- function(guess_parms) {
    
    current_parms_combination = PARMS_SCIRS_f
    parmset = names(guess_parms)
    #names(guess_parms) = parmset
    current_parms_combination[parmset] <- guess_parms
    #print(guess_parms*year)
    
    out <- sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated*year)
    out <- subset(out, select = c(time,newI))
    predict = tail(out$newI,length(time.vector))
    out = data.frame(time = time.vector, incid = predict)
    out1 = out[-c(1,52),]
    matplot(cbind(obs.data1*1e+05,round(out1$incid*1e+05)), type=c("p","l"),pch=20,lwd=3,las=1,col=c("black","blue"),ylab="incid x100,000")
    #matplot(cbind(obs.data$incid*1e+05,out$incid*1e+05), type=c("p","l"),pch=20,lwd=3,las=1,col=c("black","blue"),ylab="incid x100,000")
    #print(length(obs.data$incid))
    #print(length(out$incid))
    ## Compute and return model cost (sum of squared residuals)
    #mc=modCost(obs = obs.data, model = out) #, weight="mean"
    #print(mc$model)
    #return(mc$model)
    logDensities <- dpois(x=round(obs.data1)*1e+05, lambda=round(out1$incid)*1e+05 , log=TRUE)
    par(mfrow=c(1,1))
    negLogLik =  -sum(logDensities)
    
    if(negLogLik==Inf){negLogLik<-99999}
    cat("\n Neg Log-likelihood is : ",negLogLik)
    #print(round(out1$incid*1e+05))
    return(negLogLik)
}

parnames(Objective)<-names(PARMS_SCIRS_f)  # ! important for the mle2() function to know the names of the parameters
# used in the computation of the Objective function.

# Preparing data for optimisation
ready_data = ready_data   # district_year_data
#year_now = as.character(year_now) # indicating year of data record as a character
hcIndex = 1 # health center column index in the database
time.vector = c(coredata(ready_data$julian_day)) # the time vector at with to compare model to data
obs.data = subset(ready_data,select=c(hcIndex,julian_day)) # make sure to change first indice to match the desired health center
obs.data = data.frame(time = coredata(obs.data[,2]), incid = coredata(obs.data[,1]))
#obs.data1 = obs.data[-c(which(is.na(obs.data[,2]))),2]

#guess_parms=list(
    #beta0 = 100/year,
    #alpha = 50/year,
    #phi = 4/year, 
    #Susc0=0.77, 
    #CarrierProp=0.13, 
    #teta=97, 
    #epsilon_a=0,
    #epsilon_b=0,
    #a0=0.012/year
#)

guess_parms=list(
beta0 = seguenNonEpiForc_beta0anda0_2010[6,"beta0"],
alpha = seguenNonEpiForc_beta0anda0_2010[6,"alpha"],
phi = seguenNonEpiForc_beta0anda0_2010[6,"phi"], 
Susc0=seguenNonEpiForc_beta0anda0_2010[6,"Susc0"], 
CarrierProp=seguenNonEpiForc_beta0anda0_2010[6,"CarrierProp"], 
teta=seguenNonEpiForc_beta0anda0_2010[6,"teta"], 
epsilon_a=seguenNonEpiForc_beta0anda0_2010[6,"epsilon_0"],
epsilon_b=seguenNonEpiForc_beta0anda0_2010[6,"epsilon_b"],
a0=seguenNonEpiForc_beta0anda0_2010[6,"a0"]
)

if(a0Forcing==TRUE & beta0Forcing==FALSE){
    guess_parms = guess_parms[-c(which(names(guess_parms)=="epsilon_b"))]
}else if(beta0Forcing==TRUE & a0Forcing==FALSE){
    guess_parms = guess_parms[-c(which(names(guess_parms)=="epsilon_a"))]
}else if(beta0Forcing==TRUE & a0Forcing==TRUE){guess_parms = guess_parms}


#bounds Constrain for parameters to estimate

lower_bound = c(beta0 = 0/year, alpha=1/year, phi=0.2/year, Susc0=0.1, CarrierProp=0, teta=91, epsilon_a=0,epsilon_b=0,a0=0.012/year) # ,epsilon_b=0, a0=0.012/year
upper_bound = c(beta0 =200/year, alpha=52/year, phi=52/year, Susc0=1, CarrierProp=0.30, teta=112, epsilon_a=1,epsilon_b=1,a0=1.2/year)  #,epsilon_b=1, a0=1.2/year 

if(a0Forcing==TRUE & beta0Forcing==FALSE){
    lower_bound = lower_bound[-c(which(names(lower_bound)=="epsilon_b"))]
    upper_bound = upper_bound[-c(which(names(upper_bound)=="epsilon_b"))]
    
}else if(beta0Forcing==TRUE & a0Forcing==FALSE){
    lower_bound = lower_bound[-c(which(names(lower_bound)=="epsilon_a"))]
    upper_bound = upper_bound[-c(which(names(upper_bound)=="epsilon_a"))]
}else if(beta0Forcing==TRUE & a0Forcing==TRUE){
    lower_bound = lower_bound
    upper_bound = upper_bound
}

#Optimisation using modFit from FME package or nloptr from the nloptr package or 
# mle2 function from bbmle package

fit_algo <- function(useFME = FALSE, useNloptr=FALSE, guess_parms, lbound, ubound){
    useFME = useFME
    useNloptr = useNloptr
    if(useFME){
        Fit <- modFit(p = guess_parms, f = Objective, method="L-BFGS-B", 
                      control=list(maxit=1000,trace=6,factr=1e-14), # 
                      lower = lbound,
                      upper = ubound
        )    
    }else if(useNloptr){
        Fit <- nloptr(eval_f= Objective, x0 = guess_parms, parmset=names(guess_parms),
                      opts=list("algorithm"="NLOPT_LN_BOBYQA", "print_level"=0, # I choose to use either NLOPT_LN_BOBYQA or NLOPT_LN_NELDERMEAD; use nloptr.print.options() to see full list of implemented algo
                                "xtol_rel"=1e-8, "maxeval"= 10000),
                      lb = lbound,
                      ub = ubound
        )
        
    }else{
        Fit<-mle2(minuslogl = Objective, start = guess_parms, 
                  skip.hessian = FALSE, trace=TRUE,
                  method="L-BFGS-B", # L-BFGS-B
                  #upper= ubound,
                  lower = lbound,
                  vecpar = TRUE,
                  control=list(maxit=10000)) #,parscale=abs(unlist(start)) ,  reltol=1e-4)
    }
    return(Fit)
}


Fit = fit_algo(useFME = FALSE, useNloptr=FALSE, guess_parms = guess_parms, lbound = lower_bound, ubound = upper_bound)
summary(Fit)
cat("\n The exemple data used to test this algo has been loaded in the workspace \n with the name \"one_year_exemple_data.RData\" ")