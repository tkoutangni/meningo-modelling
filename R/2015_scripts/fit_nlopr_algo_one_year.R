library(FME)
library(nloptr)
source("loadFunctions.R")
source("GeneralDataLoading.R")
source("data.cleaning.R")

source("data_simple_mooving_week_average.R")

##==== Function takes the following argument with no default value
# district_id (must be numeric) : the unique identification num assign to the district 
# district_year_data (must be numeric) : district year specifique data including all health centres in the district
# year_now (must be numeric) : current year for which data is bein used for the fitting process. This
# is for graphical display only
# hc_vector (must be numeric vector) : contains the index of health centres with usable data in a given year
# NOTE: The for loop will loop through the index values hold in hc_vector and hc_vector also represent
# row names of the fit_out matrix.

yearSpecFit<-function(district_id, district_year_data, year_now, hc_vector){
    hc_vector = hc_vector
    fit_out<-matrix(NA,ncol=13,nrow=length(hc_vector),byrow=TRUE,
                    dimnames = list(c(hc_vector),c("district","hs","year","beta0","alpha","phi","Susc0","CarrierProp","teta","epsilon_a","epsilon_b","a0","f_val")))

    for (i in c(hc_vector)){ # for loop start here
        cat("\n Fitting model to ",names(district_year_data)[i],"data. Please wait...\n")
        
        nbYearSimulated=1 # Must be at least equal to the number of years of the observed data time serie
        
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
            predict = tail(out$newI,length(time.vector))
            out = data.frame(time = time.vector, incid = predict)
            #matplot(cbind(obs.data$incid*1e+05,out$incid*1e+05), type=c("p","l"),pch=20,lwd=3,las=1,col=c("black","blue"),ylab="incid x100,000")
            #print(length(obs.data$incid))
            #print(length(out$incid))
            ## Compute and return model cost (sum of squared residuals)
            mc=modCost(obs = obs.data, model = out) #, weight="mean"
            #print(mc$model)
            return(mc$model)
        }
        
        # Preparing data for optimisation
        ready_data = district_year_data #=======
        year_now = as.character(year_now) # indicating year of data record as a character
        hcIndex = i # health center column index in the database
        time.vector = c(coredata(ready_data$julian_day)) # the time vector at with to compare model to data
        obs.data = subset(ready_data,select=c(hcIndex,julian_day)) # make sure to change first indice to match the desired health center
        obs.data = data.frame(time = coredata(obs.data[,2]), incid = coredata(obs.data[,1]))
        
        guess_parms=c(
            beta0 = 100/year,
            alpha = 50/year,
            phi = 4/year, 
            Susc0=0.77, 
            CarrierProp=0.13, 
            teta=97, 
            epsilon_a=0,
            epsilon_b=0,
            a0=0.012/year
        )
        
        
        #Constrain bounds for parameters
        
        lower_bound = c(beta0 = 50/year, alpha=1/year, phi=0.2/year, Susc0=0.1, CarrierProp=0, teta=91, epsilon_a=0,epsilon_b=0,a0=0.012/year) # ,epsilon_b=0, a0=0.012/year
        upper_bound = c(beta0 =200/year, alpha=52/year, phi=52/year, Susc0=1, CarrierProp=0.30, teta=112, epsilon_a=1,epsilon_b=1,a0=1.2/year)  #,epsilon_b=1, a0=1.2/year 
        
        
        #Optimisation using modFit or nloptr
        fit_algo <- function(useFME = FALSE, guess_parms = guess_parms, lbound = lower_bound, ubound = upper_bound ){
            useFME = useFME
            if(useFME){
                Fit <- modFit(p = guess_parms, f = Objective, method="L-BFGS-B", 
                              control=list(maxit=1000,trace=6,factr=1e-14), # 
                              lower = lbound,
                              upper = ubound
                )    
            }else{
                Fit <- nloptr(eval_f= Objective, x0 = guess_parms, parmset=names(guess_parms),
                              opts=list("algorithm"="NLOPT_LN_BOBYQA", "print_level"=0, # may also try with NLOPT_LN_NELDERMEAD
                                        "xtol_rel"=1e-8, "maxeval"= 10000),
                              lb = lbound,
                              ub = ubound
                )
                
            }
            return(Fit)
        }
        
        Fit = fit_algo(useFME = FALSE, guess_parms = guess_parms, lbound = lower_bound, ubound = upper_bound)
        #[-which(names(lower_bound)=="epsilon_a")]
        #[-which(names(upper_bound)=="epsilon_a")]
        
        
        # print the estimated values of par  
        Fit$par = Fit$solution
        names(Fit$par) = names(guess_parms)
        
        #Fit$solution
        ## Simulate model with estimated parameters values and compare the trajectory against data
        
        fitted_parms <-PARMS_SCIRS_f # copying the initial parameters vector
        fitted_parms[names(Fit$par)]<-Fit$par # replacing the unknown parameters values with estimated values
        
        fitted_model <- sim.SCIRS_harmonic(inits,fitted_parms,nd=nbYearSimulated*year) # run model with those parameters
        fitted_model <- subset(fitted_model, select = c(time,Susc,Carrier,Ill,Recov,newI))
        fitted_model_all_var = tail(fitted_model,length(time.vector))
        fitted_model = tail(fitted_model$newI,length(time.vector))
        
        
        ##################################
        # ploting
        na.data.index = which(is.na(obs.data$incid))
        #obs.data[-c(na.data.index),]
        plot(obs.data[,2]*1e+05,las=1,pch=20, col="black",xlab="",type="p",ylim=c(0,(1.1*max(obs.data[-c(na.data.index),2]*1e+05))),ylab="")
        lines(fitted_model*1e+05,col="blue",type="l",pch=20,lwd=2)
        
        hs_name = names(ready_data)[hcIndex] # selcting health center name
        title(main=paste(hs_name),
              xlab=paste("Calendar weeks",year_now),ylab="Incid x100,000")
        ## Adding estimates to the plot.
        
        printDetail = FALSE
        if(printDetail){
        # Param estimates  graph
        max_value_data = max(obs.data[-c(na.data.index),2]*1e+05)
        minus_x = seq(1.5,round(max_value_data),1.5)
        text(x=45,y= max_value_data , bquote(bold("Estim Param (day-1)")),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[1] , bquote("Transmision"==.(Fit$par["beta0"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[2] , bquote("Carriage lost"==.(Fit$par["alpha"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[3] , bquote("Imunity lost"==.(Fit$par["phi"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[4],  bquote("Peak timing"==.(Fit$par["teta"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[5],  bquote("Forcing ampl_a"==.(Fit$par["epsilon_a"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[6],  bquote("Forcing ampl_b"==.(Fit$par["epsilon_b"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[7] , bquote(S[0]==.(Fit$par["Susc0"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[8], bquote(C[0]==.(Fit$par["CarrierProp"])),cex=.7,adj=1)
        text(x=45,y= max_value_data- minus_x[9] , bquote("Invasion rate"==.(Fit$par["a0"])),cex=.7,adj=1)
        
        
        incid_range = round(range(fitted_model_all_var["newI"])*1e+05,3)
        text(x=25,y= max_value_data , bquote(bold("Incid (x10^5)")),cex=.7,adj=1)
        text(x=25,y= max_value_data-minus_x[1] , bquote(Min ==.(incid_range[1])),cex=.7,adj=1)
        text(x=25,y= max_value_data-minus_x[2], bquote(Max ==.(incid_range[2])),cex=.7,adj=1)
        
        carr_range = round(range(fitted_model_all_var["Carrier"])*100,3)
        text(x=25,y= max_value_data-minus_x[3] , bquote(bold("Carriage (%)")),cex=.7,adj=1)
        text(x=25,y= max_value_data-minus_x[4] , bquote(Min ==.(carr_range[1])),cex=.7,adj=1)
        text(x=25,y= max_value_data-minus_x[5], bquote(Max ==.(carr_range[2])),cex=.7,adj=1)
        }
        
        range(at(Fit$par["a0"]*year,Fit$par["epsilon_a"],Fit$par["teta"]))
        range(betat(Fit$par["beta0"]*year,Fit$par["epsilon_b"],Fit$par["teta"]))
        
        district_id = district_id
        fit_data = append(c(district_id,hcIndex,as.numeric(year_now),Fit$objective),Fit$solution,after=3)
        fit_out[as.character(hcIndex),]=fit_data
        #head(fit_out,hcIndex) 
        cat("\n Completed", Fit$iterations, "iterations out of 10,000 \n")
        cat("\n Message :", Fit$message, " Fit status is ", "(",Fit$status,")\n")
        
    } # for loop ends here

    return(fit_out)
}# yearSpecFit fitting function ends here.

#==================================


