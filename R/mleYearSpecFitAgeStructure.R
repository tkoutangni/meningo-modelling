#=====================================================
## algorithm to fit the models on a yearly basis
## using maximum likelihood instead of least-squares.
## curve fitting approach
##====================================================
#=====================================================
## algorithm to fit the models on a yearly basis
## using maximum likelihood instead of least-squares.
## curve fitting approach
##====================================================

## defining objective function for parameter optimization.
Objective_max_likelihood_age_structure <- function(guess_parms, parmset = names(guess_parms), nbYearSimulated=1, obs.data, time.vector, verbose=verbose) {
        current_parms_combination = vparameters
        names(guess_parms) = parmset
        current_parms_combination[parmset] <- guess_parms
        if(verbose) {
                cat("\n Current parameters guesses:\n")
                print(guess_parms)
        }
        
        out <- sim.SCIRS_harmonic_age(inits,current_parms_combination,nd = nbYearSimulated * year)
        # sum new cases in all age groups per week to get the total number of new cases
        out<- sum_incid_cases_and_carriers_colums(data_frame = out)
        #out <- subset(out, select = c(time, newI, Carrier))
        ## make the simulated data.frame same lengh as time.vector in observed data
        out = out[1:length(time.vector),]

        out = data.frame( time = time.vector, incid = out$newI, Carrier = out$Carrier )
        
        # function for maximizing the log likelihood
        
        # take the negative loglikelihood in order to minimize the function toward 0.
        out[which(out$incid<0),c("incid", "Carrier")]<-c(0,0)
        negLogLik = -sum(dpois(
          x = round((obs.data$incid))+1e-12,lambda = ((out$incid)),log = T
        ))
        
        # negLogLik = -sum(dnorm(
        #   x = (obs.data$incid),mean = ((out$incid)), sd= sd(obs.data$incid),log = T
        # ))
        # 
        
        if(is.nan(negLogLik)){
                print(cbind(data= obs.data$incid, model= out$incid))
                #out[which(out$incid<0),c("incid", "Carrier")]<-c(0,0)
                cat("negative log likelihood is: ", negLogLik, "\nPlease check the output of model")
                break
                #negLogLik = 1e+05
        }
        
        
        if(verbose) cat("\n Negative Log-Likelihood = ",negLogLik, "\n")
        return(negLogLik)
} # end objective function.


Objective_least_square_age_structure <- function(guess_parms, parmset = names(guess_parms), nbYearSimulated=1, obs.data, time.vector, verbose=verbose) {
        current_parms_combination = vparameters
        names(guess_parms) = parmset
        current_parms_combination[parmset] <-guess_parms
        if(verbose){
                cat("\n Current parameters guesses:\n")
                print(guess_parms)
        }
        

        out <- sim.SCIRS_harmonic_age(inits,current_parms_combination,nd = nbYearSimulated * year)
        # sum new cases in all age groups per week to get the total number of new cases
        out<- sum_incid_cases_and_carriers_colums(data_frame = out)
        #out <- subset(out, select = c(time, newI, Carrier))
        ## make the simulated data.frame same lengh as time.vector in observed data
        out = out[1:length(time.vector),]
        # Bellow, the modeCost function need as input a data.frame with time vector and model estimation for which comparison against data will be computed. so i remove the Carrier = out$Carrier  vector from out this time since we are fitting to observed cases data not carriers data.
        out = data.frame( time = time.vector, incid = out$newI) 
        
        mc = modCost(
                obs = obs.data, model = out, weight = "std"
        ) # weight="mean", or weight = "std".
        if(verbose) cat("\n Model cost:", mc$model, "\n")
        
        return(mc$model)
} # end objective function.

# inequality constraints function as an additional criteria to the Objective function
ineqConstrainFunction_age_str <- function(guess_parms, parmset = names(guess_parms), nbYearSimulated, obs.data, time.vector, verbose=verbose) {
        #expectedMaxCarriagePrev = 10*1e-02 # 6 percent according to kristiansen et al paper
        current_parms_combination = vparameters
        names(guess_parms) = parmset
        current_parms_combination[parmset] <- guess_parms
        out <- sim.SCIRS_harmonic_age(inits,current_parms_combination,nd = nbYearSimulated * year)
        # sum new cases in all age groups per week to get the total number of new cases
        out<- sum_incid_cases_and_carriers_colums(data_frame = out)
        out = out[1:length(time.vector),]
        carriagePredict = out$Carrier
        annualPeakCarriagePrev = max(carriagePredict)
        #get prediction of carriage at week 8 through 10
        carriageWeek8_10 = carriagePredict[8:10]
        #get prediction of carriage at week 44 through 46
        carriageWeek44_46 = carriagePredict[44:46]
        
        # criteria of constrains that should hold in the solution or best fitted parameters
        ### Criteria 1 : carriage predition of calendar weeks 8 through
        # 10 must be less than 2 fold carriage prediction of week 44 through
        # 46. minimize {mean(carriageWeek8_10)/mean(carriageWeek44_46)-2}
        ineqConstrain =  (mean(carriageWeek44_46) / mean(carriageWeek8_10)) - 2   # mean(carriageWeek8_10)/mean(carriageWeek44_46)-2 <=0
        ### Criteria 2: Annual peak of carriage prediction must be less than or equal 6%.
        
        # setting the inequality constrain to minimize (annualPeakCarriagePrev <= espectedMaxCarriagePrev) or
        #ineqConstrain =  annualPeakCarriagePrev - expectedMaxCarriagePrev   # equivalent to annualPeakCarriagePrev - espectedMaxCarriagePrev <=0
        #print(ineqConstrain)
        return(ineqConstrain)
}


##==== Function takes the following argument with no default value
# district_id (must be numeric) : the unique identification num assign to the district
# district_year_data (must be numeric) : district year specific data including all health centres in the district
# year_now (must be numeric) : current year for which data is bein used for the fitting process. This
# is for graphical display only
# hc_vector (must be numeric vector) : contains the index of health centres with usable data in a given year
# NOTE: The for loop will loop through the index values hold in hc_vector and hc_vector also represent
# row names of the fit_out matrix.

# district_id=1
# district_year_data = seguen_2006
# year_now = 2006
# hc_vector= c(1)
# a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE 
# addCarriageConstrain = FALSE, show_plot = TRUE
# # choose one of the optim algorithm
# algorithm = "L-BFGS-B" 
# n_iter = NULL
mleYearSpecFit_age_str <-function(district_id, district_year_data, year_now,
                          hc_vector, population_size,
                          a0ForcingOnly,# must be a boolean
                          beta0ForcingOnly,# must be a boolean
                          addCarriageConstrain, # must be a boolean
                          algorithm = "L-BFGS-B", # must be of string character type
                          show_plot = FALSE, verbose = FALSE,
                          n_iter = NULL, useMLE = FALSE, useLSQ=F) {

        
        fit_out <- data.frame(matrix(
                NA, ncol = 19,nrow = length(hc_vector),byrow = TRUE,
                dimnames = list(
                        c(names(district_year_data)[hc_vector]),c(
                                "district","hc","year",names(initial_guess_parms),"f_val"
                        )
                )
        ))
        # add goodness of fit estimates when maximum likelihood approach is used instead of
        # the least square fitting approach
        if(!useLSQ){fit_out$AIC<-NA; fit_out$AICc<-NA; fit_out$BIC<-NA}
        
        guess_parms = initial_guess_parms # intial_guess_parms vector is in the parameters file.
        #Constrain bounds for parameters
        lower_bound = guess_lower_bound # guess_lower_bound vector is in the parameters R files
        upper_bound = guess_upper_bound # guess_upper_bound vector is in the parameters R files
        
        ## remove the paramter that is not being estimated from the fit_out matrice
        ## that is created at the beging of the function.
        if (a0ForcingOnly) {
                fit_out = fit_out[,-c(which(dimnames(fit_out)[[2]] == "epsilon_b"))]
                guess_parms = guess_parms[-c(which(names(guess_parms) == "epsilon_b"))]
                lower_bound = lower_bound[-c(which(names(lower_bound) == "epsilon_b"))]
                upper_bound = upper_bound[-c(which(names(upper_bound) == "epsilon_b"))]
        }else if (beta0ForcingOnly) {
                fit_out = fit_out[,-c(which(dimnames(fit_out)[[2]] == "epsilon_a"))]
                guess_parms = guess_parms[-c(which(names(guess_parms) == "epsilon_a"))]
                lower_bound = lower_bound[-c(which(names(lower_bound) == "epsilon_a"))]
                upper_bound = upper_bound[-c(which(names(upper_bound) == "epsilon_a"))]
        }
        
        nbYearSimulated = 1 # Must be at least equal to the number of years of the observed data time serie
        if (addCarriageConstrain) {
                ineqConstrain = ineqConstrainFunction_age_str
        }else{
                ineqConstrain = NULL
        }# end if addCarriageConstrain
        
        # Preparing data for optimisation
        ready_data = district_year_data
        year_now = as.character(year_now) # indicating year of data record as a character
        time.vector = coredata(ready_data$julian_day) # the time vector at with to compare model to data
        population_size<-unlist(population_size)
        age_group_fraction = get_age_grp_proportion("UV", year_now)
        #print(cbind(population_size))
        
        cat("\n Will fit the model to the following health centers: \n ", names(district_year_data)[c(hc_vector)])
        
        for (i in hc_vector) {
                
                cat("\n =====================\n ")
                cat("\n Fitting model to ",names(district_year_data)[i],"data. Please wait...\n")
                hcIndex = i # health center column index in the database
                N = population_size[hcIndex]*age_group_fraction
                names(N)<- c("<5 years","5-12 years","13-19 years", "20+ years")
                # age_group_fraction is the average fraction of the population in each of the
                # nage groups: these fraction are computed from age distribution data from burkina-faso census data publicly available on the website 
                # www.census.gov/population/international/data/idb
                
                cat("\n population size for ", names(district_year_data)[i], ": ", round(sum(N)), "\n")
                cat("\n extrapolated population size for ", names(district_year_data)[i], " in the age groups: ", round(N), "\n")
                #Format the right data
                obs.data = subset(ready_data,select = c(hcIndex,julian_day)) # match the desired health center
                # multiply incidence data in current hc with current hc population size to have case count back (in the cleaned dataset incidences were directly computed as : cases_count/N)
                obs.data = data.frame(time = coredata(obs.data[,2]), incid = coredata(obs.data[,1])*sum(N))
                
                #VERY IMPORTANT NOTICE RELATED TO THE LINE ABOVE!!!! (obs.data = ...)
                # I multiplied obs.data$incid by sum(N) because we're still fitting the model to total case not age
                # specific incidence of cases as we don't have data on age distribution of cases at a local health center level. One might assumes the age distribution of cases at district or country level is the same at the health center level, but we have no evidence for that yet. Also if we were to make that assumption of same age distribution of cases at district and health center level, we have no known source where to get the age distribution for the exact age classes considered in this simulation framework. So for now i decided not to apply just any age distribution of cases to the hc level data until i have appropriate data or information on that matter.
                # OUR CURRENT DATABASE DOES NOT CONTAIN AGE OF THE REPORTED CASES AS THESE CASES ARE REPORTED WEEKLY IN NUMBERS
                
                # ! important for the mle2() function to know the names of the parameters
                # being optimized when the first argument is a vector of all parameters
                parnames(Objective_max_likelihood_age_structure) <- c(names(guess_parms),"nbYearSimulated")
                
                ## OPTIMISATION ALGORITHM
                objfunc = Objective_max_likelihood_age_structure
                if(useLSQ) objfunc = Objective_least_square_age_structure
                
                fit_algo <- function(useMLE = useMLE, objfunc = objfunc,
                                     guess_parms = guess_parms,
                                     lbound = lower_bound,
                                     ubound = upper_bound) {
                        if (useMLE) {
                                # See ?minuslogl for algo options. Most algo defined in the optim package work.
                                # eg. "L-BFGS-B
                                #fixed_parms = vparameters[c("gamma", "mu", "rho", "act_comp_mening_death")]
                                #             print(names(as.list(guess_parms)))
                                #             Fit<-mle( minuslogl = objfunc, start = as.list(guess_parms), fixed = as.list(fixed_parms),
                                #                       method = algorithm, lower = lbound, upper = ubound,
                                #                       trace = TRUE, data = list(nbYearSimulated=nbYearSimulated, obs.data= obs.data, time.vector = time.vector, verbose=FALSE))
                                Fit <- mle2(
                                        minuslogl = objfunc, start = guess_parms, optimizer = "optim",
                                        method = algorithm, lower = lbound, upper = ubound,
                                        trace = TRUE, vecpar = TRUE, data = list(nbYearSimulated = nbYearSimulated, 
                                                                                 obs.data= obs.data, time.vector = time.vector, verbose=verbose))
                        }else{
                                # by default fit with leastsquares and the nloptr algo
                                Fit <- nloptr(
                                        eval_f = objfunc, # the objective function
                                        x0 = guess_parms,
                                        parmset = names(guess_parms),
                                        eval_g_ineq = ineqConstrain, # the inequality constrain function
                                        opts = list(
                                                "algorithm" = algorithm, "print_level" = 0,
                                                "xtol_rel" = 1e-12, "maxeval" = n_iter
                                        ),
                                        lb = lbound,
                                        ub = ubound, 
                                        nbYearSimulated=nbYearSimulated, 
                                        obs.data=obs.data,
                                        time.vector = time.vector,
                                        verbose=verbose
                                )
                                
                        }
                        return(Fit)
                } # end fit_algo function
                
                ## Run the fit algo for the first time
                # if maximum likelihood is beeing used to fit the model, run the first round of simulation with leastsquares, then use
                # the least squares estimates as starting conditions for the maximum likelihood
                firstFit = fit_algo(useMLE = useMLE, objfunc = Objective_least_square_age_structure , guess_parms = guess_parms, lbound = lower_bound, ubound = upper_bound)
                if(useMLE){
                        new_guess_parms = coef(firstFit)
                }else{
                        new_guess_parms = firstFit$solution
                        names(new_guess_parms)=names(guess_parms)
                }
                #cat("\n Negative Log-Likelihood = ",firstFit$objective, "\n")
                #cat(new_guess_parms)
                # Rerun the optimisation algo with fit_par
                # run with the estimated params as initial guess
                
                # n_iter = 40000 # increase nb iteration for maximum likelihood and use parameters estimates of the least squares as starting point for 
                # the likelihood estimation. + use those same least squares parameters estimates as lower bound of the parameter space to search during
                # the likelihood estimation.
                Fit = fit_algo(useMLE = useMLE, objfunc , guess_parms = new_guess_parms, lbound = new_guess_parms, ubound = upper_bound)
                
                # get the estimated values of par
                if(useMLE){
                        fit_par = coef(Fit)
                }else{
                        fit_par = Fit$solution
                        names(fit_par)=names(guess_parms)
                }
                
                # Setting graphical parameters.
                ##
                if(show_plot){
                        ## Simulate model with estimated parameters values and compare the trajectory against data
                        fitted_parms <- vparameters # copying the initial parameters vector
                        fitted_parms[names(fit_par)] <- as.numeric(fit_par) # replacing the unknown parameters values with estimated values
                        
                        fitted_model <- sim.SCIRS_harmonic_age(inits,fitted_parms,nd = nbYearSimulated * year) # run model with those parameters
                        # sum new cases in all age groups per week to get the total number of new cases
                        fitted_model<- sum_incid_cases_and_carriers_colums(data_frame = fitted_model)
                        fitted_model = fitted_model[1:length(time.vector),]
                        
                        #fitted_model <- subset(fitted_model, select = c(time,Susc,Carrier,Ill,Recov,newI))
                        #fitted_model_all_var = tail(fitted_model,length(time.vector))
                        ## the Carrier vector
                        carriageVector = fitted_model$Carrier # variable to plot later
                        # chack if ineq_constrain on carriage worked
                        fold_change_prev_week_6_8_and_44_46 = mean(c(carriageVector[44:46])) / mean(c(carriageVector[8:6]))
                        
                        ## get the newI vector
                        fitted_model_newI = fitted_model$newI # change the name fitted_model at this
                        ## line letter for avoiding confusion because this vector contain only newI predictions and is different
                        ## from the fitted_model matrice returned by sim.SCIRS_harmonic_age.
                        
                        
                        ##################################
                        # ploting the best fitted prediction and observed data
                        na.data.index = which(is.na(obs.data$incid))
                        #obs.data[-c(na.data.index),]
                        
                        
                        if (which(hc_vector == i) == 1) {
                                par(mar = c(5,5,2,4), mfrow = c(2,3)) # plot margings and num of row and col
                        } #end if.
                        # prepare data and calibration results for plot
                        fitted_model_newI= (fitted_model_newI / sum(N)) * per_100000
                        obs.data$incid = (obs.data$incid / sum(N)) * per_100000
                        carriageVector = (carriageVector / sum(N)) * 1e+02
                        
                        plot(obs.data[,2] ,las = 1,pch = 19, col = "black",xlab = "",type = "p",ylim =
                                     c(0,(
                                             1.1 * max(obs.data[,2] ,na.rm = TRUE)
                                     )),ylab = ""
                        ) #ylim=c(0,(1.1*max(obs.data[-c(na.data.index),2]*1e+05)))
                        lines(
                                fitted_model_newI ,col = "red",type = "l",lwd = 2
                        )
                        #lines(carriageVector*100,col="blue",type="l",lwd=2)
                        # adding main title and ylab for the first variable ploted
                        hs_name = names(ready_data)[hcIndex] # selecting corresponding health center name
                        title(
                                main = paste(hs_name),
                                xlab = paste("Calendar weeks",year_now),ylab = "Cases incidence" # Incid x100,000
                        )
                        
                        # adding the plot of model predictions of carriers to the previous plot
                        addCarriagePlot = function() {
                                par(new = T)
                                carriagePrev = carriageVector
                                plot(
                                        carriagePrev,col = "azure4",type = "l",lwd = 2, lty = 2, axes = FALSE, bty = "n", xlab = "", ylab = ""
                                )
                                axis(
                                        side = 4, at = pretty(range(
                                                carriagePrev
                                        )),col.axis = "azure4",las = 1
                                )
                                mtext(
                                        "Carriers(%)", side = 4, line = 2,cex = 0.7, col = "azure4"
                                )
                                
                        } # end addCarriagePlot function
                        ## IMPORTANT !! CALLING THE addCarriagePlot function
                        addCarriagePlot() # call the function
                        
                        # add legend
                        
                        legend(
                                "topright", inset = 0.1, seg.len = 5, x.intersp= 2,  y.intersp= 1.5,
                                legend = c("Data","Model","C"),
                                lty = c(0,1,2), pch = c(19, NA, NA),
                                lwd = c(NA,2,2),
                                col = c("black","red","azure4"),
                                bty = "n",bg = "transparent",horiz = F, merge = TRUE
                        ) #horiz = TRUE for horizontal legend
                        
                        
                        range_a = range(at(
                                fit_par["a0"] * year,fit_par["epsilon_a"],fit_par["teta"]
                        ))
                        range_b = range(betat(
                                fit_par["beta0"] * year,fit_par["epsilon_b"],fit_par["teta"]
                        )) # multiply beta0*year
                        # because the betat function take beta0 argument in per year unit not per day unit.
                        ## see beta0.harmonic function used inside betat function for details. Same explanation goes
                        ## for why i used a0*year.
                        
                        #add fold increase in invasion and or transmission to plot
                        addFoldIncrease = function() {
                                a_fold = round(range_a[2] / range_a[1],1)
                                beta_fold = round(range_b[2] / range_b[1],1)
                                title(
                                        sub = bquote(
                                                paste(
                                                        a[0],'-fold=',.(a_fold),"; ",beta[0],'-fold=',.(beta_fold)
                                                )
                                        ),col.sub = "black",cex.sub = 1.1
                                )
                        }
                        
                        addFoldIncrease() # calling the function
                } # end if plot
                
                district_id = district_id
                
                fit_data = append(c(
                        district_id,hcIndex,as.numeric(year_now),ifelse(useMLE,as.numeric(logLik(Fit)),Fit$objective)
                ),fit_par,after = 3)
                
                
                cat("\n Carriage Prev week_6_8 / Carriage Prev week_44_46: ", fold_change_prev_week_6_8_and_44_46,"\n\n ")
                
                if(useMLE){
                        print (summary(Fit))
                        cat("\n AIC = ", AIC(Fit))
                }else{
                        
                        cat("\n Model fitted using the nloptr package for parameters optimisations \n:")
                        print(Fit$call)
                        cat("\n Completed", Fit$iterations, "iterations out of ", n_iter, " \n")
                        cat("\n Message :", Fit$message, " Fit status is ", "(",Fit$status,")\n")
                        cat("\n ====================== \n ")
                        
                        if(!useLSQ){
                                cat("\n Details of the fit: \n")
                                cat("\n Loglikelihood : ", -(Fit$objective), "\n")
                                minusloglik = Fit$objective
                                k = length(Fit$solution) # number of estimated parameters
                                nobs = length(obs.data$incid) # number of observations in the data set
                                AIC = (2*minusloglik) + (2*k) # Akaike's Information Criterion
                                AIC_corrected = AIC + (2*k*(k+1)/(nobs-k-1)) # corrected Akaike's Information Criterion.
                                BIC = (2*minusloglik) + k*log(nobs) # Bayesian Information Criterion
                                cat("\n AICc = ", AIC_corrected, "and BIC = ", BIC )
                                fit_data<-c(fit_data,AIC, AIC_corrected, BIC)
                                #fit_out$AIC<- AIC ; fit_out$AICc<-AIC_corrected ; fit_out$BIC<-BIC
                                
                        } # end not useSQL if block
                } #end else block
                
                fit_out[names(district_year_data)[i],] = fit_data
        } # for loop ends here
        
        return(fit_out)
}# mleyearSpecFit fitting maximum likelyhood function ends here.


