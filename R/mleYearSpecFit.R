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
Objective_max_likelihood <- function(guess_parms, parmset = names(guess_parms), nbYearSimulated=1, obs.data, time.vector, verbose=FALSE) {
  current_parms_combination = vparameters
  names(guess_parms) = parmset
  current_parms_combination[parmset] <- guess_parms
  if(verbose) {
    cat("\n Current parameters guesses:\n")
    print(guess_parms)
    #print(parmset)
  }

  out <- sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated * year)
  out <- subset(out, select = c(time, newI, Carrier))
  out = out[1:length(time.vector),]
  # make the prediction back to count number instead of decimal number by standardizing the incidence
  # per 100,000 inhabitant for use by poissoon likelihood comupation.
  #out$newI = floor(out$newI*per_100000)
  
  predict = out$newI
  out = data.frame( time = time.vector, incid = out$newI, Carrier = out$Carrier )  #predict^2
  #print(cbind("model"= log(out$incid), "data" = log(obs.data$incid)))
  #print(cbind("model" = round(out$incid,2),"data" = round(obs.data$incid,2),"carrier" = round(out$Carrier,2)))
  
  # function for maximizi?ng the log likelihood

    # take the negative loglikelihood in order to minimize the function toward 0.
  
    negLogLik = -sum(dpois(
      x = round((obs.data$incid)),lambda = ((out$incid)+1e-12),log = T
    ))
    
    if(is.nan(negLogLik)){
            negLogLik = 1e+05
    }
    
    #   poissonLoglik = sum(dnorm(
    #     x = round(as.numeric(out$incid)), mean = (as.numeric(obs.data$incid)+1e-12), 
    #     sd=(as.numeric(obs.data$incid)+1e-12), log = T
    #   ))
  
  
  if(verbose) cat("\n Negative Log-Likelihood = ",negLogLik, "\n")
  return(negLogLik)
} # end objective function.


Objective_least_square <- function(guess_parms, parmset = names(guess_parms), nbYearSimulated=1, obs.data, time.vector, verbose=FALSE) {
    current_parms_combination = vparameters
    names(guess_parms) = parmset
    current_parms_combination[parmset] <-guess_parms
    if(verbose){
      cat("\n Current parameters guesses:\n")
      #print(guess_parms)
    }
    
    out <- sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated *year)
    out <-subset(out, select = c(time,newI))
    #predict = tail(out$newI,length(time.vector))
    out = out[1:length(time.vector),]
    out = data.frame(time = time.vector, incid = out$newI)
    
    mc = modCost(
      obs = obs.data, model = out, weight = "std"
    ) # weight="mean", or weight = "std".
    if(verbose) cat("\n Model cost:", mc$model, "\n")
    
    return(mc$model)
  } # end objective function.

# inequality constraints function as an additional criteria to the Objective function
ineqConstrainFunction <- function(guess_parms, parmset = names(guess_parms), nbYearSimulated, obs.data, time.vector, verbose=FALSE) {
  #expectedMaxCarriagePrev = 10*1e-02 # 6 percent according to kristiansen et al paper
  current_parms_combination = vparameters
  names(guess_parms) = parmset
  current_parms_combination[parmset] <- guess_parms
  out <- sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated * year)
  out <- subset(out, select = c(time,newI,Carrier))
  carriagePredict = tail(out$Carrier,length(time.vector))
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
mleYearSpecFit <-function(district_id, district_year_data, year_now,
           hc_vector,
           a0ForcingOnly,# must be a boolean
           beta0ForcingOnly,# must be a boolean
           addCarriageConstrain, # must be a boolean
           algorithm = "L-BFGS-B", # must be of string character type
           show_plot = FALSE, verbose = FALSE,
           n_iter = NULL, useMLE = FALSE, useLSQ=F) {
    # number of max iter # default to 1000
   
    fit_out <- data.frame(matrix(
      NA,ncol = 13,nrow = length(hc_vector),byrow = TRUE,
      dimnames = list(
        c(names(district_year_data)[hc_vector]),c(
          "district","hc","year","beta0",
          "alpha","phi","Susc0","CarrierProp",
          "teta","epsilon_a","epsilon_b","a0","f_val"
        )
      )
    ))
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
      ineqConstrain = ineqConstrainFunction
    }else{
      ineqConstrain = NULL
    }# end if addCarriageConstrain
    
    # Preparing data for optimisation
    ready_data = district_year_data
    year_now = as.character(year_now) # indicating year of data record as a character
    time.vector = coredata(ready_data$julian_day) # the time vector at with to compare model to data
    #print(head(ready_data))
    cat("\n Will fit the model to the following health centers: \n ", names(district_year_data)[c(hc_vector)] )
   
        
    for (i in hc_vector) {
      #if(i==1) cat("\n Will fit the model to the following health centers: ", names(district_year_data)[c(hc_vector)] )
      # for loop start here
      cat("\n =====================\n ")
      cat("\n Fitting model to ",names(district_year_data)[i],"data. Please wait...\n")
      hcIndex = i # health center column index in the database
      
      #Format the right data
      obs.data = subset(ready_data,select = c(hcIndex,julian_day)) # make sure to change first indice to match the desired health center
      obs.data = data.frame(time = coredata(obs.data[,2]), incid = coredata(obs.data[,1]))
      obs.data$incid <- obs.data$incid * N[i]# multiply back with current hc population size to have case count
      #instead of incidence of cases (i.e. cases_count/N) ?
      
      
      # ! important for the mle2() function to know the names of the parameters
      # being optimized when the first argument is a vector of all parameters
        parnames(Objective_max_likelihood) <- c(names(guess_parms),"nbYearSimulated")
      
      ## OPTIMISATION ALGORITHM
      objfunc = Objective_max_likelihood
      if(useLSQ) objfunc = Objective_least_square
      
      fit_algo <- function(useMLE = useMLE, 
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
              trace = TRUE, vecpar = TRUE, data = list(nbYearSimulated=nbYearSimulated, 
                                                       obs.data= obs.data, time.vector = time.vector, verbose=FALSE))
          }else{
            # by default fit with leastsquares and the nloptr algo
            Fit <- nloptr(
                eval_f = objfunc, # the objective function
                x0 = guess_parms,
                parmset = names(guess_parms),
                eval_g_ineq = ineqConstrain, # the inequality constrain function
                opts = list(
                  "algorithm" = algorithm, "print_level" = 0,
                  "xtol_rel" = 1e-08, "maxeval" = n_iter
                ),
                lb = lbound,
                ub = ubound, 
                nbYearSimulated=nbYearSimulated, 
                obs.data=obs.data,
                time.vector = time.vector,
                verbose=FALSE
              )
            
          }
          return(Fit)
        } # end fit_algo function
      
      ## Run the fit algo for the first time
      #time.vector = time.vector
      firstFit = fit_algo(useMLE = useMLE, guess_parms = guess_parms, lbound = lower_bound, ubound = upper_bound)
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
      Fit = fit_algo(useMLE = useMLE, guess_parms = new_guess_parms, lbound = lower_bound, ubound = upper_bound)
      
      #Fit=firstFit
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
        
        fitted_model <- sim.SCIRS_harmonic(inits,fitted_parms,nd = nbYearSimulated * year) # run model with those parameters
        fitted_model <- subset(fitted_model, select = c(time,Susc,Carrier,Ill,Recov,newI))
        fitted_model_all_var = tail(fitted_model,length(time.vector))
        ## the Carrier vector
        carriageVector = fitted_model_all_var$Carrier # variable to plot later
        # chack if ineq_constrain on carriage worked
        fold_change_prev_week_6_8_and_44_46 = mean(c(carriageVector[44:46])) / mean(c(carriageVector[8:6]))
        
        ## get the newI vector
        fitted_model = tail(fitted_model$newI,length(time.vector)) # change the name fitted_model at this
        ## line letter for avoiding confusion because this vector contain only newI predictions and is different
        ## from the fitted_model matrice returned by sim.SCIRS_harmonic.
        
        
        ##################################
        # ploting the best fitted prediction and observed data
        na.data.index = which(is.na(obs.data$incid))
        #obs.data[-c(na.data.index),]
        
        
        if (which(hc_vector == i) == 1) {
          par(mar = c(5,5,2,4), mfrow = c(2,3)) # plot margings and num of row and col
        } #end if.
        # prepare data and calibration results for plot
        fitted_model = (fitted_model / N[i]) * per_100000
        obs.data$incid = (obs.data$incid / N[i]) *
          per_100000
        carriageVector = (carriageVector / N[i]) *
          1e+02
        
        plot(obs.data[,2] ,las = 1,pch = 19, col = "black",xlab = "",type = "p",ylim =
            c(0,(
              1.1 * max(obs.data[,2] ,na.rm = TRUE)
            )),ylab = ""
        ) #ylim=c(0,(1.1*max(obs.data[-c(na.data.index),2]*1e+05)))
        lines(
          fitted_model ,col = "red",type = "l",lwd = 2
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
            ),col.sub = "blue",cex.sub = 1.1
          )
        }
        
        addFoldIncrease() # calling the function
      } # end if plot
      
      district_id = district_id
      fit_data = append(c(
        district_id,hcIndex,as.numeric(year_now),ifelse(useMLE,as.numeric(logLik(Fit)),Fit$objective)
      ),fit_par,after = 3)
      
      fit_out[names(district_year_data)[i],] = fit_data
      
      #cat("\n Carriage Prev week_6_8 / Carriage Prev week_44_46: ", fold_change_prev_week_6_8_and_44_46,"\n\n ")
      
      if(useMLE){
             print (summary(Fit))
              cat("\n AIC = ", AIC(Fit))
      }else{
        
        cat("\n Model fitted using the nloptr package for parameters optimisations \n:")
        print(Fit$call)
        cat("\n Completed", Fit$iterations, "iterations out of ", n_iter, " \n")
        cat("\n Message :", Fit$message, " Fit status is ", "(",Fit$status,")\n")
        cat("\n ====================== \n ")
        cat("\n Details of the fit: \n")
        cat("\n Loglikelihood : ", -(Fit$objective), "\n")
        minusloglik = Fit$objective
        k = length(Fit$solution) # number of estimated parameters
        nobs = length(obs.data$incid) # number of observations in the data set
        AIC = (2*minusloglik) + (2*k) # Akaike's Information Criterion
        AIC_corrected = AIC + (2*k*(k+1)/(nobs-k-1)) # corrected Akaike's Information Criterion.
        BIC = (2*minusloglik) + k*log(nobs) # Bayesian Information Criterion
        
        cat("\n AICc = ", AIC_corrected, "and BIC = ", BIC )
      }
      
    } # for loop ends here
    
    return(fit_out)
  }# mleyearSpecFit fitting maximum likelyhood function ends here.


