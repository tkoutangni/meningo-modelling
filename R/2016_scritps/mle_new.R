#setwd("~/Projets/MeningoAfrica/meningoafrica-code/R")
#rm(list=ls())

#source("objectiveFunction.R")

##==== Function takes the following argument with no default value
# district_id (must be numeric) : the unique identification num assign to the district
# district_year_data (must be numeric) : district year specifique data including all health centres in the district
# year_now (must be numeric) : current year for which data is bein used for the fitting process. This
# is for graphical display only
# hc_vector (must be numeric vector) : contains the index of health centres with usable data in a given year
# NOTE: The for loop will loop through the index values hold in hc_vector and hc_vector also represent
# row names of the fit_out matrix.



mleYearSpecFit <-
    function(district_id, district_year_data, year_now,
             hc_vector,a0ForcingOnly,
             beta0ForcingOnly,
             addCarriageConstrain, # must be a boolean
             algorithm = NULL, # must be of string character type
             show_plot = FALSE,
             n_iter = NULL) {
        # number of max iter # default to 1000
        
        hc_vector = hc_vector
        fit_out <- matrix(
            NA,ncol = 13,nrow = length(hc_vector),byrow = TRUE,
            dimnames = list(
                c(names(district_year_data)[hc_vector]),c(
                    "district","hc","year","beta0",
                    "alpha","phi","Susc0","CarrierProp",
                    "teta","epsilon_a","epsilon_b","a0","f_val"
                )
            )
        )
        fit_out = as.data.frame(fit_out) # important to convert the matrix into data frame for
        # ease of further manipulation of it content.
        ## remove the paramter that is not being estimated from the fit_out matrice
        ## that is created at the beging of the function.
        if (a0ForcingOnly) {
            fit_out = fit_out[,-c(which(dimnames(fit_out)[[2]] == "epsilon_b"))]
        }else if (beta0ForcingOnly) {
            fit_out = fit_out[,-c(which(dimnames(fit_out)[[2]] == "epsilon_a"))]
        }
        
        for (i in c(hc_vector)) {
            # for loop start here
            
            cat("\n Fitting model to ",names(district_year_data)[i],"data. Please wait...\n")
            nbYearSimulated = 1 # Must be at least equal to the number of years of the observed data time serie
            
            ## defining objective function for parameter optimization.
            
            #=======
            Objective_max_likelihood <-
                function(guess_parms, parmset = names(guess_parms)) {
                    current_parms_combination = vparameters
                    names(guess_parms) = parmset
                    current_parms_combination[parmset] <- guess_parms
                    print(guess_parms)
                    
                    out <-
                        sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated *
                                               year)
                    out <- subset(out, select = c(time,newI))
                    # make the prediction back to count number instead of decimal number by standardizing the incidence
                    # per 100,000 inhabitant for use by poissoon likelihood comupation.
                    #out$newI = floor(out$newI*per_100000)
                    
                    predict = tail(out$newI,length(time.vector))
                    out = data.frame(time = time.vector, incid = predict)  #predict^2
                    # function for maximizing the log likelihood
                    poissonLoglik = sum(dpois(
                        x = as.numeric(out$newI),lambda = as.numeric(obs.data$incid),log = TRUE
                    ))
                    # take the negative loglikelihood in order to minimize the function toward 0.
                    negLogLik = (-poissonLoglik)
                    
                    cat("\n Negative Log-Likelihood = ",negLogLik)
                    return(negLogLik)
                } # end objective function.
            
            
            
            if (addCarriageConstrain) {
                # inequality constraints function as an additional criteria to the Objective function
                ineqConstrain <-
                    function(guess_parms, parmset = names(guess_parms)) {
                        #expectedMaxCarriagePrev = 10*1e-02 # 6 percent according to kristiansen et al paper
                        current_parms_combination = vparameters
                        names(guess_parms) = parmset
                        current_parms_combination[parmset] <-
                            guess_parms
                        out <-
                            sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated *
                                                   year)
                        out <-
                            subset(out, select = c(time,newI,Carrier))
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
                
            }# end if addCarriageConstrain
            
            
            
            # Preparing data for optimisation
            ready_data = district_year_data #=======
            year_now = as.character(year_now) # indicating year of data record as a character
            hcIndex = i # health center column index in the database
            time.vector = c(coredata(ready_data$julian_day)) # the time vector at with to compare model to data
            head(time.vector)
            obs.data = subset(ready_data,select = c(hcIndex,julian_day)) # make sure to change first indice to match the desired health center
            obs.data = data.frame(time = coredata(obs.data[,2]), incid = coredata(obs.data[,1]))
            obs.data$incid<-obs.data$incid*N # multiply back with current hc population size to have case count
            
            # make the prediction back to count number instead of decimal number by standardizing the incidence
            # per 100,000 inhabitant for use by poissoon likelihood comupation.
            obs.data$incid = obs.data$incid * per_100000
            
            guess_parms = initial_guess_parms # intial_guess_parms vector is in the parameters file.
            #Constrain bounds for parameters
            lower_bound = guess_lower_bound # guess_lower_bound vector is in the parameters R files
            upper_bound = guess_upper_bound # guess_upper_bound vector is in the parameters R files
            
            ## remove the paramter that is not being estimated from the initial guess_parms vector
            ## and the upper and lower bounds vector
            if (a0ForcingOnly) {
                guess_parms = guess_parms[-c(which(names(guess_parms) == "epsilon_b"))]
                lower_bound = lower_bound[-c(which(names(lower_bound) == "epsilon_b"))]
                upper_bound = upper_bound[-c(which(names(upper_bound) == "epsilon_b"))]
            }else if (beta0ForcingOnly) {
                guess_parms = guess_parms[-c(which(names(guess_parms) == "epsilon_a"))]
                lower_bound = lower_bound[-c(which(names(lower_bound) == "epsilon_a"))]
                upper_bound = upper_bound[-c(which(names(upper_bound) == "epsilon_a"))]
            } # end if
            
            
            # ! important for the mle2() function to know the names of the parameters
            # being optimized when the first argument is a vector of all parameters
            
            parnames(Objective_max_likelihood) <- names(guess_parms)
            
            ## OPTIMISATION ALGORITHM
            #Optimisation using modFit or nloptr
            n_iter = n_iter # maximum number of iterations to run
            algorithm = algorithm # algo to use with the nloptr routine
            fit_algo <- function(useFME = FALSE, useMLE = FALSE,
                                 guess_parms = guess_parms,
                                 lbound = lower_bound,
                                 ubound = upper_bound) {
                useFME = useFME
                useMLE = useMLE
                if (useFME) {
                    Fit <- modFit(
                        p = guess_parms, f = Objective_max_likelihood, method = "L-BFGS-B",
                        control = list(
                            maxit = n_iter,trace = 6,factr = 1e-14
                        ), #
                        lower = lbound,
                        upper = ubound
                    )
                }else if (useMLE) {
                    Fit <- mle2(
                        minuslogl = Objective_max_likelihood, start = guess_parms,
                        method = "L-BFGS-B", lower = lbound, upper = ubound,
                        trace = TRUE, vecpar = TRUE
                    )

                }
                else{
                    if (addCarriageConstrain) {
                        ineqConstrain = ineqConstrain
                    }else{
                        ineqConstrain = NULL
                    }
                    # by default fit with leastsquares and the nloptr algo
                    Fit <-
                        nloptr(
                            eval_f = Objective_max_likelihood, # the objective function
                            x0 = guess_parms,
                            parmset = names(guess_parms),
                            eval_g_ineq = ineqConstrain, # the inequality constrain function
                            opts = list(
                                "algorithm" = algorithm, "print_level" = 0,
                                "xtol_rel" = 1e-08, "maxeval" = n_iter
                            ),
                            lb = lbound,
                            ub = ubound
                        )
                    
                }
                return(Fit)
            } # end fit_algo function
            
            ## Run the fit algo for the first time
            Fit0_par = function() {
                Fit = fit_algo(
                    useFME = FALSE, useMLE = TRUE,
                    guess_parms = guess_parms,
                    lbound = lower_bound,
                    ubound = upper_bound
                )
                # get the estimated values of par
                fit_par_0 = coef(Fit)
                return(fit_par_0)
            } # end function;
            
            # Rerun the optimisation algo with fit_par
            
            Fit = fit_algo(
                useFME = FALSE, useMLE = TRUE,
                guess_parms = Fit0_par(),  #Fit0_par(), # run with the estimated params as initial guess
                lbound = lower_bound,
                ubound = upper_bound
            )
            
            
            # get the estimated values of par
            fit_par = coef(Fit)
            
            #Fit$solution
            ## Simulate model with estimated parameters values and compare the trajectory against data
            
            fitted_parms <-
                vparameters # copying the initial parameters vector
            fitted_parms[names(fit_par)] <-
                as.numeric(fit_par) # replacing the unknown parameters values with estimated values
            
            fitted_model <-
                sim.SCIRS_harmonic(inits,fitted_parms,nd = nbYearSimulated * year) # run model with those parameters
            fitted_model <-
                subset(fitted_model, select = c(time,Susc,Carrier,Ill,Recov,newI))
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
            
            # Setting graphical parameters.
            ##
            if (show_plot == TRUE) {
                if (which(hc_vector == i) == 1) {
                    par(mar = c(5,5,2,4), mfrow = c(2,3)) # plot margings and num of row and col
                } #end if.
                
                plot(
                    obs.data[,2],las = 1,pch = 20, col = "black",xlab = "",type = "p",ylim =
                        c(0,(
                            1.1 * max(obs.data[,2],na.rm = TRUE)
                        )),ylab = ""
                ) #ylim=c(0,(1.1*max(obs.data[-c(na.data.index),2])))
                lines(
                    fitted_model ,col = "red",type = "l",pch = 20,lwd = 2
                )
                #lines(carriageVector*100,col="blue",type="l",lwd=2)
                # adding main title and ylab for the first variable ploted
                hs_name = names(ready_data)[hcIndex] # selecting corresponding health center name
                title(
                    main = paste(hs_name),
                    xlab = paste("Calendar weeks",year_now),ylab = "Incid x100,000"
                )
                
                # adding the plot of model predictions of carriers to the previous plot
                addCarriagePlot = function() {
                    par(new = T)
                    carriagePrev = carriageVector
                    plot(
                        carriagePrev,col = "azure4",type = "l",lwd = 2, lty = 2, axes = FALSE, bty = "n", xlab = "", ylab = ""
                    )
                    axis(
                        side = 4, at = pretty(range(carriagePrev)),col.axis = "azure4",las = 1
                    )
                    mtext(
                        "Carriers(count)", side = 4, line = 2,cex = 0.7, col = "azure4"
                    )
                    
                    legend(
                        "topright",
                        legend = c("Data","NewI","C"),
                        lty = c(0,1,2), pch = c(20, NA, NA),
                        lwd = c(NA,2,2),
                        col = c("black","red","azure4"),
                        bty = "n",bg = "transparent",horiz = FALSE, merge = TRUE
                    ) #horiz = TRUE for horizontal legend
                } # end addCarriagePlot function
                ## IMPORTANT !! CALLING THE addCarriagePlot function
                addCarriagePlot() # call the function
                
                ## Adding estimates to the plot.
                
                printDetail = FALSE
                if (printDetail) {
                    # Param estimates  graph
                    max_value_data = max(obs.data[-c(na.data.index),2])
                    minus_x = seq(1.5,round(max_value_data),1.5)
                    text(
                        x = 45,y = max_value_data , bquote(bold("Estim Param (day-1)")),cex = .7,adj =
                            1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[1] , bquote("Transmision" == .(fit_par["beta0"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[2] , bquote("Carriage lost" == .(fit_par["alpha"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[3] , bquote("Imunity lost" == .(fit_par["phi"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[4],  bquote("Peak timing" == .(fit_par["teta"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[5],  bquote("Forcing ampl_a" == .(fit_par["epsilon_a"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[6],  bquote("Forcing ampl_b" == .(fit_par["epsilon_b"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[7] , bquote(S[0] == .(fit_par["Susc0"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[8], bquote(C[0] == .(fit_par["CarrierProp"])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 45,y = max_value_data - minus_x[9] , bquote("Invasion rate" == .(fit_par["a0"])),cex =
                            .7,adj = 1
                    )
                    
                    
                    incid_range = round(range(fitted_model_all_var["newI"]) ,3)
                    text(
                        x = 25,y = max_value_data , bquote(bold("Incid (x10^5)")),cex = .7,adj =
                            1
                    )
                    text(
                        x = 25,y = max_value_data - minus_x[1] , bquote(Min == .(incid_range[1])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 25,y = max_value_data - minus_x[2], bquote(Max == .(incid_range[2])),cex =
                            .7,adj = 1
                    )
                    
                    carr_range = round(range(fitted_model_all_var["Carrier"]) *
                                           100,3)
                    text(
                        x = 25,y = max_value_data - minus_x[3] , bquote(bold("Carriage (%)")),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 25,y = max_value_data - minus_x[4] , bquote(Min == .(carr_range[1])),cex =
                            .7,adj = 1
                    )
                    text(
                        x = 25,y = max_value_data - minus_x[5], bquote(Max == .(carr_range[2])),cex =
                            .7,adj = 1
                    )
                }
                
                range_a = range(at(fit_par["a0"] * year,fit_par["epsilon_a"],fit_par["teta"]))
                range_b = range(betat(fit_par["beta0"] * year,fit_par["epsilon_b"],fit_par["teta"])) # multiply beta0*year
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
                district_id,hcIndex,as.numeric(year_now),as.numeric(logLik(Fit))
            ),fit_par,after = 3)
            
            fit_out[names(district_year_data)[i],] = fit_data
            
            #cat("\n Completed", Fit$iterations, "iterations out of ", n_iter, " \n")
            #cat("\n Message :", Fit$message, " Fit status is ", "(",Fit$status,")\n")
            cat(
                "\n Carriage Prev week_6_8 / Carriage Prev week_44_46: ", fold_change_prev_week_6_8_and_44_46,"\n "
            )
            cat("\n ")
        } # for loop ends here
        
        return(fit_out)
    }# yearSpecFit fitting function ends here.

#=====================================================
