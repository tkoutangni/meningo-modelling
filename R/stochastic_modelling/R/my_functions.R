# prepare data in a format that pomp object can use

prepare_data_for_pomp<-function(hs_data_and_population_list, start_time, end_time){
        subset(hs_data_and_population_list$demog_data,
               time>=start_time & time<end_time,
               select=c(date, semaine, time, population)) -> ready_demog_data
        # round the size of the population to an integer
        ready_demog_data$population<-round(ready_demog_data$population)
        
        subset(hs_data_and_population_list$ready_data,
               time>=start_time & time<end_time,
               select=c(date, semaine, time, meningitis_cases)) -> ready_data
        
        
        head(ready_data)
        head(ready_demog_data)
        
        return(list(ready_data = ready_data, 
                    ready_demog_data = ready_demog_data))
}




get_unknown_theta_guess<-function(params){
        # a function to get the values of desired columns in the previous params estimate matrice
        c(
                # estimates from previous work
                params["beta0"]*year,
                params["a0"]*year,
                #params["alpha"]*year,
                #params["teta"]/year,
                params["epsilon_a"],
                params["epsilon_b"],
                params["phi"]*year
        )
}

# My own functions for stochastic model simulation 
# including those functions for testing purpose only
# 
# Function to time code execussion
time_taken<-function(code_to_execute){
        start.time <- Sys.time()
        result<- code_to_execute
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        cat("\n This code took ", time.taken,  "second(s) to execute. \n")
}

#-------------------------------------------------------
print_fit_result<- function(pomp_fitted_object, to_estimate_pars){
        # a function to print the fit result.
        cat("\n Log likelihood: ", logLik(pomp_fitted_object))
        
        cat('\n estimated params values: \n')
        round(coef(pomp_fitted_object),6)[to_estimate_pars]
}

#------------------------------------
get_estimates_at_desired_time<-function(data_frame, time_step){
  # a function that will apply the approx function to all the variables 
  # returned by ssa.adaptivetau and construct a data frame with model output at the 
  # desired times.
  
  col_names<-colnames(data_frame) # get the colnames for use  later
  data_columns_names = c(colnames(data_frame[2:dim(data_frame)[2]]))
  
  f<-function(data_frame, datacolumn){
    # function f to compute approximate number of events for a given variable
    # at desired time steps
    # @ takes a data.frame with 1st column being "time", and the other model 
    # state variables event counts
    approx(x = data_frame$time, 
           y= datacolumn,
           n = time_step, # the full range of time is devided by time_step
           # to get the desired time when to output result
           # xout=1:year, # only use xout  when 'n' is not specified 
           method = "constant")
  }
  
  results = lapply(data_frame[,data_columns_names], function(col){
    #print(col)
    col_result <- f(data_frame, col)
    col_result<-data.frame(col_result)
    #names(col_result)<-c("time", as.character(names(col)))
    
  })
  
  results<-data.frame(results)
  # get time vectors generated for each columns
  # take only one of the time vector as all state variable are approximated
  # at same time interval: using c(grep(".x", colnames(results)))[,1] achieve this.
  time_vector<-c(results[,grep(".x", colnames(results))][,1])
  # state variable approximate counts for the chosen time interval
  states_var_approx<-results[,grep(".y",colnames(results))]
  # Using grep(".y",colnames(results)) allow to select the states approximate counts 
  # variable colums
  
  final_data_frame<-cbind(time = time_vector, states_var_approx)
  
  # replace the colnames by the orinal data_frame colnames
  colnames(final_data_frame)<-col_names
  return(final_data_frame)
  
} # end of get_estimates_at_desired_time function that return approximate counts for all state variables at desired time
# intervall
#------------------------------------

# run several simulation of the stochastic model
# necessary package

library(plyr)
multiple_stoch_sim<- function(inits.values,
                              transitions, 
                              rates.func, 
                              params, 
                              tf= year,
                              out_time, 
                              nsim){
  # run multiple simulation of the model and combine them into a data frame
  # @out_time is the time step at which I want to return each state variable count
  # possible values for @out_time can be: daily, weekly, monthly as a character
  if(out_time == 'daily') time_frame = tf # will be used to return output daily
  if(out_time == 'weekly') time_frame = tf/week # will be used to return output weekly
  if(out_time == 'monthly') time_frame = tf/month # will be used to return output monthly
  
    
  ldply(1:nsim, function(.nn){
    set.seed(.nn)
    cat('\n Simulation ', .nn, ' completed.')
    run<-ssa.adaptivetau(inits.values,
                         transitions,
                         rates.func,
                         params,
                         tf= tf )
    
    result<-data.frame(run)
    
    
    # if Incid column is present in the data.frame compute the diff to get 
    # instantaneaous new case counts
    result<-trace_new_cases(result)
    # computing the state variables at the desired time -> out_time specified in arguments
    result<-get_estimates_at_desired_time(result, time_step = time_frame)
    
    result$nsim<-.nn
    # create a new time vector that correspond to desired time step units
    # if desired out_time step was weekly or daily then the time_vec unit will
    # be in the unit of the desired time step. this is to aid xaxis on plotting
    result$time_vec <- seq(1:dim(subset(result, result$nsim==.nn))[1])-1
    
    return(result)
  }) 
  
} # end function


#------------------------------------

# plot the simulations results as a lattice graph

library("lattice")
# lattice plot the results

plot_sims_I<-function(data , xlab = "Times in days", title = ''){
  
  plot(
    xyplot(
      I~time | sprintf("Simulation %02d", nsim), 
      data = data, type = c("l", "g"), as.table = T, col="black",lty=1, #pch=20,
      ylab = 'Infected', xlab = xlab,
      main = title,
      scales = list(y=list(alternating=F))
    )
  )
} # end function

# ----------------------------------

plot_sims_var<-function(df , varname = 'Ill', ylab = '', xlab = "Times in days", title = ''){
  xtick_every_x_week = 4 # weeks
  plot(
    xyplot(
      df[,varname]~time_vec | sprintf("Simulation %02d", nsim), 
      data = df, type = c("b", "g"), as.table = T, col="black",lty=1, #pch=20,
      ylab = ylab , 
      xlab = xlab,
      main = title,
      scales = list(y=list(alternating=F)#,
                    #x=list(at= c(seq(0,year*nb_years,year/52*xtick_every_x_week*2)),
                    #labels = c(seq(0,52*nb_years,xtick_every_x_week*2)))
                    )
    )
  )
} # end function

#------------------------------------

scirs_bd.rate.func<-function(x,params,t){
  # a function to return transitions rates for the SCIRS model with birth and death
  # @x, is a named vector of initial values of model states variables
  # @params, is a named list of model parameters
  # @t, is the time at with the model state variable will be evaluated
  # in this stochastic model simulation @t is initially set to 0, and is calulated
  # dynamically and represent the samallest time intervall for which a total of 1 event/transition
  # occur in the system. Exemple of event/transitions are 'infection'/'recovery'..
  # @t is calculated dynamically for us by the ssa.adaptivetau function 
  
  Susc =        x[1]
  Carrier =     x[2]
  Ill =         x[3]
  Recov =       x[4]
  Incid =       x[5] # variable to track new cases count
  
  if(Susc <0) Susc = 0 #; cat("\n Susc value is negative ")
  if(Carrier <0) Carrier = 0 #; cat("\n Carrier value is negative ")
  if(Ill <0) Ill = 0 #; cat("\n Ill value is negative ")
  if(Recov <0) Recov = 0 #; cat("\n Recov value is negative ")
  
  
  N = Susc + Carrier+ Ill + Recov
  
  with(as.list(params),{
    
    if(is_a0Constant){
      a = a0
    }else{
      #a = a0*(1 + epsilon_a*cos(2*pi*(t-teta)/year))
      a = a0*((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a))
    }
    
    # seasonal function for transmission
    beta = beta0*(1 + epsilon_b*cos(2*pi*(t-teta)/year))
    
    f_SC = beta*Susc*(Carrier + Ill)/N # infection rate
    f_CI = a*Carrier
    f_CR = alpha*Carrier
    f_IR = rho*Ill 
    f_RS = phi*Recov
    f_new_b = mu*N + ( (gamma*Ill)*act_comp_mening_death )
    f_S_death = mu*Susc
    f_R_death = mu*Recov
    f_I_menin_death = gamma*Ill
    f_I_death = mu*Ill
    f_C_death = mu*Carrier
    
    
    return(c(
      f_SC,
      f_CI,
      f_CR,
      f_IR,
      f_RS,
      f_new_b,
      f_S_death,
      f_R_death,
      f_I_menin_death,
      f_I_death,
      f_C_death
    ))
  })
} # end function


#------------------------------------
# a function to calculate new Incident cases from the Incid column of the matrix return by 
# ssa.adaptivetau
trace_new_cases<-function(df){
  # '@df must be a data.frame obtained from the returned value of ssa.adaptivetau
  # the data.frame must contain the variable Incid traking the instantaneous new cases
  if(grep("Incid",colnames(df))){
    # use the diff function to compute the new cases occurring in between delta_t
    df <- cbind(df[-dim(df)[2]], Incid = c(0, apply(df[dim(df)[2]],2, diff)))
  }
  return(df)
}

#------------------------------------

scirs_bd_deterministic<-
        scirs_bd.rate.func<-function(x,params,t){
                Susc =        x[1]
                Carrier =     x[2]
                Ill =         x[3]
                Recov =       x[4]
                Incid =       x[5] # variable to track new cases count
                
                if(Susc <0) Susc = 0 #; cat("\n Susc value is negative ")
                if(Carrier <0) Carrier = 0 #; cat("\n Carrier value is negative ")
                if(Ill <0) Ill = 0 #; cat("\n Ill value is negative ")
                if(Recov <0) Recov = 0 #; cat("\n Recov value is negative ")
                
                
                N = Susc + Carrier+ Ill + Recov
                
                with(as.list(params),{
                        
                        if(is_a0Constant){
                                epsilon_a = 0
                                #a = a0*(1 + epsilon_a*cos(2*pi*(t-teta)/year))
                                a = a0*((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a))
                        }else{
                                #a = a0*(1 + epsilon_a*cos(2*pi*(t-teta)/year))
                                a = a0*((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a))
                        }
                        
                        # seasonal function for transmission
                        beta = beta0*(1 + epsilon_b*cos(2*pi*(t-teta)/year))
                        
                        f_SC = beta*Susc*(Carrier + Ill)/N # infection rate
                        f_CI = a*Carrier
                        f_CR = alpha*Carrier
                        f_IR = rho*Ill 
                        f_RS = phi*Recov
                        f_new_b = mu*N + ( (gamma*Ill)*act_comp_mening_death )
                        f_S_death = mu*Susc
                        f_R_death = mu*Recov
                        f_I_menin_death = gamma*Ill
                        f_I_death = mu*Ill
                        f_C_death = mu*Carrier
                        
                        
                        return(c(
                                f_SC,
                                f_CI,
                                f_CR,
                                f_IR,
                                f_RS,
                                f_new_b,
                                f_S_death,
                                f_R_death,
                                f_I_menin_death,
                                f_I_death,
                                f_C_death
                        ))
                })
        } # end function

#------------------------------------


#------------------------------------

# This function is for testing purpose only
sir.rate.func<-function(x,params,t){
        # a function to return transitions rates for the closed SIR model
        # without birth and death 
        # @x, is a named vector of initial values of model states variables
        # @params, is a named list of model parameters
        # @t, is the time at with the model state variable will be evaluated
        # in this stochastic model simulation @t is initially set to 0, and is calulated
        # dynamically and represent the samallest time intervall for which a total of 1 event/transition
        # occur in the system. Exemple of event/transitions are 'infection'/'recovery'..
        # @t is calculated dynamically for us by the ssa.adaptivetau function 
        
        S = x[1]
        I = x[2]
        R = x[3]
        N = S + I + R
        
        with(as.list(params),{
                f_SI = beta*S*I/N
                f_IR = gamma*I
                
                return(c(
                        f_SI,
                        f_IR
                ))
        })
}


#------------------------------------
# for testing purpose only
sirs_bd.rate.func<-function(x,params,t){
        # a function to return transitions rates for the SIRS model with birth and death
        # without birth and death 
        # @x, is a named vector of initial values of model states variables
        # @params, is a named list of model parameters
        # @t, is the time at with the model state variable will be evaluated
        # in this stochastic model simulation @t is initially set to 0, and is calulated
        # dynamically and represent the samallest time intervall for which a total of 1 event/transition
        # occur in the system. Exemple of event/transitions are 'infection'/'recovery'..
        # @t is calculated dynamically for us by the ssa.adaptivetau function 
        
        S = x[1]
        I = x[2]
        R = x[3]
        Incid = x[4] # to track new cases
        N = S + I + R
        
        with(as.list(params),{
                f_SI = beta*S*I/N
                f_IR = gamma*I
                f_RS = alpha*R
                f_new_b = mu*N
                f_S_death = mu*S
                f_I_death = mu*I
                f_R_death = mu*R 
                
                return(c(
                        f_SI,
                        f_IR,
                        f_RS,
                        f_new_b,
                        f_S_death,
                        f_I_death,
                        f_R_death
                ))
        })
}

#===========================