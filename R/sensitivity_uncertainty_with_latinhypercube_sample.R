# sensitivity and uncertainty analysis using Latinhypercube sampling of
# parameters values
library(FME)
installPackage("epiR")
library(epiR)
#names(a0ForcEstimates_matrice)[4:11]
#names(beta0ForcEstimates_matrice)[4:11]
#names(a0_beta0_ForcEstimates_matrice)[4:12]

parRangeFunction = function(param_estimates_matrice){
    #Function to create the min and max range of parameters space to sample
    parRanges<- data.frame(
        
        cbind(
            min = c(
                gamma = as.numeric(vparameters["gamma"]),
                "beta0" = as.numeric(summary(param_estimates_matrice$beta0)["1st Qu."]),
                "a0" = as.numeric(summary(param_estimates_matrice$a0)["1st Qu."]), 
                "alpha" = as.numeric(summary(param_estimates_matrice$alpha)["1st Qu."]), 
                #"epsilon_a" = min(param_estimates_matrice["epsilon_a"]), 
                #"epsilon_b" = min(param_estimates_matrice["epsilon_b"]), 
                mu = as.numeric(vparameters["mu"]), 
                "teta"  = as.numeric(summary(param_estimates_matrice$teta)["1st Qu."]), 
                rho = as.numeric(vparameters["rho"]),
                "phi" = as.numeric(summary(param_estimates_matrice$phi)["1st Qu."]), 
                act_comp_mening_death = as.numeric(vparameters["act_comp_mening_death"]), 
                "Susc0" = as.numeric(summary(param_estimates_matrice$Susc0)["1st Qu."]), 
                "CarrierProp" = as.numeric(summary(param_estimates_matrice$CarrierProp)["1st Qu."])
            ), # min ends
            max = c(
                gamma = as.numeric(vparameters["gamma"]),
                "beta0" = as.numeric(summary(param_estimates_matrice$beta0)["3rd Qu."]),
                "a0" = as.numeric(summary(param_estimates_matrice$a0)["3rd Qu."]), 
                "alpha" = as.numeric(summary(param_estimates_matrice$alpha)["3rd Qu."]), 
                #"epsilon_a" = max(param_estimates_matrice["epsilon_a"]), 
                #"epsilon_b" = max(param_estimates_matrice["epsilon_b"]), 
                mu = as.numeric(vparameters["mu"]), 
                "teta"  = as.numeric(summary(param_estimates_matrice$teta)["3rd Qu."]), 
                rho = as.numeric(vparameters["rho"]),
                "phi" = as.numeric(summary(param_estimates_matrice$phi)["3rd Qu."]), 
                act_comp_mening_death = as.numeric(vparameters["act_comp_mening_death"]), 
                "Susc0" = as.numeric(summary(param_estimates_matrice$Susc0)["3rd Qu."]), 
                "CarrierProp" = as.numeric(summary(param_estimates_matrice$CarrierProp)["3rd Qu."])
            ) # max end
        ) #cbind ends
    ) #data.frame ends
    return(parRanges)
} #end function

#rownames(parRangesForce_a0andBeta0)<- c(names(vparameters))
#parRangesForce_a0andBeta0

parRangesForce_a0andBeta0 = parRangeFunction(a0_beta0_ForcEstimates_matrice)
parRangesForce_a0andBeta0 <-rbind(parRangesForce_a0andBeta0,
                                  epsilon_a = c(min= as.numeric(summary(a0_beta0_ForcEstimates_matrice$epsilon_a)["1st Qu."]),
                                                max = as.numeric(summary(a0_beta0_ForcEstimates_matrice$epsilon_a)["3rd Qu."])),
                                  epsilon_b = c(min= as.numeric(summary(a0_beta0_ForcEstimates_matrice$epsilon_b)["1st Qu."]),
                                                max = as.numeric(summary(a0_beta0_ForcEstimates_matrice$epsilon_b)["3rd Qu."]))
) # rbinds ends

parRangesForce_a0 = parRangeFunction(a0ForcEstimates_matrice)
parRangesForce_a0 <-rbind(parRangesForce_a0,
                          epsilon_a = c(min= as.numeric(summary(a0ForcEstimates_matrice$epsilon_a)["1st Qu."]),
                                        max = as.numeric(summary(a0ForcEstimates_matrice$epsilon_a)["3rd Qu."])),
                          epsilon_b = c(min= 0, max = 0)
) # rbinds ends


parRangesForce_beta0 = parRangeFunction(beta0ForcEstimates_matrice)
parRangesForce_beta0 <-rbind(parRangesForce_beta0,
                             epsilon_b = c(min= as.numeric(summary(beta0ForcEstimates_matrice$epsilon_b)["1st Qu."]),
                                           max = as.numeric(summary(beta0ForcEstimates_matrice$epsilon_b)["3rd Qu."])),
                             epsilon_a = c(min= 0, max = 0)
) # rbinds ends


# Adding the baseline value column. baseline value for fixed parms 
# or middle value of the min max range defined for uncertain parms
parRangesForce_a0$baseline_value<-(parRangesForce_a0$min+parRangesForce_a0$max)/2
parRangesForce_beta0$baseline_value<-(parRangesForce_beta0$min+parRangesForce_beta0$max)/2
parRangesForce_a0andBeta0$baseline_value<-(parRangesForce_a0andBeta0$min+parRangesForce_a0andBeta0$max)/2

#=================================================================
# generate latin hypercube samples for each models.
#=================================================================
sampleSize = 1000

# generate latin hypercube sample for model M1
latinhyper_sample_parms_a0force = Latinhyper(parRangesForce_a0[,c("min","max")], sampleSize)

# generate latin hypercube sample for model M1
latinhyper_sample_parms_beta0force = Latinhyper(parRangesForce_beta0[,c("min","max")], sampleSize)

# generate latin hypercube sample for model M1
latinhyper_sample_parms_a0andbeta0force = Latinhyper(parRangesForce_a0andBeta0[,c("min","max")], sampleSize)

##=====
# compute the tree models predictions of the sensitivity variables
# using the generated latin hypercube sample of the parameters.
 #compute model M3 uncertainty and sensitivity of analysis

SCIRS_harmonicSim <-function(pars,nyears=1){
    out = sim.SCIRS_harmonic(inits,vparameters=pars,nd=nyears*year)
    out<-subset(out,select=c(time,Susc,Carrier,Ill,newI,Recov,f_SC))
    #colnames(out)[4]<-"newCases"
    out = out[-1,]
    return(out)
}
##=================================================================
# exemple simulation with the first row of the LHS parms combination 
# table.
is_a0Constant = FALSE
input_par = latinhyper_sample_parms_a0andbeta0force[1,]
toto = SCIRS_harmonicSim(input_par)
plot(toto$newI*1e+05,type="l")
##================================================================



##=================================================================
## explore monotonicity assumption between input parameters values
## and model outcome variables.
#==================================================================
monotonicity_test = function(parRanges_matrice,latinhyper_sample_parms_matrice){
    # The "monotonicity_test_out" list will hold the results for monotonicity test for all
    # sensitivity parameters. each elements of the list will be
    #simmulation results for each sensitivity parameter and will itself
    #be a list of length N (N is the number of random LHS sample for the
    # given sensitivity parameter)
    monotonicity_test_out = vector("list", dim(latinhyper_sample_parms_matrice)[2])
    # get the parameters range matrice rownames 
    parRanges_matrice_rownames = rownames(parRanges_matrice)
    # create a parameter vector with the baseline values
    baseline_params_values = setNames(parRanges_matrice$baseline_value,parRanges_matrice_rownames)
    # Loop through the column of the latinhyper_sample.
    for(i in colnames(latinhyper_sample_parms_matrice)){
        # a list to store the simulation results of monotonicity for
        # each sensitivity parameters
        sim_out = vector("list", dim(latinhyper_sample_parms_matrice)[1])
        column_name = i;
        # get the current column values into a vector
        values_column_name = latinhyper_sample_parms_matrice[,column_name]
        # now loop through the values in the vector
        for(j in seq_along(values_column_name)){
            #copie the vector of baseline params values
            baseline_params_values_copie = baseline_params_values
            #replace the value of the ith column_name by the jth column_name value
            #in the baseline_params_values_copie vector
            baseline_params_values_copie[column_name] = values_column_name[j]
            # now run the model with the current set of baseline values + sample
            # jth value of the current sentivity parameter
            sim_out[[j]] = SCIRS_harmonicSim(baseline_params_values_copie)
        } #end for loop with j index
        # store the result of the current ith parameter in the monotonicity_test_out list
        monotonicity_test_out[[i]] = sim_out
        } #end for loop with i values
    #monotonicity_test_out<-setNames(monotonicity_test_out,c(colnames(latinhyper_sample_parms_matrice)))
    return(monotonicity_test_out)
} # end monotonicity_test function

## ===============================================================
# selecting the sensitivity parameters for which we want to test
# monotonicity assumption from the lating hyper matrice.
## ===============================================================
select_sensitivity_params = function(latinhyper_sample_parms_matrice){
    sensitivity_params =  subset(latinhyper_sample_parms_matrice, 
                            select= c("beta0","a0","alpha","phi",
                                      "teta","Susc0","CarrierProp",
                                      "epsilon_a","epsilon_b"))
    # remove the epsilon column depending on the models
    
    if(sum(sensitivity_params[,"epsilon_a"])==0){
        sensitivity_params = sensitivity_params[,-which(colnames(sensitivity_params) %in% c("epsilon_a"))]
    }else if(sum(sensitivity_params[,"epsilon_b"])==0){
        sensitivity_params = sensitivity_params[,-which(colnames(sensitivity_params) %in% c("epsilon_b"))] 
    }
    return(sensitivity_params)
} #end select_sensitivity_params function

##===================================================================
# testing monotonicity between uncertain parameters and selected model veriables.
is_a0Constant = FALSE
monotonicity_test_model3 = monotonicity_test(
    parRanges_matrice = parRangesForce_a0andBeta0,
    latinhyper_sample_parms_matrice = select_sensitivity_params(latinhyper_sample_parms_a0andbeta0force))

is_a0Constant = TRUE
monotonicity_test_model2 = monotonicity_test(
    parRanges_matrice = parRangesForce_beta0,
    latinhyper_sample_parms_matrice = select_sensitivity_params(latinhyper_sample_parms_beta0force))

is_a0Constant = FALSE
monotonicity_test_model1 = monotonicity_test(
    parRanges_matrice = parRangesForce_a0,
    latinhyper_sample_parms_matrice = select_sensitivity_params(latinhyper_sample_parms_a0force))

##===================================================================
# function to compute summary values of sensitivity variable of interest
# from each parameters combination model simulation outputs. 
##===================================================================

sensitivity_output_var_summary_value = function(model_sensitivity_list){
    # take an object of type 'list' as argument.
    # the list contain the output of model simulation as a matrix
    # each element of the list must be a simulation output matrix
    # each simulation output is the result of a set of LHS sampled
    # params combination (in case of uncertainty analysis)
    # or a combination of baseline parameters values and a single
    # LHS parameter sampled value (in the case of monotonicity test)
    model_sensitivity_cumul_incidence = vector(length=length(model_sensitivity_list))
    model_sensitivity_mean_annual_carriage = vector(length=length(model_sensitivity_list))
    for(i in 1:length(model_sensitivity_list)){
        model_sensitivity_cumul_incidence[i] = sum(model_sensitivity_list[[i]]$newI)
        model_sensitivity_mean_annual_carriage[i] = mean(model_sensitivity_list[[i]]$Carrier)
    }
    return(list(incid_cumul_annuel = model_sensitivity_cumul_incidence,
                prev_moyenne_annuel = model_sensitivity_mean_annual_carriage))
}# end 'sensitivity_output_var_summary_value' function


summary_monotonicity_test = function(latinhyper_sample_parms_matrice, monotonicity_test_output){
    colnames_LHS_parameters = colnames(
    select_sensitivity_params(latinhyper_sample_parms_matrice))
    monotonicity_test_summary_list = list()
    for(i in seq_along(colnames_LHS_parameters)){
        param_name = colnames_LHS_parameters[i]
        cat("\n calculating summary of output var for ",param_name," monotonicity test")
        monotonicity_test_summary_list[[i]] = sensitivity_output_var_summary_value(
        model_sensitivity_list = monotonicity_test_output[[param_name]])
    }
    monotonicity_test_summary_list = setNames(monotonicity_test_summary_list,
                                              c(colnames_LHS_parameters))
    return(monotonicity_test_summary_list)
}# end summary_monotonicity_test function   

monotonicity_test_summary_list_model3 = summary_monotonicity_test(
    latinhyper_sample_parms_a0andbeta0force,
    monotonicity_test_model3)

monotonicity_test_summary_list_model2 = summary_monotonicity_test(
    latinhyper_sample_parms_beta0force,
    monotonicity_test_model2)

monotonicity_test_summary_list_model1 = summary_monotonicity_test(
    latinhyper_sample_parms_a0force,
    monotonicity_test_model1)

generate_monotonicity_data_frame = function(latinhyper_sample_parms_matrice,
                                            monotonicity_test_summary_list,param_name){
    # param_name_argument must match a culumn name in LHS_matrice
    param_value = latinhyper_sample_parms_matrice[,param_name]
    param_monotonicity_table = data.frame(
        param_values = param_value,
        cumul_incid = monotonicity_test_summary_list[[param_name]][["incid_cumul_annuel"]],
        mean_annual_prev = monotonicity_test_summary_list[[param_name]][["prev_moyenne_annuel"]])
        #colnames(param_monotonicity_table)[1] = param_name
        param_monotonicity_table$param<-param_name
    
    return(param_monotonicity_table)
} # end generate_monotonicity_data_frame function

create_final_monotonicity_data_frame = function(latinhyper_sample_parms_matrice,
         monotonicity_test_summary_list){
    monotonicity_data_frame_beta0 = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"beta0")
    monotonicity_data_frame_a0 = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"a0")
    monotonicity_data_frame_alpha = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"alpha")
    monotonicity_data_frame_phi = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"phi")
    monotonicity_data_frame_teta = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"teta")
    monotonicity_data_frame_Susc0 = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"Susc0")
    monotonicity_data_frame_CarrierProp = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"CarrierProp")
    
    if(sum(latinhyper_sample_parms_matrice[,"epsilon_a"])==0){
        monotonicity_data_frame_epsilon_b = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"epsilon_b")
    }else if(sum(latinhyper_sample_parms_matrice[,"epsilon_b"])==0){
        monotonicity_data_frame_epsilon_a = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"epsilon_a")
    }else{
        monotonicity_data_frame_epsilon_a = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"epsilon_a")
        monotonicity_data_frame_epsilon_b = generate_monotonicity_data_frame(latinhyper_sample_parms_matrice,monotonicity_test_summary_list,"epsilon_b")
    }
    
    final_data = rbind(monotonicity_data_frame_beta0,
                 monotonicity_data_frame_a0,
                 monotonicity_data_frame_alpha,
                 monotonicity_data_frame_phi,
                 monotonicity_data_frame_teta,
                 monotonicity_data_frame_Susc0,
                 monotonicity_data_frame_CarrierProp
                 #monotonicity_data_frame_epsilon_a,
                 #monotonicity_data_frame_epsilon_b
                 )
    
    if(sum(latinhyper_sample_parms_matrice[,"epsilon_a"])==0){
        final_data = rbind(final_data,monotonicity_data_frame_epsilon_b)
    }else if(sum(latinhyper_sample_parms_matrice[,"epsilon_b"])==0){
        final_data = rbind(final_data, monotonicity_data_frame_epsilon_a)
    }else{
        final_data = rbind(final_data, monotonicity_data_frame_epsilon_a)
        final_data = rbind(final_data,monotonicity_data_frame_epsilon_b)
    }
    
    # dynamically defining parameters labels
    par_labels<-c(rep("Mean transmission/day",sampleSize),
              rep("Mean invasion/day",sampleSize),
              rep("Carriage loss/day",sampleSize),
              rep("Immunity lost/day",sampleSize),
              rep("Pick time (days)",sampleSize),
              rep("Initial susceptibles",sampleSize),
              rep("Initial carriers",sampleSize),
              rep("Invasion forcing ampl.",sampleSize),
              rep("Transmission forcing ampl.",sampleSize))
    
    if(sum(latinhyper_sample_parms_matrice[,"epsilon_a"])==0){
        final_data$param_label<-par_labels[-c(701:800)]
    }else if(sum(latinhyper_sample_parms_matrice[,"epsilon_b"])==0){
        final_data$param_label<-par_labels[-c(801:900)]
    }else{
        final_data$param_label<-par_labels
    }
    
    return(final_data)
    
}

monotonicity_test_data_model3 = create_final_monotonicity_data_frame(latinhyper_sample_parms_a0andbeta0force,monotonicity_test_summary_list_model3)
monotonicity_test_data_model2 = create_final_monotonicity_data_frame(latinhyper_sample_parms_beta0force,monotonicity_test_summary_list_model2)
monotonicity_test_data_model1 = create_final_monotonicity_data_frame(latinhyper_sample_parms_a0force,monotonicity_test_summary_list_model1)

require(ggplot2)
# Check monotonicity with annual cumulative incidences
monotonicity_plot_cumul_incid = function(monotonicity_test_data){
plot1<-ggplot(data = monotonicity_test_data, aes(x=param_values, y=cumul_incid*1e+05)) + 
    geom_point() + xlab("")
    plot1 <- plot1 + facet_wrap( ~ param_label, scales="free",nrow = 3)
    plot1 <- plot1 + xlab("Uncertain Parameters") + ylab("Annual Cumul. Incid.\n x 1e-05") + 
    ggtitle("Monotonicity between uncertain parameters\n and Annual cumulative incidence")
    return(plot1)
}
# Check monotonicity with annual average prevalence
monotonicity_plot_average_prevalence = function(monotonicity_test_data){
    plot2<-ggplot(data = monotonicity_test_data, aes(x=param_values, y=mean_annual_prev*1e+02)) + 
    geom_point() + xlab("")
    plot2 <- plot2 + facet_wrap( ~ param_label, scales="free",nrow = 3)
    plot2 <- plot2 + xlab("Uncertain Parameters") + ylab("Average annual prev.\n x 1e-02") + 
    ggtitle("Monotonicity between uncertain parameters\n and mean annual carriage prevalence")
    return(plot2)
}

monotonicity_plot_cumul_incid(monotonicity_test_data_model1)
monotonicity_plot_cumul_incid(monotonicity_test_data_model2)
monotonicity_plot_cumul_incid(monotonicity_test_data_model3)

# monotonicity check with regard to annual average carriage 
monotonicity_plot_average_prevalence(monotonicity_test_data_model1)
monotonicity_plot_average_prevalence(monotonicity_test_data_model2)
monotonicity_plot_average_prevalence(monotonicity_test_data_model3)


##==============================================================================
## Global Uncertainty analysis start here
##==============================================================================
##==============================================================================
# uncertainty analysis based on all sampled sensitivity parameters combination
##==============================================================================

compute_model_variables_sensitivity = function(hypercube_parms_matrice){
    sim_out= vector("list", dim(hypercube_parms_matrice)[1])
    for(i in 1:dim(hypercube_parms_matrice)[1]){
        sim_out[[i]]= SCIRS_harmonicSim(hypercube_parms_matrice[i,])
    }
    return(sim_out)
}

model_1_sensitivity = compute_model_variables_sensitivity(latinhyper_sample_parms_a0force) 
model_2_sensitivity = compute_model_variables_sensitivity(latinhyper_sample_parms_beta0force)
model_3_sensitivity = compute_model_variables_sensitivity(latinhyper_sample_parms_a0andbeta0force)


model_1_sensitivity_out = sensitivity_output_var_summary_value(model_1_sensitivity)
model_2_sensitivity_out = sensitivity_output_var_summary_value(model_2_sensitivity)
model_3_sensitivity_out = sensitivity_output_var_summary_value(model_3_sensitivity)


dat1 <- as.data.frame(cbind(latinhyper_sample_parms_a0force,cumul_incid = model_1_sensitivity_out$incid_cumul_annuel))
dat1<-cbind(dat1,average_annual_carriage = model_1_sensitivity_out$prev_moyenne_annuel)                  

dat2 <- as.data.frame(cbind(latinhyper_sample_parms_beta0force,cumul_incid = model_2_sensitivity_out$incid_cumul_annuel))
dat2<-cbind(dat2,average_annual_carriage = model_2_sensitivity_out$prev_moyenne_annuel)                  

dat3 <- as.data.frame(cbind(latinhyper_sample_parms_a0andbeta0force,cumul_incid = model_3_sensitivity_out$incid_cumul_annuel))
dat3<-cbind(dat3,average_annual_carriage = model_3_sensitivity_out$prev_moyenne_annuel)                  

# subsetting the data.frame to compte PRCC for uncertain parameters

prcc_cumul_incid_model3 = cbind(select_sensitivity_params(latinhyper_sample_parms_a0andbeta0force),
                         cumul_incid = dat3$cumul_incid,
                         average_annual_carriage = dat3$average_annual_carriage)

prcc_cumul_incid_model2 = cbind(select_sensitivity_params(latinhyper_sample_parms_beta0force),
                                cumul_incid = dat2$cumul_incid,
                                average_annual_carriage = dat2$average_annual_carriage)

prcc_cumul_incid_model1 = cbind(select_sensitivity_params(latinhyper_sample_parms_a0force),
                                cumul_incid = dat1$cumul_incid,
                                average_annual_carriage = dat1$average_annual_carriage)

prcc_cumul_incid_model1 = as.data.frame(prcc_cumul_incid_model1)
prcc_cumul_incid_model2 = as.data.frame(prcc_cumul_incid_model2)
prcc_cumul_incid_model3 = as.data.frame(prcc_cumul_incid_model3)



library("sensitivity")
sensitivity_model3_prcc = pcc(prcc_cumul_incid_model3[,c(1:9)], 
                              prcc_cumul_incid_model3$cumul_incid ,
                              rank=TRUE, nboot = sampleSize)

sensitivity_model2_prcc = pcc(prcc_cumul_incid_model2[,c(1:8)], 
                              prcc_cumul_incid_model2$cumul_incid ,
                              rank=TRUE, nboot = sampleSize)

sensitivity_model1_prcc = pcc(prcc_cumul_incid_model1[,c(1:8)], 
                              prcc_cumul_incid_model1$cumul_incid ,
                              rank=TRUE, nboot = sampleSize)

plot(sensitivity_model3_prcc, main="Model3")
title(xlab = "Model3 parameters values")
plot(sensitivity_model2_prcc,main="Model2")
title(xlab = "Model2 parameters values")
plot(sensitivity_model1_prcc, main="Model1")
title(xlab = "Model1 parameters values")

##================================================================
# compute descriptive statistics from uncertainty analysis result
##================================================================

descript_summary = function(model_sensitivity_out_var,outcome_var_label="" ){
    # takes an argument. 
    # - model sensitivity var summary values
descript_summary = round(summary(model_sensitivity_out_var),3)
percentile = round(quantile(model_sensitivity_out_var,c(.05, .50, .95)),3)
variance = round(var(model_sensitivity_out_var),3)
percentile = data.frame('percentile' = percentile)
cat('descriptives stats for:',outcome_var_label,'\n')
print(descript_summary)
cat('Variance for:', outcome_var_label,'\n')
print(variance)
cat('Percentiles for:', outcome_var_label,'\n')
print(percentile)

} # ends descript_summary function.

## computation of summaries.
descript_summary(model_1_sensitivity_out$incid_cumul_annuel*1e+05,
                 'Model1 Annual Cumul. incid.')
descript_summary(model_1_sensitivity_out$prev_moyenne_annuel*1e+02,
                 'Model1 Annual Averag. Carriage Prev.')

# Model 2

descript_summary(model_2_sensitivity_out$incid_cumul_annuel*1e+05,
                 'Model2 Annual Cumul. incid.')
descript_summary(model_2_sensitivity_out$prev_moyenne_annuel*1e+02,
                 'Model2 Annual Averag. Carriage Prev.')
# Model 3

descript_summary(model_3_sensitivity_out$incid_cumul_annuel*1e+05,
                 'Model3 Annual Cumul. incid.')
descript_summary(model_3_sensitivity_out$prev_moyenne_annuel*1e+02,
                 'Model3 Annual Averag. Carriage Prev.')


#library(epiR)
#epi.prcc(dat1, sided.test = 2) # two sided test.
## computing the PRCC 
# PRCC with cumulative annual incidences
compute_prcc<-function(prcc_data_frame, sensitivity_var_colname,sampleSize){
    #take one argument which is the name of the sensitivity var column
    # argement is a string/ character matching the column name
    # containing values of the sensitivity outcome var
    cat("\n PRCC for: ", sensitivity_var_colname, "\n")
    if(dim(prcc_data_frame)[2]==10){
    model_prcc = pcc(prcc_data_frame[,c(1:8)],prcc_data_frame[,sensitivity_var_colname],
        rank = TRUE, nboot = sampleSize)
    }else if(dim(prcc_data_frame)[2]==11){
    model_prcc = pcc(prcc_data_frame[,c(1:9)],prcc_data_frame[,sensitivity_var_colname],
                     rank = TRUE,nboot = sampleSize)
    }else{
        cat("Error, the data frame need to be inspected expected 10 or 11 columns.\n")
    }
    print(model_prcc$PRCC)
    return(model_prcc$PRCC)
} # end function cimpute_prcc


# PRCC with cumulative annual incidences. provide 95% CI as well

model1_prcc = compute_prcc(prcc_cumul_incid_model1,"cumul_incid",sampleSize)
model2_prcc = compute_prcc(prcc_cumul_incid_model2,"cumul_incid",sampleSize)
model3_prcc = compute_prcc(prcc_cumul_incid_model3,"cumul_incid",sampleSize)

# PRCC with cumulative annual incidences. provide two sided p-value
# which evaluate whether or not the partial rank correlation coefficient 
# is greater than or less than zero at a significance level of 5%.

# for model 1
epi.prcc(prcc_cumul_incid_model1[,c(1:9)],sided.test = 2)
epi.prcc(prcc_cumul_incid_model2[,c(1:9)],sided.test = 2)
epi.prcc(prcc_cumul_incid_model3[,c(1:10)],sided.test = 2)

