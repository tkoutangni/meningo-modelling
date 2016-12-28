insert_age_structure = TRUE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")
#dev.off() # clear all graphical devices and plots in the plotting area.
graphSettings() # seeting plotting parameters

# a function that take as argument a simulation result from sim.SCIRS_harmonic_age()
# and return a data_frame with row sums of Carriers and Susceptibles respectively as well as the time vector

sum_incid_cases_and_carriers_colums<-function(data_frame){
        if (c("newI1")%in%colnames(data_frame)){
                new_data_frame<- data.frame(
                        "time" = data_frame[,"time"], 
                        "newI" = rowSums(data_frame[,grep("newI", colnames(data_frame))]),
                        "Carrier" = rowSums(data_frame[,grep("Carrier", colnames(data_frame))])
                        )
        }else{
                stop("The data.frame doesn't seem to come from an age structured simulation")
        }
        return (new_data_frame)
        
}

