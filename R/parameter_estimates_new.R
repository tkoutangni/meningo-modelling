# the Objective function in "fit_nloptr_algo_one_year_automate.R" discriminate only based
# on model cost (sum of the squared residuals)
#source("fit_nloptr_algo_one_year_automate.R")

# the Objective function in "yearSpecificFittingAlgo_nov_2015.R" add a criteria
# which checks plausibility of carriage prevalence variations. The criteria can be switch off by 
# setting the parameter addCarriageConstrain = FALSE. When addCarriageConstrain = TRUE then change
# algorithm : see http://ab-initio.mit.edu/wiki/index.php/NLopt_Algorithms for algo suporting
# inequality and equality constrains.
setwd("~/Projets/MeningoAfrica/meningoafrica-code/R")
source("yearSpecificFittingAlgo_nov_2015.R")
dev.off() # clear all graphical devices and plots in the plotting area.

#par(cex.lab=1.1) # seeting plotting parameters
#op <- par(fig=c(0,1,0,1),mfrow=c(2,3),
#mar=c(5,4,3,1),mgp=c(2,1,0))

#currentPar= par()
#currentPar$mfrow = c(2,3)
#suppressWarnings(par(currentPar))

doPlot = FALSE
if(doPlot){
    par(mfrow=c(2,3))
    plot(seguen_2007[,c(1:6)]*1e+05,plot.type="m",type="b",ylab="")
    plot(seguen_2007[,c(2,3,5,6)]*1e+05,plot.type="m",type="b",ylab="")
    plot(seguen_2009[,6]*1e+05,plot.type="m",type="b",ylab="",las=1)
    plot(seguen_2010[,c(1:6)]*1e+05,plot.type="m",type="b",ylab="",las=1)
}


###################################################################################
## IMPORTANT ! It is necessary to set 
## is_a0Constant = FALSE or TRUE depending if we want the invasion rate to be constant or not
## By default the value is FALSE. Which mean the invasion rate can vary seasonaly by a factor of
## 10 - fold to 100-fold. When set to TRUE it mean the invasion rate is unforced but its value
## is 10*a0. 
## It value can be change between TRUE of FALSE in any script as long as the variable 
## is_a0Constant scope is the global environment and must be specified before runing the year specific parameters estimates.

#=============================================================
## Parameters estimates when only invasion rate is forced
#=============================================================

# check to see if the parameter estimates matrice exists from a previous run of this script.

# file.exists("../data/processed_data/a0ForcEstimates.RData") ## returns TRUE

if(!file.exists("a0ForcEstimates.RData")){ # here we are checking in the current working directory
    # ...meningoafrica-code/R
    
    # seasonal forcing of invasion rate alone.
    
    # DISTRICT OF SEGUENEGA ( Id = 1)
    
    is_a0Constant = FALSE; 
    seguenNonEpiForc_a0_2006 = yearSpecFit(district_id=1,
                                          district_year_data = seguen_2006, 
                                          year_now = 2006, hc_vector= c(1:4,6),
                                          a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                          addCarriageConstrain = TRUE,
                                          nloptrAlgorithm = "NLOPT_LN_COBYLA",  # NLOPT_LN_BOBYQA
                                          n_iter = 10000)
    
    
    seguenNonEpiForc_a0_2007 = yearSpecFit(district_id=1,
                                           district_year_data = seguen_2007, 
                                           year_now = 2007, hc_vector= c(2,3,5,6),
                                           a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                           addCarriageConstrain = TRUE, 
                                           nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                           n_iter = 10000)
    

    
    seguenNonEpiForc_a0_2009 = yearSpecFit(district_id=1,
                                           district_year_data = seguen_2009, 
                                           year_now = 2009, hc_vector = c(6),
                                           a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                           addCarriageConstrain = TRUE, 
                                           nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                           n_iter = 10000)
        

    
    seguenNonEpiForc_a0_2010 = yearSpecFit(district_id=1,
                                           district_year_data = seguen_2010, 
                                           year_now = 2010, hc_vector= c(1:6),
                                           a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                           addCarriageConstrain = TRUE, 
                                           nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                           n_iter = 10000)
    
    
    
    # DISTRICT OF HOUNDE ( Id = 2)
    
    EstimNonEpiHoundeForc_a0_2004 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2004, 
                                                year_now = 2004, hc_vector= c(4,7,13),
                                                a0ForcingOnly=TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_a0_2006 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2006, 
                                                year_now = 2006, hc_vector= c(1:14),
                                                a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)

    
    
    EstimNonEpiHoundeForc_a0_2007 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2007, 
                                                year_now = 2007, hc_vector= c(2,3,17),  # 15,
                                                a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
        
    
    EstimNonEpiHoundeForc_a0_2008 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2008, 
                                                year_now = 2008, hc_vector= c(1:4,6:11,15:18),
                                                a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
        
    
    EstimNonEpiHoundeForc_a0_2009 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2009, 
                                                year_now = 2009, hc_vector= c(1,15:17),
                                                a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
        
        
    #EstimNonEpiHoundeForc_a0_2010 = yearSpecFit(district_id=2, 
                                                #district_year_data = hounde_2010, 
                                                #year_now = 2010, hc_vector = c(5),
                                                #a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                                #addCarriageConstrain = TRUE, 
                                                #nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                #n_iter = 10000)
    
    EstimNonEpiLenaForc_a0_2006 = yearSpecFit(district_id=3, 
                                              district_year_data = lena_2006, 
                                              year_now = 2006, hc_vector = c(1:4),
                                              a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                              n_iter = 10000)
    
    

    EstimNonEpiLenaForc_a0_2007 = yearSpecFit(district_id=3,
                                              district_year_data = lena_2007, 
                                              year_now = 2007, hc_vector = c(1,3),
                                              a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                              n_iter = 10000)

    
    ## === DISTRICT OF K.VIGUE (id = 4) === ###
    
    EstimNonEpiKvigueForc_a0_2008 = yearSpecFit(district_id=4, 
                                                district_year_data = Kvigue_2008, 
                                                year_now = 2008, hc_vector = c(2,3),
                                                a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    EstimNonEpiKvigueForc_a0_2010 = yearSpecFit(district_id=4, 
                                                district_year_data = Kvigue_2010, 
                                                year_now = 2010, hc_vector = c(3,5),
                                                a0ForcingOnly = TRUE, beta0ForcingOnly = FALSE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    }else{
        cat("\n The file \"a0ForcEstimates.RData\" already exist \n in :",getwd(), 
            "\n Please delete it first in order to reestimate parameters")
    } #end of if -else conditional statement for force_a0 estimates. 
    
save(seguenNonEpiForc_a0_2006,
     seguenNonEpiForc_a0_2007,
     seguenNonEpiForc_a0_2009,
     seguenNonEpiForc_a0_2010,
     EstimNonEpiHoundeForc_a0_2004,
     EstimNonEpiHoundeForc_a0_2006,
     EstimNonEpiHoundeForc_a0_2007,
     EstimNonEpiHoundeForc_a0_2008,
     EstimNonEpiHoundeForc_a0_2009,
     EstimNonEpiLenaForc_a0_2006,
     EstimNonEpiLenaForc_a0_2007,
     EstimNonEpiKvigueForc_a0_2008,
     EstimNonEpiKvigueForc_a0_2010,
     file="../data/processed_data/new_processed_data/a0ForcEstimates.RData")

#=============================================================
## Parameters estimates when only transmission rate is forced.
#=============================================================
    
if(!file.exists("beta0ForcEstimates.RData")){ # here we are checking in the current working directory
    # ...meningoafrica-code/R
    
    # seasonal forcing of invasion rate alone.
    
    # DISTRICT OF SEGUENEGA ( Id = 1)
    
    is_a0Constant = TRUE; 
    seguenNonEpiForc_beta0_2006 = yearSpecFit(district_id=1,
                                           district_year_data = seguen_2006, 
                                           year_now = 2006, hc_vector= c(1:4,6),
                                           a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                           addCarriageConstrain = TRUE, 
                                           nloptrAlgorithm = "NLOPT_LN_COBYLA",  # NLOPT_LN_BOBYQA
                                           n_iter = 10000)
    
    
    seguenNonEpiForc_beta0_2007 = yearSpecFit(district_id=1,
                                           district_year_data = seguen_2007, 
                                           year_now = 2007, hc_vector= c(2,3,5,6),
                                           a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                           addCarriageConstrain = TRUE, 
                                           nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                           n_iter = 10000)
    
    
    
    seguenNonEpiForc_beta0_2009 = yearSpecFit(district_id=1,
                                           district_year_data = seguen_2009, 
                                           year_now = 2009, hc_vector= c(6),
                                           a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                           addCarriageConstrain = TRUE, 
                                           nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                           n_iter = 10000)
    
    
    
    seguenNonEpiForc_beta0_2010 = yearSpecFit(district_id=1,
                                           district_year_data = seguen_2010, 
                                           year_now = 2010, hc_vector= c(1:6),
                                           a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                           addCarriageConstrain = TRUE, 
                                           nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                           n_iter = 10000)
    
    
    
    # DISTRICT OF HOUNDE ( Id = 2)
    
    EstimNonEpiHoundeForc_beta0_2004 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2004, 
                                                year_now = 2004, hc_vector= c(4,7,13),
                                                a0ForcingOnly=FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_beta0_2006 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2006, 
                                                year_now = 2006, hc_vector= c(1:14),
                                                a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    
    
    EstimNonEpiHoundeForc_beta0_2007 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2007, 
                                                year_now = 2007, hc_vector= c(2,3,15,17),
                                                a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_beta0_2008 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2008, 
                                                year_now = 2008, hc_vector= c(1:4,6:11,15:18),
                                                a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_beta0_2009 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2009, 
                                                year_now = 2009, hc_vector= c(1,15:17),
                                                a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_beta0_2010 = yearSpecFit(district_id=2, 
                                                district_year_data = hounde_2010, 
                                                year_now = 2010, hc_vector = c(5),
                                                a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    EstimNonEpiLenaForc_beta0_2006 = yearSpecFit(district_id=3, 
                                              district_year_data = lena_2006, 
                                              year_now = 2006, hc_vector = c(1:4),
                                              a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                              n_iter = 10000)
    
    
    
    EstimNonEpiLenaForc_beta0_2007 = yearSpecFit(district_id=3,
                                              district_year_data = lena_2007, 
                                              year_now = 2007, hc_vector = c(1,3),
                                              a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                              n_iter = 10000)
    
    
    ## === DISTRICT OF K.VIGUE (id = 4) === ###
    
    EstimNonEpiKvigueForc_beta0_2008 = yearSpecFit(district_id=4, 
                                                district_year_data = Kvigue_2008, 
                                                year_now = 2008, hc_vector = c(2,3),
                                                a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
    
    EstimNonEpiKvigueForc_beta0_2010 = yearSpecFit(district_id=4, 
                                                district_year_data = Kvigue_2010, 
                                                year_now = 2010, hc_vector = c(3,5),
                                                a0ForcingOnly = FALSE, beta0ForcingOnly = TRUE, 
                                                addCarriageConstrain = TRUE, 
                                                nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                n_iter = 10000)
}else{
    cat("\n The file \"beta0ForcEstimates.RData\" already exist \n in :",getwd(), 
        "\n Please delete it first in order to reestimate parameters")
} #end of if -else conditional statement for force_beta0 estimates. 


save(seguenNonEpiForc_beta0_2006,
     seguenNonEpiForc_beta0_2007,
     seguenNonEpiForc_beta0_2009,
     seguenNonEpiForc_beta0_2010,
    EstimNonEpiHoundeForc_beta0_2004,
    EstimNonEpiHoundeForc_beta0_2006,
    EstimNonEpiHoundeForc_beta0_2007,
    EstimNonEpiHoundeForc_beta0_2008,
    EstimNonEpiHoundeForc_beta0_2009,
    EstimNonEpiHoundeForc_beta0_2010,
    EstimNonEpiLenaForc_beta0_2006,
    EstimNonEpiLenaForc_beta0_2007,
    EstimNonEpiKvigueForc_beta0_2008,
    EstimNonEpiKvigueForc_beta0_2010,
    file="../data/processed_data/new_processed_data/beta0ForcEstimates.RData")


#=============================================================
## Parameters estimates when both transmission and invasion rate are 
# forced.
#=============================================================

if(!file.exists("a0_beta0_ForcEstimates.RData")){ # here we are checking in the current working directory
    # ...meningoafrica-code/R
    
    # seasonal forcing of invasion rate alone.
    
    # DISTRICT OF SEGUENEGA ( Id = 1)
    
    is_a0Constant = FALSE; 
    seguenNonEpiForc_a0_beta0_2006 = yearSpecFit(district_id=1,
                                              district_year_data = seguen_2006, 
                                              year_now = 2006, hc_vector= c(1:4,6),
                                              a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",  # NLOPT_LN_BOBYQA
                                              n_iter = 10000)
    
    
    seguenNonEpiForc_a0_beta0_2007 = yearSpecFit(district_id=1,
                                              district_year_data = seguen_2007, 
                                              year_now = 2007, hc_vector= c(2,3,5,6),
                                              a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                              n_iter = 10000)
    
    
    
    seguenNonEpiForc_a0_beta0_2009 = yearSpecFit(district_id=1,
                                              district_year_data = seguen_2009, 
                                              year_now = 2009, hc_vector= c(6),
                                              a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                              n_iter = 10000)
    
    
    
    seguenNonEpiForc_a0_beta0_2010 = yearSpecFit(district_id=1,
                                              district_year_data = seguen_2010, 
                                              year_now = 2010, hc_vector= c(1:6),
                                              a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                              addCarriageConstrain = TRUE, 
                                              nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                              n_iter = 10000)
    
    
    
    # DISTRICT OF HOUNDE ( Id = 2)
    
    EstimNonEpiHoundeForc_a0_beta0_2004 = yearSpecFit(district_id=2, 
                                                   district_year_data = hounde_2004, 
                                                   year_now = 2004, hc_vector= c(4,7,13),
                                                   a0ForcingOnly=FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_a0_beta0_2006 = yearSpecFit(district_id=2, 
                                                   district_year_data = hounde_2006, 
                                                   year_now = 2006, hc_vector= c(1:14),
                                                   a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
    
    
    
    EstimNonEpiHoundeForc_a0_beta0_2007 = yearSpecFit(district_id=2, 
                                                   district_year_data = hounde_2007, 
                                                   year_now = 2007, hc_vector= c(2,3,15,17),
                                                   a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_a0_beta0_2008 = yearSpecFit(district_id=2, 
                                                   district_year_data = hounde_2008, 
                                                   year_now = 2008, hc_vector= c(1:4,6:11,15:18),
                                                   a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_a0_beta0_2009 = yearSpecFit(district_id=2, 
                                                   district_year_data = hounde_2009, 
                                                   year_now = 2009, hc_vector= c(1,15:17),
                                                   a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
    
    
    EstimNonEpiHoundeForc_a0_beta0_2010 = yearSpecFit(district_id=2, 
                                                   district_year_data = hounde_2010, 
                                                   year_now = 2010, hc_vector = c(5),
                                                   a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
    
    EstimNonEpiLenaForc_a0_beta0_2006 = yearSpecFit(district_id=3, 
                                                 district_year_data = lena_2006, 
                                                 year_now = 2006, hc_vector = c(1:4),
                                                 a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                 addCarriageConstrain = TRUE, 
                                                 nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                 n_iter = 10000)
    
    
    
    EstimNonEpiLenaForc_a0_beta0_2007 = yearSpecFit(district_id=3,
                                                 district_year_data = lena_2007, 
                                                 year_now = 2007, hc_vector = c(1,3),
                                                 a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                 addCarriageConstrain = TRUE, 
                                                 nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                 n_iter = 10000)
    
    
    ## === DISTRICT OF K.VIGUE (id = 4) === ###
    
    EstimNonEpiKvigueForc_a0_beta0_2008 = yearSpecFit(district_id=4, 
                                                   district_year_data = Kvigue_2008, 
                                                   year_now = 2008, hc_vector = c(2,3),
                                                   a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
    
    EstimNonEpiKvigueForc_a0_beta0_2010 = yearSpecFit(district_id=4, 
                                                   district_year_data = Kvigue_2010, 
                                                   year_now = 2010, hc_vector = c(3,5),
                                                   a0ForcingOnly = FALSE, beta0ForcingOnly = FALSE, 
                                                   addCarriageConstrain = TRUE, 
                                                   nloptrAlgorithm = "NLOPT_LN_COBYLA",
                                                   n_iter = 10000)
}else{
    cat("\n The file \"a0_beta0_ForcEstimates.RData\" already exist \n in :",getwd(), 
        "\n Please delete it first in order to reestimate parameters")
} #end of if -else conditional statement for force_beta0 estimates. 


save(seguenNonEpiForc_a0_beta0_2006,
     seguenNonEpiForc_a0_beta0_2007,
     seguenNonEpiForc_a0_beta0_2009,
     seguenNonEpiForc_a0_beta0_2010,
     EstimNonEpiHoundeForc_a0_beta0_2004,
     EstimNonEpiHoundeForc_a0_beta0_2006,
     EstimNonEpiHoundeForc_a0_beta0_2007,
     EstimNonEpiHoundeForc_a0_beta0_2008,
     EstimNonEpiHoundeForc_a0_beta0_2009,
     EstimNonEpiHoundeForc_a0_beta0_2010,
     EstimNonEpiLenaForc_a0_beta0_2006,
     EstimNonEpiLenaForc_a0_beta0_2007,
     EstimNonEpiKvigueForc_a0_beta0_2008,
     EstimNonEpiKvigueForc_a0_beta0_2010,
     file="../data/processed_data/new_processed_data/a0_beta0_ForcEstimates.RData") 
 
    
############################################################################
## concatenating all the estimates per model type into data frames or matrix 
############################################################################       

# Model with forcing of the invasion alone.

a0ForcEstimates_matrice = rbind(
      seguenNonEpiForc_a0_2006,
      seguenNonEpiForc_a0_2007,
      seguenNonEpiForc_a0_2009,
      seguenNonEpiForc_a0_2010,
      EstimNonEpiHoundeForc_a0_2004,
      EstimNonEpiHoundeForc_a0_2006,
      EstimNonEpiHoundeForc_a0_2007,
      EstimNonEpiHoundeForc_a0_2008,
      EstimNonEpiHoundeForc_a0_2009,
      EstimNonEpiLenaForc_a0_2006,
      EstimNonEpiLenaForc_a0_2007,
      EstimNonEpiKvigueForc_a0_2008,
      EstimNonEpiKvigueForc_a0_2010)

save(a0ForcEstimates_matrice,
    file = "../data/processed_data/new_processed_data/a0ForcEstimates_matrice.RData")
  

# Model with forcing of the transmission alone. 

beta0ForcEstimates_matrice = rbind(
    seguenNonEpiForc_beta0_2006,
    seguenNonEpiForc_beta0_2007,
    seguenNonEpiForc_beta0_2009,
    seguenNonEpiForc_beta0_2010,
    EstimNonEpiHoundeForc_beta0_2004,
    EstimNonEpiHoundeForc_beta0_2006,
    EstimNonEpiHoundeForc_beta0_2007,
    EstimNonEpiHoundeForc_beta0_2008,
    EstimNonEpiHoundeForc_beta0_2009,
    EstimNonEpiHoundeForc_beta0_2010,
    EstimNonEpiLenaForc_beta0_2006,
    EstimNonEpiLenaForc_beta0_2007,
    EstimNonEpiKvigueForc_beta0_2008,
    EstimNonEpiKvigueForc_beta0_2010)

save(beta0ForcEstimates_matrice,
     file = "../data/processed_data/new_processed_data/beta0ForcEstimates_matrice.RData")


# Model with forcing of both the transmission and invasion rate. 

a0_beta0_ForcEstimates_matrice = rbind(
    seguenNonEpiForc_a0_beta0_2006,
    seguenNonEpiForc_a0_beta0_2007,
    seguenNonEpiForc_a0_beta0_2009,
    seguenNonEpiForc_a0_beta0_2010,
    EstimNonEpiHoundeForc_a0_beta0_2004,
    EstimNonEpiHoundeForc_a0_beta0_2006,
    EstimNonEpiHoundeForc_a0_beta0_2007,
    EstimNonEpiHoundeForc_a0_beta0_2008,
    EstimNonEpiHoundeForc_a0_beta0_2009,
    EstimNonEpiHoundeForc_a0_beta0_2010,
    EstimNonEpiLenaForc_a0_beta0_2006,
    EstimNonEpiLenaForc_a0_beta0_2007,
    EstimNonEpiKvigueForc_a0_beta0_2008,
    EstimNonEpiKvigueForc_a0_beta0_2010)

save(a0_beta0_ForcEstimates_matrice,
     file = "../data/processed_data/new_processed_data/a0_beta0_ForcEstimates_matrice.RData")


    