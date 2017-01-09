# the Objective function in "fit_nloptr_algo_one_year_automate.R" discriminate only based
# on model cost (sum of the squared residuals)
#source("fit_nloptr_algo_one_year_automate.R")

# the Objective function in "yearSpecificFittingAlgo_nov_2015.R" add a criteria
# which checks plausibility of carriage prevalence variations.
source("yearSpecificFittingAlgo_nov_2015.R") 

#par(cex.lab=1.1) # seeting plotting parameters
#op <- par(fig=c(0,1,0,1),mfrow=c(2,3),
          #mar=c(5,4,3,1),mgp=c(2,1,0))
currentPar= par()
currentPar$mfrow = c(2,3)
suppressWarnings(par(currentPar))

doPlot = FALSE
if(doPlot){
    par(mfrow=c(2,3))
    plot(seguen_2007[,c(1:6)]*1e+05,plot.type="m",type="b",ylab="")
    plot(seguen_2007[,c(2,3,5,6)]*1e+05,plot.type="m",type="b",ylab="")
    plot(seguen_2009[,6]*1e+05,plot.type="m",type="b",ylab="",las=1)
    plot(seguen_2010[,c(1:6)]*1e+05,plot.type="m",type="b",ylab="",las=1)
}

## IMPORTANT ! the sourced file containing the FITTING ALGORITHM require to set 
## either of a0Forcing or beta0Forcing or both to "TRUE" depending if we want to seasonaly force those parameters
## By default their values are FALSE in the algo. Which mean the algo assums both parameters are unforced.
## a0Forcing = "FALSE" means however that the parameter is rather increased by a factor of 10 without seasonal forcing
## Also remember to switch the appropriate cosine function for a(t) before running the parameters estimates

#=============================================================
## Parameters estimates when only invasion rate is forced
#=============================================================
#a0ForcingOnly = FALSE
#beta0ForcingOnly = FALSE
if(!exists("a0ForcEstimates.RData")){

    a0Forcing = TRUE
    beta0Forcing = FALSE
    
    # DISTRICT OF HOUNDE ( Id = 1)
    seguenNonEpiForc_a0_2006 = computeEstimates(
                                                district_id=1,district_year_data = seguen_2006, year_now = 2006, hc_vector= c(1:4,6),
                            figureFile="../figs/seguenNonEpiForc_a0_2006.png")
    
    
    
    seguenNonEpiForc_a0_2007 = computeEstimates(
                                                district_id=1, district_year_data = seguen_2007, year_now = 2007, hc_vector= c(2,3,5,6),
                                                figureFile="../figs/seguenNonEpiForc_a0_2007.png")
    
    
    seguenNonEpiForc_a0_2009 = computeEstimates(
                                                district_id=1, district_year_data = seguen_2009, year_now = 2009, hc_vector= c(6),
                                                figureFile="../figs/seguenNonEpiForc_a0_2009.png")
    
    seguenNonEpiForc_a0_2010 = computeEstimates(
                                                district_id=1, district_year_data = seguen_2010, year_now = 2010, hc_vector= c(1:6),
                                                figureFile="../figs/seguenNonEpiForc_a0_2010.png")
    
    
    # DISTRICT OF HOUNDE ( Id = 2)
    
    EstimNonEpiHoundeForc_a0_2004 = computeEstimates(
        district_id=2, district_year_data = hounde_2004, year_now = 2004, hc_vector= c(4,7,13),
        figureFile = "../figs/houndeNonEpi_2004.png")
    
    
    EstimNonEpiHoundeForc_a0_2006 = computeEstimates(
        district_id=2, district_year_data = hounde_2006, year_now = 2006, hc_vector= c(1:14),
        figureFile = "../figs/houndeNonEpi_2006_1.png")
    
        #dev.print(png, file = "../figs/houndeNonEpi_2006_2.png", width = 836, height = 446, bg = "transparent")
        #dev.print(png, file = "../figs/houndeNonEpi_2006_3.png", width = 836, height = 446, bg = "transparent")
    
    
    
    EstimNonEpiHoundeForc_a0_2007 = computeEstimates(
        district_id=2, district_year_data = hounde_2007, year_now = 2007, hc_vector= c(2,3,15,17),
        figureFile = "../figs/houndeNonEpi_2007.png")
    
    
    EstimNonEpiHoundeForc_a0_2008 = yearSpecFit(
        district_id=2, district_year_data = hounde_2008, year_now = 2008, hc_vector= c(1:4,6:11,15:18))
    
    EstimNonEpiHoundeForc_a0_2009 = yearSpecFit(
        district_id=2, district_year_data = hounde_2009, year_now = 2009, hc_vector= c(1,15:17))
    
    EstimNonEpiHoundeForc_a0_2010= yearSpecFit(
        district_id=2, district_year_data = hounde_2010, year_now = 2010, hc_vector = c(5))
    
    # save the variables to the workspace in order to load them at any time
    save(seguenNonEpiForc_a0_2006,
         seguenNonEpiForc_a0_2007,
         seguenNonEpiForc_a0_2010,
         EstimNonEpiHoundeForc_a0_2004,
         EstimNonEpiHoundeForc_a0_2006,
         EstimNonEpiHoundeForc_a0_2007,
         EstimNonEpiHoundeForc_a0_2008,
         EstimNonEpiHoundeForc_a0_2009,
         file="a0ForcEstimates.RData")
}

# DISTRICT OF LENA ( Id = 3)
if(!exists("a0ForcEstimates2.RData")){
EstimNonEpiLenaForc_a0_2006 = yearSpecFit(district_id=3, district_year_data = lena_2006, year_now = 2006, hc_vector = c(1:4))
dev.print(png, file = "../figs/lenaNonEpi_2006.png", width = 836, height = 446, bg = "transparent")

EstimNonEpiLenaForc_a0_2007 = yearSpecFit(district_id=3, district_year_data = lena_2007, year_now = 2007, hc_vector = c(1,3))
dev.print(png, file = "../figs/lenaNonEpi_2006_2007.png", width = 836, height = 446, bg = "transparent")

## === DISTRICT OF K.VIGUE (id = 4) === ###

EstimNonEpiKvigueForc_a0_2008 = yearSpecFit(district_id=4, district_year_data = Kvigue_2008, year_now = 2008, hc_vector = c(2,3))
EstimNonEpiKvigueForc_a0_2010 = yearSpecFit(district_id=4, district_year_data = Kvigue_2010, year_now = 2010, hc_vector = c(3,5))

save(EstimNonEpiLenaForc_a0_2006,
    EstimNonEpiLenaForc_a0_2007,
    EstimNonEpiKvigueForc_a0_2008,
    EstimNonEpiKvigueForc_a0_2010,
    file="a0ForcEstimates2.RData")

}
#=============================================================
## Parameters estimates when only transmission rate is forced
#=============================================================
if(!exists("beta0ForcEstimateSeguen.RData")){
    
a0Forcing = FALSE    # for this remember to change the seasonal function for a0 to be
                     # unscaled cosine function
beta0Forcing = TRUE
## District of segunega ID=1

seguenNonEpiForc_beta0_2006 = computeEstimates(
    district_id=1,district_year_data = seguen_2006, year_now = 2006, hc_vector= c(1:4,6),
    figureFile="../figs/seguenNonEpiForc_beta0_2006.png")



seguenNonEpiForc_beta0_2007 = computeEstimates(
    district_id=1, district_year_data = seguen_2007, year_now = 2007, hc_vector= c(2,3,5,6),
    figureFile="../figs/seguenNonEpiForc_beta0_2007.png")


seguenNonEpiForc_beta0_2009 = computeEstimates(
    district_id=1, district_year_data = seguen_2009, year_now = 2009, hc_vector= c(6),
    figureFile="../figs/seguenNonEpiForc_beta0_2009.png")

seguenNonEpiForc_beta0_2010 = computeEstimates(
    district_id=1, district_year_data = seguen_2010, year_now = 2010, hc_vector= c(1:6),
    figureFile="../figs/seguenNonEpiForc_beta0_2010.png")

save(seguenNonEpiForc_beta0_2006,
     seguenNonEpiForc_beta0_2007,
     #seguenNonEpiForc_beta0_2009, #estimate is missing due to indexing problem
     seguenNonEpiForc_beta0_2010,
    file="beta0ForcEstimateSeguen.RData")
}

## District of hounde ID=2
if(!existss("beta0ForcEstimateHoundeKvigue.RData")){
EstimNonEpiHoundeForc_beta0_2004 = computeEstimates(
    district_id=2, district_year_data = hounde_2004, year_now = 2004, hc_vector= c(4,7,13),
    figureFile = "../figs/houndeNonEpiForc_beta0_2004.png")


EstimNonEpiHoundeForc_beta0_2006 = computeEstimates(
    district_id=2, district_year_data = hounde_2006, year_now = 2006, hc_vector= c(1:14),
    figureFile = "../figs/houndeNonEpiForc_beta0_2006_1.png")

#dev.print(png, file = "../figs/houndeNonEpi_2006_2.png", width = 836, height = 446, bg = "transparent")
#dev.print(png, file = "../figs/houndeNonEpi_2006_3.png", width = 836, height = 446, bg = "transparent")



EstimNonEpiHoundeForc_beta0_2007 = computeEstimates(
    district_id=2, district_year_data = hounde_2007, year_now = 2007, hc_vector= c(2,3,15,17),
    figureFile = "../figs/houndeNonEpiForc_beta0_2007.png")


EstimNonEpiHoundeForc_beta0_2008 = yearSpecFit(
    district_id=2, district_year_data = hounde_2008, year_now = 2008, hc_vector= c(1:4,6:11,15:18))

EstimNonEpiHoundeForc_beta0_2009 = yearSpecFit(
    district_id=2, district_year_data = hounde_2009, year_now = 2009, hc_vector= c(1,15:17))

EstimNonEpiHoundeForc_beta0_2010= yearSpecFit(
    district_id=2, district_year_data = hounde_2010, year_now = 2010, hc_vector = c(5))



# DISTRICT OF LENA ( Id = 3)

EstimNonEpiLenaForc_beta0_2006 = yearSpecFit(district_id=3, district_year_data = lena_2006, year_now = 2006, hc_vector = c(1:4))

EstimNonEpiLenaForc_beta0_2007 = yearSpecFit(district_id=3, district_year_data = lena_2007, year_now = 2007, hc_vector = c(1,3))

## === DISTRICT OF K.VIGUE (id = 4) === ###

EstimNonEpiKvigueForc_beta0_2008 = yearSpecFit(district_id=4, district_year_data = Kvigue_2008, year_now = 2008, hc_vector = c(2,3))
EstimNonEpiKvigueForc_beta0_2010 = yearSpecFit(district_id=4, district_year_data = Kvigue_2010, year_now = 2010, hc_vector = c(3,5))

save(
     EstimNonEpiHoundeForc_beta0_2004,
     EstimNonEpiHoundeForc_beta0_2006,
     EstimNonEpiHoundeForc_beta0_2007,
     EstimNonEpiHoundeForc_beta0_2008,
     EstimNonEpiHoundeForc_beta0_2009,
     #EstimNonEpiHoundeForc_beta0_2010,# estimate is missing due to indexing problem
     EstimNonEpiLenaForc_beta0_2006,
     EstimNonEpiLenaForc_beta0_2007,
     EstimNonEpiKvigueForc_beta0_2008,
     EstimNonEpiKvigueForc_beta0_2010,
     file="beta0ForcEstimateHoundeKvigue.RData")

}
#====================================================================
## Parameters estimates when invasion and transmission rate is forced
#====================================================================
if(!existss("beta0anda0ForcEstimates.RData")){
a0Forcing = TRUE
beta0Forcing = TRUE

seguenNonEpiForc_beta0anda0_2006 = computeEstimates(
                                                    district_id=1,district_year_data = seguen_2006, year_now = 2006, hc_vector= c(1:4,6),
                                                    figureFile="../figs/seguenNonEpiForc_beta0anda0_2006.png")



seguenNonEpiForc_beta0anda0_2007 = computeEstimates(
                                                    district_id=1, district_year_data = seguen_2007, year_now = 2007, hc_vector= c(2,3,5,6),
                                                    figureFile="../figs/seguenNonEpiForc_beta0anda0_2007.png")


seguenNonEpiForc_beta0anda0_2009 = computeEstimates(
                                                    district_id=1, district_year_data = seguen_2009, year_now = 2009, hc_vector= c(6),
                                                    figureFile="../figs/seguenNonEpiForc_beta0anda0_2009.png")

seguenNonEpiForc_beta0anda0_2010 = computeEstimates(
                                                    district_id=1, district_year_data = seguen_2010, year_now = 2010, hc_vector= c(1:6),
                                                    figureFile="../figs/seguenNonEpiForc_beta0anda0_2010.png")




## District of hounde ID=2

EstimNonEpiHoundeForc_beta0anda0_2004 = computeEstimates(
    district_id=2, district_year_data = hounde_2004, year_now = 2004, hc_vector= c(4,7,13),
    figureFile = "../figs/houndeNonEpiForc_beta0anda0_2004.png")


EstimNonEpiHoundeForc_beta0anda0_2006 = computeEstimates(
    district_id=2, district_year_data = hounde_2006, year_now = 2006, hc_vector= c(1:14),
    figureFile = "../figs/houndeNonEpiForc_beta0anda0_2006_1.png")

#dev.print(png, file = "../figs/houndeNonEpi_2006_2.png", width = 836, height = 446, bg = "transparent")
#dev.print(png, file = "../figs/houndeNonEpi_2006_3.png", width = 836, height = 446, bg = "transparent")



EstimNonEpiHoundeForc_beta0anda0_2007 = computeEstimates(
    district_id=2, district_year_data = hounde_2007, year_now = 2007, hc_vector= c(2,3,15,17),
    figureFile = "../figs/houndeNonEpiForc_beta0anda0_2007.png")


EstimNonEpiHoundeForc_beta0anda0_2008 = yearSpecFit(
    district_id=2, district_year_data = hounde_2008, year_now = 2008, hc_vector= c(1:4,6:11,15:18))

EstimNonEpiHoundeForc_beta0anda0_2009 = yearSpecFit(
    district_id=2, district_year_data = hounde_2009, year_now = 2009, hc_vector= c(1,15:17))

EstimNonEpiHoundeForc_beta0anda0_2010= yearSpecFit(
    district_id=2, district_year_data = hounde_2010, year_now = 2010, hc_vector = c(5))



# DISTRICT OF LENA ( Id = 3)

EstimNonEpiLenaForc_beta0anda0_2006 = yearSpecFit(district_id=3, district_year_data = lena_2006, year_now = 2006, hc_vector = c(1:4))

EstimNonEpiLenaForc_beta0anda0_2007 = yearSpecFit(district_id=3, district_year_data = lena_2007, year_now = 2007, hc_vector = c(1,3))

## === DISTRICT OF K.VIGUE (id = 4) === ###

EstimNonEpiKvigueForc_beta0anda0_2008 = yearSpecFit(district_id=4, district_year_data = Kvigue_2008, year_now = 2008, hc_vector = c(2,3))
EstimNonEpiKvigueForc_beta0anda0_2010 = yearSpecFit(district_id=4, district_year_data = Kvigue_2010, year_now = 2010, hc_vector = c(3,5))


save(seguenNonEpiForc_beta0anda0_2006,
     seguenNonEpiForc_beta0anda0_2007,
     seguenNonEpiForc_beta0anda0_2009,
     seguenNonEpiForc_beta0anda0_2010,
     EstimNonEpiHoundeForc_beta0anda0_2004,
     EstimNonEpiHoundeForc_beta0anda0_2006,
     EstimNonEpiHoundeForc_beta0anda0_2007,
     EstimNonEpiHoundeForc_beta0anda0_2008,
     EstimNonEpiHoundeForc_beta0anda0_2009,
     EstimNonEpiHoundeForc_beta0anda0_2010,
     EstimNonEpiLenaForc_beta0anda0_2006,
     EstimNonEpiLenaForc_beta0anda0_2007,
     EstimNonEpiKvigueForc_beta0anda0_2008,
     EstimNonEpiKvigueForc_beta0anda0_2010,
     file="beta0anda0ForcEstimates.RData")

}
if(!existss("allEstmatesForNonEpi.RData")){
save(seguenNonEpiForc_a0_2006,
     seguenNonEpiForc_a0_2007,
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
     
     seguenNonEpiForc_beta0_2006,
     seguenNonEpiForc_beta0_2007,
     #seguenNonEpiForc_beta0_2009, # estimate is missing due to indexing problem
     seguenNonEpiForc_beta0_2010,
     EstimNonEpiHoundeForc_beta0_2004,
     EstimNonEpiHoundeForc_beta0_2006,
     EstimNonEpiHoundeForc_beta0_2007,
     EstimNonEpiHoundeForc_beta0_2008,
     EstimNonEpiHoundeForc_beta0_2009,
     #EstimNonEpiHoundeForc_beta0_2010, # estimate is missing due to indexing problem
     EstimNonEpiLenaForc_beta0_2006,
     EstimNonEpiLenaForc_beta0_2007,
     EstimNonEpiKvigueForc_beta0_2008,
     EstimNonEpiKvigueForc_beta0_2010,
     
     seguenNonEpiForc_beta0anda0_2006,
     seguenNonEpiForc_beta0anda0_2007,
     seguenNonEpiForc_beta0anda0_2009,
     seguenNonEpiForc_beta0anda0_2010,
     EstimNonEpiHoundeForc_beta0anda0_2004,
     EstimNonEpiHoundeForc_beta0anda0_2006,
     EstimNonEpiHoundeForc_beta0anda0_2007,
     EstimNonEpiHoundeForc_beta0anda0_2008,
     EstimNonEpiHoundeForc_beta0anda0_2009,
     EstimNonEpiHoundeForc_beta0anda0_2010,
     EstimNonEpiLenaForc_beta0anda0_2006,
     EstimNonEpiLenaForc_beta0anda0_2007,
     EstimNonEpiKvigueForc_beta0anda0_2008,
     EstimNonEpiKvigueForc_beta0anda0_2010,
     file="allEstmatesForNonEpi.RData") 
}


#####################################################
## concatenating all the estimates per model type

# model with seasonal foecing of the invasion rate alone.

forc_a0ParamEstimNonEpi<-rbind(
    seguenNonEpiForc_a0_2006,
    seguenNonEpiForc_a0_2007,
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

save(forc_a0ParamEstimNonEpi,file="forc_a0ParamEstimNonEpi.RData")

# model with seasonal forcing of the transmission rate alone.

forc_beta0ParamEstimNonEpi<-rbind(
    seguenNonEpiForc_beta0_2006,
    seguenNonEpiForc_beta0_2007,
    #seguenNonEpiForc_beta0_2009, # estimate is missing due to indexing problem
    seguenNonEpiForc_beta0_2010,
    EstimNonEpiHoundeForc_beta0_2004,
    EstimNonEpiHoundeForc_beta0_2006,
    EstimNonEpiHoundeForc_beta0_2007,
    EstimNonEpiHoundeForc_beta0_2008,
    EstimNonEpiHoundeForc_beta0_2009,
    #EstimNonEpiHoundeForc_beta0_2010, # estimate is missing due to indexing problem
    EstimNonEpiLenaForc_beta0_2006,
    EstimNonEpiLenaForc_beta0_2007,
    EstimNonEpiKvigueForc_beta0_2008,
    EstimNonEpiKvigueForc_beta0_2010
    )

save(forc_beta0ParamEstimNonEpi,file="forc_beta0ParamEstimNonEpi.RData")


forc_a0andbeta0ParamEstimNonEpi<-rbind(
    seguenNonEpiForc_beta0anda0_2006,
    seguenNonEpiForc_beta0anda0_2007,
    seguenNonEpiForc_beta0anda0_2009,
    seguenNonEpiForc_beta0anda0_2010,
    EstimNonEpiHoundeForc_beta0anda0_2004,
    EstimNonEpiHoundeForc_beta0anda0_2006,
    EstimNonEpiHoundeForc_beta0anda0_2007,
    EstimNonEpiHoundeForc_beta0anda0_2008,
    EstimNonEpiHoundeForc_beta0anda0_2009,
    EstimNonEpiHoundeForc_beta0anda0_2010,
    EstimNonEpiLenaForc_beta0anda0_2006,
    EstimNonEpiLenaForc_beta0anda0_2007,
    EstimNonEpiKvigueForc_beta0anda0_2008,
    EstimNonEpiKvigueForc_beta0anda0_2010
    )

save(forc_a0andbeta0ParamEstimNonEpi,file="forc_a0andbeta0ParamEstimNonEpi.RData")

## Doing somme summary graphs for parameters estimates
##============================
## Model with a0 forcing only.
##============================

## remoove all observation where Rsquared is negative

a0ForcNegativeRsquared<-which(forc_a0ParamEstimNonEpi[,"Rsquared"]<0)


new_forc_a0ParamEstimNonEpi<-forc_a0ParamEstimNonEpi[-a0ForcNegativeRsquared,]

## plot result per district for the Rsquared
#boxplot(new_forc_a0ParamEstimNonEpi[,"Rsquared"]~new_forc_a0ParamEstimNonEpi[,"district"])

## Get the median and interquartile range (1st and 3rd quartiles)

round(summary(new_forc_a0ParamEstimNonEpi[,"Rsquared"]),2)
hist(new_forc_a0ParamEstimNonEpi[,"Rsquared"],main="",xlab="Rsquared")
writelabel("a")

## estimate model predictions using the estimated parameters.

# a function to compute the model predictions given each combination of parameters set.
## The following compute model prediction and return graphs for trajectory matching between
# between model and data.
# it also return peak week incidences for each prediction based on estimated parameters values

computeModelPrediction = function(paramEstimatesMatrice){
    peakWeeklyIncid<-vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    peakWeeklyCarriagePrev<-vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    PredictedIncidList<-list()
    PredictedCarriageList<-list()
    incidRange<-matrix(NA,nrow=nrow(paramEstimatesMatrice),ncol=2,byrow=T,
                       dimnames = list(c(1:dim(paramEstimatesMatrice)[1]),c("Incidmin","Incidmax")))
    carriageRange<-matrix(NA,nrow=nrow(paramEstimatesMatrice),ncol=2,byrow=T,
                          dimnames = list(c(1:dim(paramEstimatesMatrice)[1]),c("Carriagemin","Carriagemax")))
    
    for(i in seq_along(paramEstimatesMatrice[,"district"])){
        
        current_parms_combination["beta0"]=paramEstimatesMatrice[i,"beta0"]
        current_parms_combination["alpha"]=paramEstimatesMatrice[i,"alpha"]
        current_parms_combination["phi"]=paramEstimatesMatrice[i,"phi"]
        current_parms_combination["Susc0"]=paramEstimatesMatrice[i,"Susc0"]
        current_parms_combination["CarrierProp"]=paramEstimatesMatrice[i,"CarrierProp"]
        current_parms_combination["teta"]=paramEstimatesMatrice[i,"teta"]
        
        if(any("epsilon_a"%in%colnames(paramEstimatesMatrice))){
            current_parms_combination["epsilon_a"]=paramEstimatesMatrice[i,"epsilon_a"]
        }
        if(any("epsilon_b"%in%colnames(paramEstimatesMatrice))){
            current_parms_combination["epsilon_b"]=paramEstimatesMatrice[i,"epsilon_b"]
        }
        current_parms_combination["a0"]=paramEstimatesMatrice[i,"a0"]
        
        
        
        #plot data against prediction
        
        # ploting data and model.
        
        #select data corresponding 
        year_now=paramEstimatesMatrice[i,"year"]
        district_now = paramEstimatesMatrice[i,"district"]
        
        select_data<-function(year_now,district_now){
            if(year_now==2006 & district_now==1){
                data = seguen_2006
            }else if(year_now==2007 & district_now==1) {
                data = seguen_2007
            }else if (year_now==2008 & district_now==1){
                data = seguen_2008
            }else if(year_now==2009 & district_now==1){
                data = seguen_2009
            }else if(year_now==2010 & district_now==1){
                data = seguen_2010
            }else if(year_now==2004 & district_now==2){
                data = hounde_2004
            }else if(year_now==2005 & district_now==2){
                data=hounde_2005
            }else if (year_now==2006 & district_now==2){
                data = hounde_2006
            }else if (year_now==2007 & district_now==2){
                data = hounde_2007
            }else if(year_now==2008 & district_now==2){
                data = hounde_2008
            }else if(year_now==2009 & district_now==2){
                data= hounde_2009
            }else if (year_now==2010 & district_now==2){
                data = hounde_2010
            }else if(year_now==2004 & district_now==3){
                data = lena_2004
            }else if(year_now==2005 & district_now==3){
                data = lena_2005
            }else if (year_now==2006 & district_now==3){
                data = lena_2006
            }else if(year_now==2007 & district_now==3){
                data= lena_2007
            }else if (year_now==2008 & district_now==3){
                data = lena_2008
            }else if (year_now==2009 & district_now==3){
                data = lena_2009
            }else if (year_now==2010 & district_now==3){
                data = lena_2010
            }else if(year_now==2008 & district_now==4){
                data = Kvigue_2008
            }else if(year_now==2009 & district_now==4){
                data = Kvigue_2009
            }else if(year_now==2010 & district_now==4){
                data = Kvigue_2010
            }
            return (data)
        } # select data function
        
        data = select_data(year_now,district_now)
        selected_hs_data = data[,paramEstimatesMatrice[i,"hs"]]
        coredata = coredata(selected_hs_data)
        
        modelPredict <- sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated*year)
        modelPredict = tail(modelPredict,length(coredata))
        
        peakWeeklyIncid[i]<-max(modelPredict$newI)
        peakWeeklyCarriagePrev[i]<-max(modelPredict$Carrier)
        
        PredictedIncidList[[i]]<-modelPredict$newI
        PredictedCarriageList[[i]]<-modelPredict$Carrier
        
        incidRange[i,]<-c(range(modelPredict$newI)[1],range(modelPredict$newI)[2])
        carriageRange[i,]<-c(range(modelPredict$Carrier)[1],range(modelPredict$Carrier)[2])
        
        plot(coredata*1e+05,las=1,pch=1, # 20 the old value of pch
             lwd=1,col="black",type="p",
             #ylim=c(0,(1.1*max(data[,paramEstimatesMatrice[i,"hs"]],na.rm=T)*1e+05)),
             ylab="",
             xlab="") #,ylim=c(0,(1.1*max(data[,j]*1e+05)))
        lines(modelPredict$newI*1e+05,col="blue",type="l",pch=1,lwd=1)
        
        hs_name = names(data)[paramEstimatesMatrice[i,"hs"]] # selecting health center name
        title(main=paste(hs_name),
              xlab=paste("Calendar Weeks",year_now),ylab="Incid x100,000")
        
    }# end for loop
    return(list(peakWeeklyIncid=peakWeeklyIncid,peakWeeklyCarriagePrev=peakWeeklyCarriagePrev,
                PredictedIncidList=PredictedIncidList,PredictedCarriageList=PredictedCarriageList,
                incidRange=incidRange,carriageRange=carriageRange))
}


peakWeeklyEstimForc_a0 = computeModelPrediction(new_forc_a0ParamEstimNonEpi)
save(peakWeeklyEstimForc_a0,file="peakWeeklyEstimForc_a0.RData")

range(peakWeeklyEstimForc_a0$incidRange[,"Incidmin"]*1e+05)
range(peakWeeklyEstimForc_a0$incidRange[,"Incidmax"]*1e+05)

range(peakWeeklyEstimForc_a0$carriageRange[,"Carriagemin"]*100)
range(peakWeeklyEstimForc_a0$carriageRange[,"Carriagemax"]*100)



computeFold_inc<-function(paramEstimatesMatrice){
    if(any("epsilon_b"%in%colnames(paramEstimatesMatrice))&any("epsilon_a"%in%colnames(paramEstimatesMatrice))){
        fold_inc<-matrix(NA,ncol=2,nrow=dim(paramEstimatesMatrice)[1],byrow=TRUE,
        dimnames = list(c(1:dim(paramEstimatesMatrice)[1]),c("fold_inc_a0","fold_inc_beta0")))
        
    }else if(!any("epsilon_b"%in%colnames(paramEstimatesMatrice))){
        fold_inc<-matrix(NA,ncol=1,nrow=dim(paramEstimatesMatrice)[1],byrow=TRUE,
                         dimnames = list(c(1:dim(paramEstimatesMatrice)[1]),c("fold_inc_a0")))
    }else if(!any("epsilon_a"%in%colnames(paramEstimatesMatrice))){
        fold_inc<-matrix(NA,ncol=1,nrow=dim(paramEstimatesMatrice)[1],byrow=TRUE,
                         dimnames = list(c(1:dim(paramEstimatesMatrice)[1]),c("fold_inc_beta0")))
    }
    
    for (i in 1:dim(paramEstimatesMatrice)[1]){
        if(any("epsilon_a"%in%colnames(paramEstimatesMatrice))){
        compute_at = at(paramEstimatesMatrice[,c("a0")][i]*year,paramEstimatesMatrice[,c("epsilon_a")][i],paramEstimatesMatrice[,c("teta")][i])
        range_at = compute_at$range[2]/compute_at$range[1]
        }
        
        if(any("epsilon_b"%in%colnames(paramEstimatesMatrice))){
        compute_betat = betat(paramEstimatesMatrice[,c("beta0")][i]*year,paramEstimatesMatrice[,c("epsilon_b")][i],paramEstimatesMatrice[,c("teta")][i])
        range_betat = compute_betat$range[2]/compute_betat$range[1]
        }
        
        if(any("epsilon_b"%in%colnames(paramEstimatesMatrice))&any("epsilon_a"%in%colnames(paramEstimatesMatrice))){
        fold_inc[i,]=append(range_at,range_betat)
        }else if(!any("epsilon_b"%in%colnames(paramEstimatesMatrice))){
            fold_inc[i,]= range_at  
        }else if(!any("epsilon_a"%in%colnames(paramEstimatesMatrice))){
            fold_inc[i,]= range_betat
        }
        
    }
    return(fold_inc)
}

estimatedFoldInc_a0 = computeFold_inc(new_forc_a0ParamEstimNonEpi)
save(estimatedFoldInc_a0,file="estimatedFoldInc_a0.RData")
summary(estimatedFoldInc_a0)

## WARNING : Remember to manually change the forcing function of a(t) to standard cosine
##============================
## Model with beta0 forcing only.
##============================
## remoove all observation where Rsquared is negative
beta0ForcNegativeRsquared<-which(forc_beta0ParamEstimNonEpi[,"Rsquared"]<0)

new_forc_beta0ParamEstimNonEpi<-forc_beta0ParamEstimNonEpi[-beta0ForcNegativeRsquared,]
hist(new_forc_beta0ParamEstimNonEpi[,"Rsquared"],main="",xlab="Rsquared")
round(summary(new_forc_beta0ParamEstimNonEpi[,"Rsquared"]),2)
writelabel("b")


peakWeeklyEstimForc_beta0 = computeModelPrediction(new_forc_beta0ParamEstimNonEpi)

range(peakWeeklyEstimForc_beta0$incidRange[,"Incidmin"]*1e+05)
range(peakWeeklyEstimForc_beta0$incidRange[,"Incidmax"]*1e+05)

range(peakWeeklyEstimForc_beta0$carriageRange[,"Carriagemin"]*100)
range(peakWeeklyEstimForc_beta0$carriageRange[,"Carriagemax"]*100)

# remember to change forcing function of a(t) to be unscaled before runing 
# foldIncreaseEstimate_beta0

foldIncreaseEstimate_beta0<-computeFold_inc(new_forc_beta0ParamEstimNonEpi)
summary(foldIncreaseEstimate_beta0)




#=====================================================
# Model with forcing of both transmission and invasion.
##=====================================================

## remoove all observation where Rsquared is negative
a0Andbeta0ForcNegativeRsquared<-which(forc_a0andbeta0ParamEstimNonEpi[,"Rsquared"]<0)

new_forc_a0Andbeta0ParamEstimNonEpi<-forc_a0andbeta0ParamEstimNonEpi[-a0Andbeta0ForcNegativeRsquared,]
hist(new_forc_a0Andbeta0ParamEstimNonEpi[,"Rsquared"],main="",xlab="Rsquared")
writelabel("c)",cex=.98)
round(summary(new_forc_a0Andbeta0ParamEstimNonEpi[,"Rsquared"]),2)

peakWeeklyEstimForc_a0_beta0 = computeModelPrediction(new_forc_a0Andbeta0ParamEstimNonEpi)
save(peakWeeklyEstimForc_a0_beta0,file="peakWeeklyEstimForc_a0_beta0.RData")

#boxplot(new_forc_a0ParamEstimNonEpi[,"Rsquared"],new_forc_beta0ParamEstimNonEpi[,"Rsquared"],new_forc_a0Andbeta0ParamEstimNonEpi[,"Rsquared"])

foldIncA0Beta0 = computeFold_inc(new_forc_a0Andbeta0ParamEstimNonEpi)

range(peakWeeklyEstimForc_a0_beta0$incidRange[,"Incidmin"]*1e+05)
range(peakWeeklyEstimForc_a0_beta0$incidRange[,"Incidmax"]*1e+05)

range(peakWeeklyEstimForc_a0_beta0$carriageRange[,"Carriagemin"]*100)
range(peakWeeklyEstimForc_a0_beta0$carriageRange[,"Carriagemax"]*100)

addVarforc_a0Andbeta0ParamEstimNonEpi = cbind(new_forc_a0Andbeta0ParamEstimNonEpi,foldIncA0Beta0)
par(mfrow=c(1,2))

boxplot(log(addVarforc_a0Andbeta0ParamEstimNonEpi[,"fold_inc_beta0"])~addVarforc_a0Andbeta0ParamEstimNonEpi[,"year"],main="Fold-increase \n in transmission rate",
        ylab="Fold-increase (Log scale)",xlab="Year")
boxplot(log(addVarforc_a0Andbeta0ParamEstimNonEpi[,"fold_inc_a0"])~addVarforc_a0Andbeta0ParamEstimNonEpi[,"year"],
        main="Fold-increase \n in invasion rate",
        xlab="Year")

boxplot(cbind("Transmission\n rate" = foldIncA0Beta0[foldIncA0Beta0[,"fold_inc_beta0"]<415], 
             "Invasion\n rate"=foldIncA0Beta0[,"fold_inc_a0"]),ylab="Fold-increase")

# boxplot showing distribution of teta according to years.
getTeta = new_forc_a0Andbeta0ParamEstimNonEpi[,"teta"]/7
getYear = new_forc_a0Andbeta0ParamEstimNonEpi[,"year"]
getPhi = new_forc_a0Andbeta0ParamEstimNonEpi[,"phi"]
getBeta0 = new_forc_a0Andbeta0ParamEstimNonEpi[,"beta0"]
geta0 = new_forc_a0Andbeta0ParamEstimNonEpi[,"a0"]
getAlpha = new_forc_a0Andbeta0ParamEstimNonEpi[,"alpha"]
getCarrierProp = new_forc_a0Andbeta0ParamEstimNonEpi[,"CarrierProp"]
getSusc0 = new_forc_a0Andbeta0ParamEstimNonEpi[,"Susc0"]
getEpsilon_a = new_forc_a0Andbeta0ParamEstimNonEpi[,"epsilon_a"]
getEpsilon_b = new_forc_a0Andbeta0ParamEstimNonEpi[,"epsilon_b"]
getFoldInc_a0 = addVarforc_a0Andbeta0ParamEstimNonEpi[,"fold_inc_a0"]
getFoldInc_beta0 = addVarforc_a0Andbeta0ParamEstimNonEpi[,"fold_inc_beta0"]
getIndexOfFold_inc_beta0<-which(addVarforc_a0Andbeta0ParamEstimNonEpi[,"fold_inc_beta0"]<400)
getYearVector<-addVarforc_a0Andbeta0ParamEstimNonEpi[getIndexOfFold_inc_beta0,"year"]

summary(getSusc0)
summary(getCarrierProp)


par(cex.lab=1.1)
op <- par(fig=c(0,1,0,1),mfrow=c(2,3),
          mar=c(5,4,3,1),mgp=c(2,1,0))

boxplot(getTeta~getYear,
        xlab="Year",ylab="Calendar week",
        main="Avg timing of weekly \n incidence peak")

summary(getTeta*7) # already converted into calendar weeks number
## plot phi

boxplot(((1/getPhi)/year)~getYear,
        xlab="Year",ylab="Number of years",
        main="Avg duration of immunity")
summary((1/getPhi)/year)


boxplot(getBeta0*year~getYear,
        xlab="Year",ylab="Rate /year",ylim=c(0,250),
        main="Mean transmission rate")
summary(getBeta0*year)

# plot a0
boxplot(geta0*year~getYear,
        xlab="Year",ylab="Rate /year",ylim=c(0,0.035),
        main="Mean Invasion rate")
summary(a0*year)
# plot (alpha)
boxplot(1/getAlpha~getYear,
        xlab="Year",ylab="Number of days",
        main="Avg Duration of \n assymptomatic carriage")
summary(1/getAlpha)


# plot epsilon_a and epsilon_b



## plot fold-increases

par(cex.lab=1.1)
op <- par(fig=c(0,1,0,1),mfrow=c(2,2),
          mar=c(5,4,3,1),mgp=c(2,1,0))

boxplot(getEpsilon_a~getYear,
        xlab="Year",ylab="Forcing Amplitude",
        main="Invasion Seasonal \n forcing amplitude")
summary(getEpsilon_a)


boxplot(getEpsilon_b~getYear,
        xlab="Year",ylab="Forcing Amplitude",
        main="Transmission Seasonal \n forcing amplitude")
summary(getEpsilon_b)


boxplot(getFoldInc_a0~getYear,
        xlab="Year",ylab="Fold-increase",
        main="Invasion rate \n fold-increase ",las=1)

boxplot(getFoldInc_beta0[getFoldInc_beta0<400]~getYearVector,
        xlab="Year",ylab="Fold-increase",
        main="Transmission rate \n fold-increase ",las=1)











##==============================================
# Evrything after this line to be deleted later
##==============================================

EstimNonEpiSeguen_2006 = yearSpecFit(district_id=1, district_year_data = seguen_2006, year_now = 2006, hc_vector= c(1:4,6))
dev.print(png, file = "../figs/segueneguaNonEpi_2006.png", width = 836, height = 446, bg = "transparent")

EstimNonEpiSeguen_2007 = yearSpecFit(district_id=1, district_year_data = seguen_2007, year_now = 2007, hc_vector= c(2,3,5,6))
dev.print(png, file = "../figs/segueneguaNonEpi_2007.png", width = 836, height = 446, bg = "transparent")

EstimNonEpiSeguen_2009 = yearSpecFit(district_id=1, district_year_data = seguen_2009, year_now = 2009, hc_vector= c(6))
dev.print(png, file = "../figs/segueneguaNonEpi_2009.png", width = 836, height = 446, bg = "transparent")

EstimNonEpiSeguen_2010 = yearSpecFit(district_id=1, district_year_data = seguen_2010, year_now = 2010, hc_vector= c(1:6))
dev.print(png, file = "../figs/segueneguaNonEpi_2010.png", width = 836, height = 446, bg = "transparent")


if(doPlot){
    op <- par(mar = c(5,7,4,2) + 0.1)
    par(op)
    
    plot(hounde_2004[,c(1:14)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    plot(hounde_2004[,c(4,7,13)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    # index to use : c(4,7,13)
    plot(hounde_2005[,c(1:14)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    #index to use : none
    
    plot(hounde_2006[,c(1:14)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black")
    #index to use c(1:14)
    plot(hounde_2007[,c(1:18)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black") 
    plot(hounde_2007[,c(2,3,15,17)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black")
    #index to use c(2,3,15,17)
    
    plot(hounde_2008[,c(1:18)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black")
    plot(hounde_2008[,c(1:4,6:11,15:18)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black") # useful
    #index to use c(1:4,6:11,15:18)
    
    
    plot(hounde_2009[,c(1:18)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black")
    plot(hounde_2009[,c(1,15:17)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black")
    # index to use c(1,15:17)
    
    plot(hounde_2010[,c(1:18)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black")
    plot(hounde_2010[,c(4:5)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m",col="black")
    # index to use c(5)
    
} # end of doPlot conditional

# DISTRICT OF HOUNDE ( Id = 2)
EstimNonEpiHounde_2004 = yearSpecFit(district_id=2, district_year_data = hounde_2004, year_now = 2004, hc_vector= c(4,7,13))
dev.print(png, file = "../figs/houndeNonEpi_2004.png", width = 836, height = 446, bg = "transparent")

par(mfrow=c(2,3))
EstimNonEpiHounde_2006 = yearSpecFit(district_id=2, district_year_data = hounde_2006, year_now = 2006, hc_vector= c(1:14))

dev.print(png, file = "../figs/houndeNonEpi_2006_1.png", width = 836, height = 446, bg = "transparent")
dev.print(png, file = "../figs/houndeNonEpi_2006_2.png", width = 836, height = 446, bg = "transparent")
dev.print(png, file = "../figs/houndeNonEpi_2006_3.png", width = 836, height = 446, bg = "transparent")

par(mfrow=c(2,3))
EstimNonEpiHounde_2007 = yearSpecFit(district_id=2, district_year_data = hounde_2007, year_now = 2007, hc_vector= c(2,3,15,17))
dev.print(png, file = "../figs/houndeNonEpi_2007.png", width = 836, height = 446, bg = "transparent")


par(mfrow = c(2,3))
EstimNonEpiHounde_2008 = yearSpecFit(district_id=2, district_year_data = hounde_2008, year_now = 2008, hc_vector= c(1:4,6:11,15:18))

dev.print(png, file = "../figs/houndeNonEpi_2008_1.png", width = 836, height = 446, bg = "transparent")
dev.print(png, file = "../figs/houndeNonEpi_2008_2.png", width = 836, height = 446, bg = "transparent")
dev.print(png, file = "../figs/houndeNonEpi_2008_3.png", width = 836, height = 446, bg = "transparent")

par(mfrow = c(2,3))
EstimNonEpiHounde_2009 = yearSpecFit(district_id=2, district_year_data = hounde_2009, year_now = 2009, hc_vector= c(1,15:17))
dev.print(png, file = "../figs/houndeNonEpi_2009.png", width = 836, height = 446, bg = "transparent")

par(mfrow = c(2,3))
EstimNonEpiHounde_2010 = yearSpecFit(district_id=2, district_year_data = hounde_2010, year_now = 2010, hc_vector = c(5))
dev.print(png, file = "../figs/houndeNonEpi_2010.png", width = 836, height = 446, bg = "transparent")

EstimNonEpiHounde_2010 = yearSpecFit(district_id=2, district_year_data = hounde_2010, year_now = 2010, hc_vector = c(5))


####======== LENA DISTRICT  (district id = 3)== ==== ####

if(doPlot){
    op <- par(mar = c(5,7,4,2) + 0.1)
    par(op)
    plot(lena_2005[,c(1:6)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    #index to use none
    plot(lena_2004[,c(1:6)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    # index to use : none
    
    plot(lena_2006[,c(1:6)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    #index to use: c(1:4)
    plot(lena_2007[,c(1:6)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    # index to use c(1,3)
    plot(lena_2008[,c(1:6)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    #index to use : none
    plot(lena_2009[,c(1:6)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    #index to use : none
    plot(lena_2010[,c(1:6)]*1e+05,type="b",pch=16,ylab="",las=1,plot.type="m")
    # index to use none
}

par(mfrow = c(2,3))
EstimNonEpiLena_2006 = yearSpecFit(district_id=3, district_year_data = lena_2006, year_now = 2006, hc_vector = c(1:4))
dev.print(png, file = "../figs/lenaNonEpi_2006.png", width = 836, height = 446, bg = "transparent")

EstimNonEpiLena_2007 = yearSpecFit(district_id=3, district_year_data = lena_2007, year_now = 2007, hc_vector = c(1,3))
dev.print(png, file = "../figs/lenaNonEpi_2006_2007.png", width = 836, height = 446, bg = "transparent")

## === K.vigue district (id = 4) === ###

par(mfrow = c(2,3))
EstimNonEpiKvigue_2008 = yearSpecFit(district_id=4, district_year_data = Kvigue_2008, year_now = 2008, hc_vector = c(2,3))
EstimNonEpiKvigue_2010 = yearSpecFit(district_id=4, district_year_data = Kvigue_2010, year_now = 2010, hc_vector = c(3,5))
dev.print(png, file = "../figs/kvigueNonEpi_2008_et_2010.png", width = 836, height = 446, bg = "transparent")





plot(Kvigue_2008[,c(2,3)],type="b")


toto = rbind(EstimNonEpiSeguen_2006,EstimNonEpiSeguen_2007,EstimNonEpiSeguen_2009,EstimNonEpiSeguen_2010,EstimNonEpiHounde_2004,EstimNonEpiHounde_2006,EstimNonEpiHounde_2007,EstimNonEpiHounde_2008,EstimNonEpiHounde_2009,EstimNonEpiHounde_2010,EstimNonEpiLena_2006,EstimNonEpiLena_2007,EstimNonEpiKvigue_2008,EstimNonEpiKvigue_2010)


boxplot(toto[,4]*year~toto[,3], main="Mean Transmission Rate", 
        xlab="Calendar year", ylab="Rate/year")

boxplot(1/toto[,5]~toto[,3], main="Avg Duration of Assymptomatic \n Carriage", 
        xlab="Calendar Year", ylab=" Number of days")

boxplot((1/toto[,6])/year~toto[,3], main="Avg Duration of Immunity ", 
        xlab="Calendar Year", ylab="Number of Years")

boxplot((toto[,9])/7~toto[,3], main="Avg timing of weekly \n incidence peak", 
        xlab="Calendar Year", ylab="Calendar Week Number")

boxplot((toto[,12])*year~toto[,3], main="Mean Invasion Rate", 
        xlab="Calendar Year", ylab="Rate/year")

fold_inc<-matrix(NA,ncol=2,nrow=dim(toto)[1],byrow=TRUE,
         dimnames = list(c(1:dim(toto)[1]),c("fold_inc_a0","fold_inc_beta0")))
for (i in 1:dim(toto)[1]){
compute_at = at(toto[,c("a0")][i]*year,toto[,c("epsilon_a")][i],toto[,c("teta")][i])
range_at = compute_at$range[2]/compute_at$range[1]
compute_betat = betat(toto[,c("beta0")][i]*year,toto[,c("epsilon_b")][i],toto[,c("teta")][i])
range_betat = compute_betat$range[2]/compute_betat$range[1]
fold_inc[i,]=append(range_at,range_betat)
}

toto = toto[,-c(16:19)]
par(mfrow=c(2,3))
boxplot((toto[,c("fold_inc_a0")])~toto[,3], main="Mean Invasion Rate \n fold Increase", 
        xlab="Calendar Year", ylab="fold increase")

boxplot((toto[,c("fold_inc_beta0")])~toto[,3], main="Mean Transmission Rate \n fold Increase", 
        xlab="Calendar Year", ylab="fold increase")

boxplot(toto[,c("fold_inc_beta0")][which(toto[,c("fold_inc_beta0")]<50000)]~toto[,3][which(toto[,c("fold_inc_beta0")]<50000)], main="Mean Transmission Rate \n fold Increase", 
        xlab="Calendar Year", ylab="fold increase")




