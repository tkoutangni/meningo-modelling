#lapply(test_params_estimates_2007_NELDER_M_M3, get_district_name)
#==========================
#installPackage('hydroGOF')
#require(hydroGOF)
## function to add computation of Percent Bias and the Ratio of Root-Mean-Squared error to data standard deviation

computeModelPredictionAndPerformStat_epi = function(paramEstimatesMatrice){

    # A function to compute model prediction based on best fitted parameters estimate obtain from optimisation routine
    # it also compute some summary estimates and statistics from the model predictions based on optimized parameters.
    # @ parameters : takes the matrix of estimated parameters (which contain estimated parameters values for each health center-year)
    peakWeeklyIncid<-vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    peakWeeklyCarriagePrev<-vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    PredictedIncidList<-list()
    PredictedCarriageList<-list()
    foldInc<-list()
    incidRange<-matrix(NA,nrow=nrow(paramEstimatesMatrice),ncol=2,byrow=T,
                       dimnames = list(c(1:dim(paramEstimatesMatrice)[1]),c("Incidmin","Incidmax")))
    carriageRange<-matrix(NA,nrow=nrow(paramEstimatesMatrice),ncol=2,byrow=T,
                          dimnames = list(c(1:dim(paramEstimatesMatrice)[1]),c("Carriagemin","Carriagemax")))
    
    # vectors for halding the computed value of model performence statistics
    
    RMSE_stat <- vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    RSR_stat <- vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    PBIAS_stat <- vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    Rsquared<- vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    a_fold<- vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    beta_fold<- vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    
    
    
    current_parms_combination = vparameters
    ## looping through the matrice of parameter estimates.
    for(i in seq_along(paramEstimatesMatrice[,"district"])){
        
        current_parms_combination["beta0"]=paramEstimatesMatrice[i,"beta0"]
        current_parms_combination["alpha"]=paramEstimatesMatrice[i,"alpha"]
        current_parms_combination["phi"]=paramEstimatesMatrice[i,"phi"]
        current_parms_combination["Susc0"]=paramEstimatesMatrice[i,"Susc0"]
        current_parms_combination["CarrierProp"]=paramEstimatesMatrice[i,"CarrierProp"]
        current_parms_combination["teta"]=paramEstimatesMatrice[i,"teta"]
        current_parms_combination["a0"]=paramEstimatesMatrice[i,"a0"]
        
        if(any("epsilon_a"%in%colnames(paramEstimatesMatrice))&
           !any("epsilon_b"%in%colnames(paramEstimatesMatrice))){
            current_parms_combination["epsilon_a"]=paramEstimatesMatrice[i,"epsilon_a"]
            ## comute fold increase of the invasion parameter.
            range_a = range(at(current_parms_combination["a0"]*year,current_parms_combination["epsilon_a"],current_parms_combination["teta"]))
            range_b = NA
        }
        else if(any("epsilon_b"%in%colnames(paramEstimatesMatrice))&
                !any("epsilon_a"%in%colnames(paramEstimatesMatrice))){
            current_parms_combination["epsilon_b"]=paramEstimatesMatrice[i,"epsilon_b"]
            ## comute fold increase of transmission parameter.
            range_b = range(betat(current_parms_combination["beta0"]*year,current_parms_combination["epsilon_b"],current_parms_combination["teta"]))
            range_a = NA
        }else{
            current_parms_combination["epsilon_a"]=paramEstimatesMatrice[i,"epsilon_a"]
            current_parms_combination["epsilon_b"]=paramEstimatesMatrice[i,"epsilon_b"]
            range_a = range(at(current_parms_combination["a0"]*year,current_parms_combination["epsilon_a"],current_parms_combination["teta"]))
            range_b = range(betat(current_parms_combination["beta0"]*year,current_parms_combination["epsilon_b"],current_parms_combination["teta"]))
        }
        
        # multiply beta0*year in range_a and range_b
        # because the betat function take beta0 argument in per year unit not per day unit.
        ## see beta0.harmonic function used inside betat function for details. Same explanation goes
        ## for why i used a0*year.
        
        
        #select data corresponding 
        year_now = paramEstimatesMatrice[i,"year"]
        #district_now = paramEstimatesMatrice[i,"district"]
        epi_district_now = get_district_name(paramEstimatesMatrice)
        # may need to moove this function from the local scope of
        # the computeModelPredictionAndPerformStat function to 
        # a general scope so that it can be extended without breaking 
        # this function code as mor health center year data becomes availaible.
        select_data<-function(year_now,epi_district_now){
            
            if (year_now==2006 & epi_district_now=="DAFRA"){
                data = epidata_dafra_2006
            }else if (year_now==2006 & epi_district_now=="GOURCY"){
                data = epidata_gourcy_2006
            }else if (year_now==2006 & epi_district_now=="SEGUENEGA"){
                data = epidata_seguenega_2006
            }else if (year_now==2006 & epi_district_now=="LENA"){
                data = epidata_lena_2006
            }else if (year_now==2006 & epi_district_now=="KARANGASSO VIGUE"){
                data = epidata_kvigue_2006
            }else if (year_now==2006 & epi_district_now=="HOUNDÉ"){
                data = epidata_hounde_2006
            }else if (year_now==2006 & epi_district_now=="DANDE"){
                data = epidata_dande_2006
            }else if (year_now==2007 & epi_district_now=="GOURCY"){
                data = epidata_gourcy_2007
            }else if (year_now==2007 & epi_district_now=="ORODARA"){
                data = epidata_orodara_2007
            }else if (year_now==2007 & epi_district_now=="SEGUENEGA"){
                data = epidata_seguenega_2007
            }else if (year_now==2008 & epi_district_now=="BOULSA"){
                data = epidata_boulsa_2008
            }else if (year_now==2008 & epi_district_now=="GOURCY"){
                data = epidata_gourcy_2008
            }else if (year_now==2008 & epi_district_now=="ORODARA"){
                data = epidata_orodara_2008
            }else if (year_now==2010 & epi_district_now=="SEGUENEGA"){
                data = epidata_seguenega_2010
            }              
            return (data)
        } # ends select data function
        
        data = select_data(year_now,epi_district_now)
        selected_hc_data = data[,paramEstimatesMatrice[i,"hc"]]
        coredata = coredata(selected_hc_data)
        
        # Compute model predictions based on the estimated parameters combination.
        nbYearSimulated = 1
        modelPredict <- sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated*year)
        modelPredict = tail(modelPredict,length(coredata))
        modelPredict <- subset(modelPredict, select = c(time,Susc,Carrier,Ill,Recov,newI))
        carriageVector = modelPredict$Carrier # variable to plot later
        
        # check if ineq_constrain on carriage worked
        #fold_change_prev_week_6_8_and_44_46 = mean(c(carriageVector[44:46])) / mean(c(carriageVector[8:6]))
        
        # COMPUTING SOMME PERFORMANCE STATISTIQUES
        
        ## function to compute percent bias and SRS statistic
        
        computeModelPerformanceStats<-function(selectedhcData,PredictedNewI){
            # 1st compute residuals between model predictions and data
            residuals = PredictedNewI - selectedhcData;
            #print(residuals)
            
            #We'll also need the number of observation data points
            
            ## nDataPoints = length (selectedhcData); ## This will underestimate the final estimate as it doesn't remoove missing data
            
            nDataPoints = length(residuals[!is.na(residuals)==TRUE]) # this is for handling missing data which might create problem in division and sqrt
            
            # To compute the RMSE
            sumOfSquaredREsiduals = sum(residuals*residuals, na.rm=TRUE) # also called sum of squared errors
            RMSE = sqrt(sumOfSquaredREsiduals/nDataPoints); # single value returned
            
            #compute the RSR statistic
            RSR = RMSE/(sd(selectedhcData,na.rm=TRUE)) # retuen a single value
            
            #I would interpret bias (systematic error) as
            #the average residual, which should be zero for random errors, i.e.
            #"noise"
            
            ## PBIAS = 100 * [ sum( sim - obs ) / sum( obs ) ] 
            
             PBIAS = pbias(sim = PredictedNewI, obs=selectedhcData,na.rm=TRUE)  # compute the percent bias based on the two specified vector of data and predictions
            
            #computing the R-squared coef
            meanObsData = mean(selectedhcData,na.rm=TRUE)
            totalSumOfSquares = sum((selectedhcData - meanObsData)^2,na.rm=TRUE)
            #sumOfSquaredError = sumOfSquaredREsiduals
            Rsquared = 1 - (sumOfSquaredREsiduals/totalSumOfSquares)
            
            ModelPerformanceStatsVector = c(RMSE,RSR,PBIAS,Rsquared)
            return (ModelPerformanceStatsVector)           
        } # ends computeModelPerformanceStats function
        
        ModelPerformanceStats = computeModelPerformanceStats(
            selectedhcData = coredata,
            PredictedNewI = modelPredict$newI) # compute the actual performence stats
        
        ## store the computed stats in their respective vector.
        RMSE_stat[i] <- ModelPerformanceStats[1] # store first value in the vector 
        RSR_stat[i] <- ModelPerformanceStats[2]
        PBIAS_stat[i] <- ModelPerformanceStats[3]
        Rsquared[i]<- ModelPerformanceStats[4]
        
        peakWeeklyIncid[i]<-max(modelPredict$newI)
        peakWeeklyCarriagePrev[i]<-max(modelPredict$Carrier)
        
        PredictedIncidList[[i]]<-modelPredict$newI
        PredictedCarriageList[[i]]<-modelPredict$Carrier
        
        incidRange[i,]<-c(range(modelPredict$newI)[1],range(modelPredict$newI)[2])
        carriageRange[i,]<-c(range(modelPredict$Carrier)[1],range(modelPredict$Carrier)[2])
        
        
        
        ####################################################################################
        #plot observed data against prediction
        ####################################################################################
        ## find max value in the observed time serie.
        #na.data.index = which(is.na(coredata))
        
        peak_value_data = max(coredata,na.rm = TRUE)
        if(max(modelPredict$newI)>peak_value_data){
            peak_value = max(modelPredict$newI)
        }else{peak_value = peak_value_data}
        
        # selecting health center name to add to plot
        hc_name = names(data)[paramEstimatesMatrice[i,"hc"]]
        
        plotOnlyPositiveRsquared = function(doPlot){
            if(doPlot == TRUE){
                hc_with_very_poor_fit = Rsquared[i]<0
                doPlot = !hc_with_very_poor_fit
            }else{
                doPlot = !doPlot
            }
            return(doPlot)
        }
        
        doPlot = plotOnlyPositiveRsquared(FALSE)
        if(doPlot){
            # Plot observed data
            plot(coredata*1e+05,las=1,pch=20, col="black",type="p",
                 ylab="",
                 xlab="",
                 ylim =c(0,1.1*peak_value*1e+05)
            )
            # plot model incidence predictions
            lines(modelPredict$newI*1e+05,col="red",type="l",pch=20,lwd=2)
            
            title(main=paste(hc_name),
                  xlab=paste("Calendar Weeks",year_now),ylab="Incid x100,000")
            
            # adding the plot of model predictions of carriers to the previous plot
            addCarriagePlot = function(){
                par(new = T)
                carriagePrev = carriageVector*1e+02
                plot(carriagePrev,col="azure4",type="l",lwd=2, lty=2, axes=FALSE, bty = "n", xlab = "", ylab = "")
                axis(side = 4, at=pretty(range(carriagePrev)),col.axis="azure4",las=1)
                mtext("Carriers(%)", side=4, line=2,cex = 0.7, col="azure4")
                
                legend("topright",
                       legend=c("Data","Cases","Carr"),
                       lty=c(0,1,2), pch=c(20, NA, NA),
                       lwd=c(NA,2,2),
                       col=c("black","red","azure4"),
                       bty="n",bg="transparent",horiz=FALSE, merge=TRUE) #horiz = TRUE for horizontal legend
            } # end addCarriagePlot function
            ## IMPORTANT !! CALLING THE addCarriagePlot function
            addCarriagePlot() # call the function
            
            #add fold increase in invasion and or transmission to plot
            addFoldIncrease = function(range_a=NA,range_b=NA,Rsquared=NULL){
                a_fold = round(range_a[2]/range_a[1],1)
                beta_fold = round(range_b[2]/range_b[1],1)
                Rsquared = round(Rsquared*100)
                title(sub=bquote(paste(a[0],'-fold=',.(a_fold),
                                       "; ",beta[0],'-fold=',.(beta_fold),
                                       "; ",R^{2},'=',.(Rsquared)
                )),
                col.sub="blue",cex.sub=1.1)
            }
            
            addFoldIncrease(range_a,range_b,Rsquared[i]) # add fold increase to the plot
        }# end if(!hc_with_very_poor_fit) statement
        a_fold[i]<-round(range_a[2]/range_a[1],1)
        beta_fold[i]<-round(range_b[2]/range_b[1],1)
    } # end for loop
    return(list(a_fold=a_fold,beta_fold=beta_fold,peakWeeklyIncid=peakWeeklyIncid,peakWeeklyCarriagePrev=peakWeeklyCarriagePrev,
                PredictedIncidList=PredictedIncidList,PredictedCarriageList=PredictedCarriageList,
                incidRange=incidRange,carriageRange=carriageRange,RMSE_stat=RMSE_stat,RSR_stat=RSR_stat,PBIAS_stat=PBIAS_stat,Rsquared = Rsquared))
} # computeModelPrediction function






