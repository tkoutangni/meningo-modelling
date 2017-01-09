##########################################################
# This file contains custom functions.
# Author: Thibaut Koutangni -- thibautkoutangni@gmail.com
# Created: Oct, 2015
# Copyright Thibaut Koutangni, 2015
############################################################

#############################################################
# Defining some constant for use in the script and function.
per_100000 = 1e+05
#############################################################

# Some function for data manipulation and visualization

plot_time_serie = function(zoo_time_serie){
    xyplot(zoo_time_serie, 
           xlab="Time (Calendar Weeks)",
           ylab="Incid. per 100,000",type="b",pch=16)
}

add_julian_day = function(zoo_time_serie,year){  
    selected_year = zoo_time_serie[year(time(zoo_time_serie))==year]
    data_with_julian_day = julian_day(data=selected_year, orig="2003-12-28")
    return(data_with_julian_day)
} #end of add_julian_day function.

# A function for melting a data frame for use in a ggplot function
melt_data = function(zoo_time_serie,year){
    # this function assume the zoo time serie has a column named "julian_day"
    #remove the "julian_day_column_first
    #[,-which(colnames(zoo_time_serie)%in%c("julian_day"))]
    subset_data = subset(zoo_time_serie,year(time(zoo_time_serie))==year)
    # transforme the zoo time serie into a data.frame with a column
    # containing the dates
    subset_data_frame = fortify(subset_data)
    # melt the data / reshape it in long format for use by ggplot2
    melt_data_frame = melt(subset_data_frame, id=c("Index"),variable.name = "health_center_name",
                           value.name = 'weekly_incid')
    return(melt_data_frame)
} #end melt data function


melt_data_all_year = function(zoo_time_serie){
    
    data_frame = fortify(zoo_time_serie)
    # melt the data / reshape it in long format for use by ggplot2
    melt_data_frame = melt(data_frame, id=c("Index"),variable.name = "health_center_name",
                           value.name = 'weekly_incid')
    return(melt_data_frame)
} #end melt data all year function

ggplot_time_series = function(melt_data_frame,year){
    p<-ggplot(melt_data_frame,aes(x=Index,y=weekly_incid*per_100000)) + geom_line() + geom_point()
    p<-p+facet_wrap(~health_center_name,ncol=4, scales="free_y") + xlab(paste("Calendar Weeks", year,sep=", ")) + ylab("Weekly Incidence \n per 100,000")
    
    # Apply a locally weighted regression to smooth time series
    #p<- p + stat_smooth(method = "loess", formula = y ~ x, size = 1)
    return (p)
} # end of ggplot_time_series function

ggplot_time_series_all_years = function(melt_data_frame,ncol=4){
    p<-ggplot(data=melt_data_frame[!is.na(melt_data_frame$weekly_incid),] ,aes(x=Index,y=weekly_incid*per_100000)) + geom_line() #+ geom_point()
    p<-p+facet_wrap(~health_center_name,ncol=ncol, scales="free_y") + xlab(paste("Calendar Weeks")) + ylab("Weekly Incidence \n per 100,000")
    
    # Apply a locally weighted regression to smooth time series
    #p<- p + stat_smooth(method = "loess", formula = y ~ x, size = 1)
    #p = p + theme_bw()
    #p = p + theme_opts
    return (p)
} # end function ggplot_time_series_all_years


ggplot_rbindlist_data_frames = function(list_of_data_frames){
    # function takes as argument: a clean list of data-frames with the first
    # column being the time serie dates as returned with the fortify 
    # function in ggplot2 package.
    # rbindlist is used to append the elmts (data_frames) of the list by row
    #In lapply the function melt_data is applied to each elmt of list
    # melt_data_all_year is a custum function to reshape the data in a 
    # format appropriate for ggplot2.
    ggplot_time_series_all_years(
        rbindlist(
            lapply(list_of_data_frames, melt_data_all_year))
    )
} # end of fuction  ggplot_rbindlist_data_frames

# keeping only health center with data that is useful.
# a function to remove unwanted health centers in each district.
remove_unwanted_column = function(data_frame_or_zoo_serie,colnames_to_remove){
    # take as first argument a zoo time serie or data frame
    # takes as second argument a vector of the column names to remove
    data_with_wanted_column = data_frame_or_zoo_serie[,which(!colnames(data_frame_or_zoo_serie)%in%colnames_to_remove)]
    return(data_with_wanted_column)
}

# select data for a given year from a time serie.
select_ts_year = function(zoo_time_serie,year){
    return(zoo_time_serie[year(time(zoo_time_serie))==year])
} # end select_ts_year function.

######################################################

# function for sample simulation of the SCIRS model

sample_sim = function(epsilon_a, epsilon_b, forcing_invasion, age_structured=FALSE, nyears = 5) {
    vparameters["epsilon_a"] = epsilon_a;
    vparameters["epsilon_b"] = epsilon_b;
    is_a0Constant = !forcing_invasion;
    if (age_structured){
        sample_sim = sim.SCIRS_harmonic_age(inits, vparameters, nyears * year)
    }else{
        sample_sim = sim.SCIRS_harmonic(inits, vparameters, nyears * year);
    }

}

# Graphics settings
graphSettings <- function() {
    return(suppressWarnings(par(
        mar = c(5,5,2,4), mfrow = c(3,3), par(mgp=c(2.3, 1, 0))
        # mgp for setting xlab and ylab distance from their axis.
    )))
}

addSysDate <- function(fileName = " ") {
    # function to add current date to saved files
    paste(fileName,Sys.Date(),sep = "_")
} # addSysdate ends.

saveWorkspace <- function(fileName = " ") {
    # function so save current workspace
    # @ parameters : fileName is must be of character class.
    return(save.image(file = paste(addSysDate(fileName),"RData",sep = ".")))
} # saveWorkspace ends.

# installPackage IS MOVED TO rpackage.R file for refactoring 'runfirst.R' execusion flow

# installPackage <- function(packageName) {
#     #scheck if a package is installed then install it if not.
#     #@ parmeter : a character representing the package name
#     if (packageName %in% rownames(installed.packages()) ==  FALSE) {
#         install.packages(packageName, repos = "https://cran.univ-paris1.fr/")
#     }else{
#         cat("COSTUM MESSAGE: ", packageName, " package was already installed.\n")
#     }
# } # installPackage ends

addTenthPowerTextToPlot <- function(power) {
    # function to add mtext x10^-y to the plot.
    # @parameter: a real number representing the power of 10
    # by which the yaxis values have been multiply. eg. -5        with     display
    # x10^-5 on the left top of the plot
    mtext(bquote(paste('x',10 ^ .(power))),adj = 0,cex = .8)
}   # addTenthPowerTextToPlot ends

drawCostumXaxisTicks <- function(dataFrame) {
    # draw costum yearly ticks and labels on the x axis of a plot
    # @ parameter: a dataframe or matrix
    if (dim(dataFrame)[1] * week >= 2 * year) {
        # draw yearly xaxis ticks and labels
        whereToDrawTicks = seq(0,dim(dataFrame)[1] * week,by = year)[-1]
        labelsOfTicks = seq(1,length(whereToDrawTicks),by = 1)
        axis(1, at = c(whereToDrawTicks), labels = c(labelsOfTicks))
        title(xlab = "Time (years)")
    }else{
        # draw weekly xaxis ticks and labels
        # the number 5 is used here to draw xaxis ticks evry 5 weeks
        # for readability of plot when the weekly time series being plot is not
        # longer than 2 years
        whereToDrawTicks = seq(0,dim(dataFrame)[1] * week, by = week*5)[-1]
        labelsOfTicks = seq(1,length(whereToDrawTicks),by = 1)*5
        axis(1, at = c(whereToDrawTicks), labels = c(labelsOfTicks))
        title(xlab = "Calendar weeks")
    }
}# function ends

plotModelVariable <- function(x,y,perPop) {
    #@ parameter: the model variable to plot
    # and the per population number wich is eg :1e+05 for 100,000          pop
    plot(
        x,y * perPop,xlab = "",
        ylab = "",main = "",type = "l", xaxt = "n",
        col = "black",lwd = 2,las = 1
    )
}

plotAllModelVariables <- function(modelOut, per_100000 = TRUE, percent = TRUE) {
    # only set per_100000, and percent to FALSE when model output number of cases
    # instead of proportion of the total population. 
    # (meaning if population size N = 1, or npop = 1)
    if (percent){
        percent<-1e+02
        addTenthPowerTextToPlot = addTenthPowerTextToPlot(power = -2)
        
    }else{
        percent<-1
        addTenthPowerTextToPlot = ''
    }
    
    if (per_100000){
        per_100000<-1e+05
        addTenthPowerTextToPlot = addTenthPowerTextToPlot(power = -5)
    }else{
        per_100000 = 1
        addTenthPowerTextToPlot = ''
    }
    suppressWarnings(par(graphSettings)) # set graphical paramaters.
    plotModelVariable(modelOut$time,modelOut$newI, perPop = per_100000)
    title(ylab = "Weekly Cases Incid.")
    addTenthPowerTextToPlot
    drawCostumXaxisTicks(modelOut)
    
    plotModelVariable(modelOut$time,modelOut$Carrier,perPop = percent)
    addTenthPowerTextToPlot
    drawCostumXaxisTicks(modelOut)
    title(ylab = "Carriers")
    
    plotModelVariable(modelOut$time,modelOut$Ill,perPop = percent)
    addTenthPowerTextToPlot
    drawCostumXaxisTicks(modelOut)
    title(ylab = "Ill")
    
    plotModelVariable(modelOut$time,modelOut$Susc,perPop = percent)
    addTenthPowerTextToPlot
    drawCostumXaxisTicks(modelOut)
    title(ylab = "Susceptibles")
    
    plotModelVariable(modelOut$time,modelOut$Recov,perPop = percent)
    addTenthPowerTextToPlot
    drawCostumXaxisTicks(modelOut)
    title(ylab = "Recovered")
    
    plotModelVariable(modelOut$time,modelOut$Ill,perPop = percent)
    addTenthPowerTextToPlot
    drawCostumXaxisTicks(modelOut)
    title(ylab = "Ill")
}

## replace carriers or cases proportion values infinitely small and negative by 0

replace_negligeable_negative_values <- function(vector) {
    vector[vector<0]<-0
    return (vector)
} #end function

add_text_to_plot <- function(text_to_plot,
                             font_cex,
                             valign,
                             halign) {
    textplot(
        text_to_plot, valign = valign, halign = halign,
        mar = c(0,0,0,0), col = "black", family = "serif", cex = font_cex
    )
}# end function.


computeModelPredictionAndPerformStat <-
    function(paramEstimatesMatrice) {
        # A function to compute model prediction based on best fitted parameters
        # estimate obtain from optimisation routine it also compute some summary
        # estimates and statistics from the model predictions based on
        # optimized parameters.
        # @ parameters : takes the matrix of estimated parameters (which
        # contain estimated parameters values for each health center-year)
        peakWeeklyIncid <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        peakWeeklyCarriagePrev <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        PredictedIncidList <- list()
        PredictedCarriageList <- list()
        foldInc <- list()
        incidRange <-
            matrix(
                NA,nrow = nrow(paramEstimatesMatrice),ncol = 2,byrow = T,
                dimnames = list(c(1:dim(
                    paramEstimatesMatrice
                )[1]),c("Incidmin","Incidmax"))
            )
        carriageRange <-
            matrix(
                NA,nrow = nrow(paramEstimatesMatrice),ncol = 2,byrow = T,
                dimnames = list(c(1:dim(
                    paramEstimatesMatrice
                )[1]),c("Carriagemin","Carriagemax"))
            )
        
        # vectors for storing the computed values of model performence statistics
        
        RMSE_stat <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        RSR_stat <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        PBIAS_stat <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        Rsquared <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        a_fold <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        beta_fold <-
            vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
        
        current_parms_combination = vparameters
        
        for (i in seq_along(paramEstimatesMatrice[,"district"])) {
            current_parms_combination["beta0"] = paramEstimatesMatrice[i,"beta0"]
            current_parms_combination["alpha"] = paramEstimatesMatrice[i,"alpha"]
            current_parms_combination["phi"] = paramEstimatesMatrice[i,"phi"]
            current_parms_combination["Susc0"] = paramEstimatesMatrice[i,"Susc0"]
            current_parms_combination["CarrierProp"] = paramEstimatesMatrice[i,"CarrierProp"]
            current_parms_combination["teta"] = paramEstimatesMatrice[i,"teta"]
            current_parms_combination["a0"] = paramEstimatesMatrice[i,"a0"]
            
            if (any("epsilon_a" %in% colnames(paramEstimatesMatrice)) &
                !any("epsilon_b" %in% colnames(paramEstimatesMatrice))) {
                current_parms_combination["epsilon_a"] = paramEstimatesMatrice[i,"epsilon_a"]
                ## comute fold increase of the invasion parameter.
                range_a = range(
                    at(
                        current_parms_combination["a0"] * year,current_parms_combination["epsilon_a"],current_parms_combination["teta"]
                    )
                )
                range_b = NA
            }
            else if (any("epsilon_b" %in% colnames(paramEstimatesMatrice)) &
                     !any("epsilon_a" %in% colnames(paramEstimatesMatrice))) {
                current_parms_combination["epsilon_b"] = paramEstimatesMatrice[i,"epsilon_b"]
                ## comute fold increase of transmission parameter.
                range_b = range(
                    betat(
                        current_parms_combination["beta0"] * year,current_parms_combination["epsilon_b"],current_parms_combination["teta"]
                    )
                )
                range_a = NA
            }else{
                current_parms_combination["epsilon_a"] = paramEstimatesMatrice[i,"epsilon_a"]
                current_parms_combination["epsilon_b"] = paramEstimatesMatrice[i,"epsilon_b"]
                range_a = range(
                    at(
                        current_parms_combination["a0"] * year,current_parms_combination["epsilon_a"],current_parms_combination["teta"]
                    )
                )
                range_b = range(
                    betat(
                        current_parms_combination["beta0"] * year,current_parms_combination["epsilon_b"],current_parms_combination["teta"]
                    )
                )
            }
            
            # multiply beta0*year in range_a and range_b
            # because the betat function take beta0 argument in per year unit not per day unit.
            ## see beta0.harmonic function used inside betat function for details. Same explanation goes
            ## for why i used a0*year.
            
            #select data corresponding
            year_now = paramEstimatesMatrice[i,"year"]
            district_now = paramEstimatesMatrice[i,"district"]
            # may need to moove this function from the local scope of
            # the computeModelPredictionAndPerformStat function to
            # a general scope so that it can be extended without breaking
            # this function code as mor health center year data becomes availaible.
            select_data <- function(year_now,district_now) {
                if (year_now == 2006 & district_now == 1) {
                    data = seguen_2006
                }else if (year_now == 2007 & district_now == 1) {
                    data = seguen_2007
                }else if (year_now == 2008 & district_now == 1) {
                    data = seguen_2008
                }else if (year_now == 2009 & district_now == 1) {
                    data = seguen_2009
                }else if (year_now == 2010 & district_now == 1) {
                    data = seguen_2010
                }else if (year_now == 2004 & district_now == 2) {
                    data = hounde_2004
                }else if (year_now == 2005 & district_now == 2) {
                    data = hounde_2005
                }else if (year_now == 2006 & district_now == 2) {
                    data = hounde_2006
                }else if (year_now == 2007 & district_now == 2) {
                    data = hounde_2007
                }else if (year_now == 2008 & district_now == 2) {
                    data = hounde_2008
                }else if (year_now == 2009 & district_now == 2) {
                    data = hounde_2009
                }else if (year_now == 2010 & district_now == 2) {
                    data = hounde_2010
                }else if (year_now == 2004 & district_now == 3) {
                    data = lena_2004
                }else if (year_now == 2005 & district_now == 3) {
                    data = lena_2005
                }else if (year_now == 2006 & district_now == 3) {
                    data = lena_2006
                }else if (year_now == 2007 & district_now == 3) {
                    data = lena_2007
                }else if (year_now == 2008 & district_now == 3) {
                    data = lena_2008
                }else if (year_now == 2009 & district_now == 3) {
                    data = lena_2009
                }else if (year_now == 2010 & district_now == 3) {
                    data = lena_2010
                }else if (year_now == 2008 & district_now == 4) {
                    data = Kvigue_2008
                }else if (year_now == 2009 & district_now == 4) {
                    data = Kvigue_2009
                }else if (year_now == 2010 & district_now == 4) {
                    data = Kvigue_2010
                }
                return (data)
            } # ends select data function
            
            data = select_data(year_now,district_now)
            selected_hc_data = data[,paramEstimatesMatrice[i,"hc"]]
            coredata = coredata(selected_hc_data)
            
            # Compute model predictions based on the estimated parameters combination.
            nbYearSimulated = 1
            modelPredict <-
                sim.SCIRS_harmonic(inits,current_parms_combination,nd = nbYearSimulated *
                                       year)
            modelPredict = tail(modelPredict,length(coredata))
            modelPredict <-
                subset(modelPredict, select = c(time,Susc,Carrier,Ill,Recov,newI))
            carriageVector = modelPredict$Carrier # variable to plot later
            carriageVector = sapply(carriageVector,replace_negligeable_negative_values)
            
            # check if ineq_constrain on carriage worked
            #fold_change_prev_week_6_8_and_44_46 = mean(c(carriageVector[44:46])) / mean(c(carriageVector[8:6]))
            
            # COMPUTING SOMME PERFORMANCE STATISTIQUES
            
            ## function to compute percent bias and SRS statistic
            
            computeModelPerformanceStats <-
                function(selectedhcData,PredictedNewI) {
                    # 1st compute residuals between model predictions and data
                    residuals = PredictedNewI - selectedhcData;
                    #print(residuals)
                    
                    #We'll also need the number of observation data points
                    
                    ## nDataPoints = length (selectedhcData); ## This will underestimate the final estimate as it doesn't remoove missing data
                    
                    nDataPoints = length(residuals[!is.na(residuals) == TRUE]) # this is for handling missing data which might create problem in division and sqrt
                    
                    # To compute the RMSE
                    sumOfSquaredREsiduals = sum(residuals * residuals, na.rm = TRUE) # also called sum of squared errors
                    RMSE = sqrt(sumOfSquaredREsiduals / nDataPoints); # single value returned
                    
                    #compute the RSR statistic
                    RSR = RMSE / (sd(selectedhcData,na.rm = TRUE)) # retuen a single value
                    
                    #I would interpret bias (systematic error) as
                    #the average residual, which should be zero for random errors, i.e.
                    #"noise"
                    
                    ## PBIAS = 100 * [ sum( sim - obs ) / sum( obs ) ]
                    
                    PBIAS = pbias(PredictedNewI, selectedhcData,na.rm = TRUE)  # compute the percent bias based on the two specified vector of data and predictions
                    
                    #computing the R-squared coef
                    meanObsData = mean(selectedhcData,na.rm = TRUE)
                    totalSumOfSquares = sum((selectedhcData - meanObsData) ^ 2,na.rm =
                                                TRUE)
                    #sumOfSquaredError = sumOfSquaredREsiduals
                    Rsquared = 1 - (sumOfSquaredREsiduals / totalSumOfSquares)
                    
                    ModelPerformanceStatsVector = c(RMSE,RSR,PBIAS,Rsquared)
                    return (ModelPerformanceStatsVector)
                } # ends computeModelPerformanceStats function
            
            ModelPerformanceStats = computeModelPerformanceStats(coredata,modelPredict$newI) # compute the actual performence stats
            
            ## store the computed stats in their respective vector.
            RMSE_stat[i] <-
                ModelPerformanceStats[1] # store first value in the vector
            RSR_stat[i] <- ModelPerformanceStats[2]
            PBIAS_stat[i] <- ModelPerformanceStats[3]
            Rsquared[i] <- ModelPerformanceStats[4]
            
            peakWeeklyIncid[i] <- max(modelPredict$newI)
            peakWeeklyCarriagePrev[i] <- max(modelPredict$Carrier)
            
            PredictedIncidList[[i]] <- modelPredict$newI
            PredictedCarriageList[[i]] <- modelPredict$Carrier
            
            incidRange[i,] <-
                c(range(modelPredict$newI)[1],range(modelPredict$newI)[2])
            carriageRange[i,] <-
                c(range(modelPredict$Carrier)[1],range(modelPredict$Carrier)[2])
            
            
            
            ####################################################################################
            #plot observed data against prediction
            ####################################################################################
            ## find max value in the observed time serie.
            #na.data.index = which(is.na(coredata))
            
            peak_value_data = max(coredata,na.rm = TRUE)
            if (max(modelPredict$newI) > peak_value_data) {
                peak_value = max(modelPredict$newI)
            }else{
                peak_value = peak_value_data
            }
            
            # selecting health center name to add to plot
            hc_name = names(data)[paramEstimatesMatrice[i,"hc"]]
            
            plotOnlyPositiveRsquared = function(doPlot) {
                if (doPlot == TRUE) {
                    hc_with_very_poor_fit = Rsquared[i] < 0
                    doPlot = !hc_with_very_poor_fit
                }else{
                    doPlot = !doPlot
                }
                return(doPlot)
            }
            
            doPlot = plotOnlyPositiveRsquared(FALSE)
            if (doPlot) {
                # Plot observed data
                plot(
                    coredata * 1e+05,las = 1,pch = 20, col = "black",type = "p",
                    ylab = "",
                    xlab = "",
                    ylim = c(0,1.1 * peak_value * 1e+05)
                )
                # plot model incidence predictions
                lines(
                    modelPredict$newI * 1e+05,col = "black",type = "l",pch = 20,lwd = 3
                )
                
                title(
                    main = paste(hc_name),
                    xlab = paste("Calendar Weeks",year_now),ylab = "Incidence per 100,000"
                )
                
                # adding the plot of model predictions of carriers to the previous plot
                addCarriagePlot = function() {
                    par(new = T)
                    carriagePrev = carriageVector * 1e+02
                    plot(
                        carriagePrev,col = "azure4",type = "l",lwd = 3, lty = 2, axes = FALSE, bty = "n", xlab = "", ylab = ""
                    )
                    axis(
                        side = 4, at = pretty(range(carriagePrev)),col.axis = "azure4",las = 1
                    )
                    mtext(
                        "Carriers (%)", side = 4, line = 2.4,cex = 0.7, col = "azure4"
                    )
                    
                    legend(
                        "topright",
                        legend = c("Data","Cases","Carriers"),
                        lty = c(0,1,2), pch = c(20, NA, NA),
                        lwd = c(NA,3,3),
                        col = c("black","black","azure4"),
                        bty = "n",bg = "transparent",horiz = FALSE, merge =
                            TRUE
                    ) #horiz = TRUE for horizontal legend
                } # end addCarriagePlot function
                ## IMPORTANT !! CALLING THE addCarriagePlot function
                addCarriagePlot() # call the function
                
                #add fold increase in invasion and or transmission to plot
                addFoldIncrease = function(range_a = NA,range_b = NA,Rsquared =
                                               NULL) {
                    a_fold = round(range_a[2] / range_a[1],1)
                    beta_fold = round(range_b[2] / range_b[1],1)
                    Rsquared = round(Rsquared * 100)
                    if(is.na(beta_fold)){
                        title(
                            sub = bquote(
                                paste(
                                    a[0],'-fold = ',.(a_fold),
                                    
                                    "; ",R ^ {
                                        2
                                    },' = ',.(Rsquared)
                                )
                            ),
                            col.sub = "black",cex.sub = 1,line=3.6
                            
                        )
                    }else if(is.na(a_fold)){
                        title(
                            sub = bquote(
                                paste(
    
                                    beta[0],'-fold = ',.(beta_fold),
                                    "; ",R ^ {
                                        2
                                    },' = ',.(Rsquared)
                                )
                            ),
                            col.sub = "black",cex.sub = 1,line=3.5
                            
                        )
                    }else{
                    title(
                        sub = bquote(
                            paste(
                                a[0],'-fold = ',.(a_fold),
                                "; ",beta[0],'-fold = ',.(beta_fold),
                                "; ",R ^ {
                                    2
                                },' = ',.(Rsquared)
                            )
                        ),
                        col.sub = "black",cex.sub = 1,line=3.5
                        
                    )
                        }
                }
                
                addFoldIncrease(range_a,range_b,Rsquared[i]) # add fold increase to the plot
            }# end if(!hc_with_very_poor_fit) statement
            a_fold[i] <- round(range_a[2] / range_a[1],1)
            beta_fold[i] <- round(range_b[2] / range_b[1],1)
        } # end for loop
        #dev.off() # end of printing plots to pdf.
        
        return(
            list(
                a_fold = a_fold,beta_fold = beta_fold,peakWeeklyIncid = peakWeeklyIncid,peakWeeklyCarriagePrev =
                    peakWeeklyCarriagePrev,
                PredictedIncidList = PredictedIncidList,PredictedCarriageList =
                    PredictedCarriageList,
                incidRange = incidRange,carriageRange = carriageRange,RMSE_stat =
                    RMSE_stat,RSR_stat = RSR_stat,PBIAS_stat = PBIAS_stat,Rsquared = Rsquared
            )
        )
    } # computeModelPrediction function


# Summarizing the the model_performance statistics.
sumarize_performance_stat = function(performance_stat_estim_list) {
    # Rsquared.
    rsquared_min = min(performance_stat_estim_list$Rsquared)
    rsquared_max = max(performance_stat_estim_list$Rsquared)
    rsquared_median = median(performance_stat_estim_list$Rsquared)
    rsquared_iqr = IQR(performance_stat_estim_list$Rsquared, type = 8)
    rsquared_first_quantile = quantile(performance_stat_estim_list$Rsquared)[c(2)]
    rsquared_third_quantile = quantile(performance_stat_estim_list$Rsquared)[c(4)]
    rsquared_mean = mean(performance_stat_estim_list$Rsquared)
    rsquared_sd = sd(performance_stat_estim_list$Rsquared)
    
    # RMSE
    rmse_min = min(performance_stat_estim_list$RMSE_stat)
    rmse_max = max(performance_stat_estim_list$RMSE_stat)
    rmse_median = median(performance_stat_estim_list$RMSE_stat)
    rmse_iqr = IQR(performance_stat_estim_list$RMSE_stat, type = 8)
    rmse_first_quantile = quantile(performance_stat_estim_list$RMSE_stat)[c(2)]
    rmse_third_quantile = quantile(performance_stat_estim_list$RMSE_stat)[c(4)]
    rmse_mean = mean(performance_stat_estim_list$RMSE_stat)
    rmse_sd = sd(performance_stat_estim_list$RMSE_stat)
    
    # PBIAS_stat
    pbias_min = min(performance_stat_estim_list$PBIAS_stat)
    pbias_max = max(performance_stat_estim_list$PBIAS_stat)
    pbias_median = median(performance_stat_estim_list$PBIAS_stat)
    pbias_iqr = IQR(performance_stat_estim_list$PBIAS_stat, type = 8)
    pbias_first_quantile = quantile(performance_stat_estim_list$PBIAS_stat)[c(2)]
    pbias_third_quantile = quantile(performance_stat_estim_list$PBIAS_stat)[c(4)]
    pbias_mean = mean(performance_stat_estim_list$PBIAS_stat)
    pbias_sd = sd(performance_stat_estim_list$PBIAS_stat)
    
    # RSR_stat
    rsr_min = min(performance_stat_estim_list$RSR_stat)
    rsr_max = max(performance_stat_estim_list$RSR_stat)
    rsr_median = median(performance_stat_estim_list$RSR_stat)
    rsr_iqr = IQR(performance_stat_estim_list$RSR_stat, type = 8)
    rsr_first_quantile = quantile(performance_stat_estim_list$RSR_stat)[c(2)]
    rsr_third_quantile = quantile(performance_stat_estim_list$RSR_stat)[c(4)]
    rsr_mean = mean(performance_stat_estim_list$RSR_stat)
    rsr_sd = sd(performance_stat_estim_list$RSR_stat)
    
    performence_summary = data.frame(
        rsquared_median = rsquared_median,
        rsquared_iqr = rsquared_iqr,
        rsquared_first_quantile = rsquared_first_quantile,
        rsquared_third_quantile = rsquared_third_quantile,
        rsquared_mean = rsquared_mean,
        rsquared_sd = rsquared_sd,
        rsquared_min = rsquared_min,
        rsquared_max = rsquared_max,
        
        rmse_median = rmse_median,
        rmse_iqr = rmse_iqr,
        rmse_first_quantile = rmse_first_quantile,
        rmse_third_quantile = rmse_third_quantile,
        rmse_mean = rmse_mean,
        rmse_sd = rmse_sd,
        rmse_min = rmse_min,
        rmse_max = rmse_max,
        
        pbias_median = pbias_median,
        pbias_iqr = pbias_iqr,
        pbias_first_quantile = pbias_first_quantile,
        pbias_third_quantile = pbias_third_quantile,
        pbias_mean = pbias_mean,
        pbias_sd = pbias_sd,
        pbias_min = pbias_min,
        pbias_max = pbias_max,
        
        rsr_median = rsr_median,
        rsr_iqr = rsr_iqr,
        rsr_first_quantile = rsr_first_quantile,
        rsr_third_quantile = rsr_third_quantile,
        rsr_mean = rsr_mean,
        rsr_sd = rsr_sd,
        rsr_min = rsr_min,
        rsr_max = rsr_max
    ) # end of data_frame.
    
    return(performence_summary)
} # end of sumarize_performance_stat function


computeIntegralWeeklyCases <- function(dataFrame) {
    n_week = floor(nrow(dataFrame) / 7) # 7 is the number of days in a calendar week
    weekCase = rep(NA, n_week)
    i = 1
    while (i < nrow(dataFrame)) {
        for (j in 1:n_week) {
            weekCase[j] = with(dataFrame,integrate(approxfun(time,(
                newI
            )), time[i],time[(i + 6)]))$value
            i = i + 7
            if (i == 365) {
                break
            }
        }# for loop ends
    }# while loop ends
    return(weekCase) # return computed value of numbers of new cases per week.
}# end of function


sumCasesWeekly <- function(dataFrame) {
    n_week = floor(nrow(dataFrame) / 7) # 7 is the number of days in a calendar week
    weekCase = rep(NA, n_week)
    i = 1
    while (i < nrow(dataFrame)) {
        for (j in 1:n_week) {
            weekCase[j] = with(dataFrame, sum((newI[i:(i + 6)])))
            i = i + 7
            if (i == 365) {
                break
            }
        }# for loop ends
    }# while loop ends
    return(weekCase) # return computed value of numbers of new cases per week.
}# end of function.

## function for ploting prediction.

plotPrediction <- function(modelPred, variable = "newCases") {
    variable = as.character(variable)
    plot(
        modelPred[,"time"],round(modelPred[,variable]),type = "b",las = 1,ylab =
            "cases",xlab = "Week Number", xaxt = "n"
    )
    #axis(1,xaxp=c(1,year,4))
    Axis(
        x = range(seq(1:year)),at = seq(1,year,7),labels = seq(1,53,1),side = 1
    )
}

saveParEstimates <- function(parEstimMatrice,fileName = "") {
    if (exists(paste(fileName))) {
        save(parEstimMatrice,file = paste(fileName,"Rdata",sep = "."))
    }
}


to.pdf <- function(expr, filename, ..., verbose = TRUE) {
    if (verbose)
        cat(sprintf("Creating %s\n", filename))
    pdf(filename, ...)
    on.exit(dev.off())
    eval.parent(substitute(expr))
    
} # end of function

plot.var <-
    function(x,y,evry_xyear,lty = NULL) {
        # this function was used to plot model variables
        # when model returned daily values of variable and won't work for when model
        # return weekly values instead ( t_inc = week instead of t_inc = day)
        
        plot(
            x,y,type = 'l',xaxt = "n",xlab = "time (years)",ylab = "",main = "",yaxs =
                "r",xaxs = "r",las = 2,lty = lty
        )
        t_range.year <- seq(0,length(x),by = year)
        t_range.year = t_range.year[-1]
        evry_xyear = evry_xyear
        t_xaxis = seq(0,length(t_range.year),by = evry_xyear)
        t_xaxis = t_xaxis[-1]
        axis(1,at = c(0,t_range.year[t_xaxis]), labels = c(0,t_xaxis))
        
    } # end of function

plot.var_vs_t = function(var.name,x,eq_time = 50 * year,everyx = 5,lty =
                             NULL,per_pop = 1e+05) {
    # function plot variables
    # for the model when model return weekly values instead of daily values
    
    eq_time = eq_time
    everyx = everyx
    x = subset(x, time >= eq_time)
    var.name = var.name
    plot(
        x$time, x[,var.name] * per_pop,
        type = "l",
        lty = lty,
        xaxt = "n",
        ylab = "",
        main = "",
        xlab = "time (years)",
        #xaxs="i",
        #yaxs="i",
        las = 2
    )
    
    x_seq = seq(eq_time,x[dim(x)[1],1], everyx * year) # time sequence starting
    # from equilibrium time.
    axis(1, at = x_seq, labels = seq(0,length.out = length(x_seq),by = everyx))
}

bifur1D <- function(mbifur,range.x,range.y) {
    # 1-Dimention bifurcation diagram
    # function takes 2 arguments:
    # range.x is the range of the bifurcation parameter
    # range.y is the range of the vector containing the values of "I"s
    # at the start of each of the j last years of simulation
    mbifur = mbifur
    range.x = range.x
    range.y = range.y
    
    plot(
        0,0,pch = "",
        xlim = range.x,
        ylim = range.y, # I Can choose to plot log scale of I for clarity.
        main = "",
        sub  = "",
        xlab = "",
        ylab = "",
        title(ylab = "Annual I",line = 4.0),
        las = 1,
        cex.lab = 1
        
        #xlab=paste("Seasonality Amplitude,",param.name)
    ) ## end plot region
    ## start for loop for plotting the I's at the start of each year
    
    for (i in 1:length(param.seq)) {
        points(rep(param.seq[i], 10),
               mbifur[i,],cex = 0.0001) # plotting log(bifur_I) for clarity.
    } ## for loop end here
    
} # end of function


a0.harmonic <- function(a0,epsilon_a,teta) {
    # compute invasion rate at time t using sinusoidal function
    # takes 2 arguments:
    # the baseline disease invasion rate 'a0' and,
    # the amplitude of seasonality of a0 the invasion rate
    # if different than 0, the argument a0 must be in years not in days^-1
    
    year = year
    a0 = a0 / year
    if (a0 != 0) {
        t = seq(1,3 * year,1)
        a  = a0 * ((49.5 * epsilon_a) * cos(2 * pi * (t - teta) / year) +
                       (1 + 49.5 * epsilon_a))
        return(a)
        
    }else{
        return(
            cat(
                "a0 is the meningococcus baseline invasion rate\n and must be > 0. The current value of a0 is : ",a0
            )
        )
    }
}

at <- function(a0, epsilon_a,teta) {
    a_t = a0.harmonic(a0,epsilon_a,teta)
    range(a_t); (range(a_t)[2]) / range(a_t)[1]
    #cat("value range of a(t) is :",range(a_t), "which correspond to a ",range(a_t)[2]/range(a_t)[1], "fold-increase")
    at = list(a_t = a_t, range = range(a_t))
    return(at)
}

beta0.harmonic <- function(beta0,epsilon_b,teta) {
    year = year
    beta0 = beta0 / year
    if (beta0 != 0) {
        t = seq(1,3 * year,1)
        beta    = beta0 * (1 + epsilon_b * cos(2 * pi * (t - teta) / year))
        return(beta)
        
    }else{
        return(
            cat(
                "beta0 is the meningococcus mean transmission rate\n and must be > 0 .The current value of beta0 is : ",beta0
            )
        )
        
    }
}

betat <- function(beta0, epsilon_b,teta) {
    beta_t = beta0.harmonic(beta0,epsilon_b,teta)
    range(beta_t); (range(beta_t)[2]) / range(beta_t)[1]
    #cat("value range of beta(t) is :",range(beta_t), "which correspond to a ",range(beta_t)[2]/range(beta_t)[1], "fold-increase\n")
    betat = list(beta_t = beta_t, range = range(beta_t))
    return(betat)
}

incid_plot <- function(x, main = "", ylim = NULL, sub = "") {
    #fontion to plot lattice graph of health centers data for a given district
    xyplot(
        x * 1e+05,
        pch = 16,type = "b",ylim = ylim,main = main,cex = .7,col = "black",
        sub = sub,layout = c(3,3),ylab = "Weekly Incidence \n per 100,000",
        #group = c(group1,group2), col=c("red","blue"),
        panel = function(x,y,...) {
            panel.abline(h = 75, lty = "dotted", col = "black")
            
            panel.xyplot(x,y,...)
        }
    )
}

# defining graphical parameters of the districts.

main.title <-
    function(district.name = "------") {
        #  argument must be of character class
        titre = paste(
            "Weekly incidence/100,000 \n Sanitary District of",
            district.name, "(Burkina-Faso) \n Dotted line represent treshold for localized outbreak"
        )
        titre
    }

sub.title <-
    function(district.name = "------") {
        #  argument must be of character class
        titre = paste("Health Centers in the sanitary District of",
                      district.name, "(Burkina-Faso)")
        titre
    }

# Function to plot the mean weekly incidence data and  the 95% CI at health centre level
# takes dataframe as argument
plot.mean.weekly <- function(df,sub = "",title = "") {
    title = title
    sub = sub # subtitle of the plot
    toto = ci_95(df) # output mean as well as 95% CI around the mean
    max.y = max(toto[,"hbound"][which(!is.nan(toto[,"hbound"]))])
    min.y = max(toto[,"lbound"][which(!is.nan(toto[,"lbound"]))])
    plot(
        time(df), row.mean(df) * 1e+05,type = "n",lwd = 2,xlab = "",ylab = "Average Weekly \n Incidence",las =
            1, ylim = c(min.y,1.2 * (max.y * 1e+05))
    )
    num_date = as.numeric(time(df))
    #polygon(c(rev(num_date), num_date), c(rev(toto[,"hbound"]*1e+05), toto[,"lbound"]*1e+05), col = 'grey75', border = NA)
    lines(
        time(df), toto[,2] * 1e+05,col = "black",lwd = 2,lty = 1
    )
    lines(
        time(df), toto[,"hbound"] * 1e+05,col = 2,lwd = 2,lty = 3
    )
    lines(
        time(df), toto[,"lbound"] * 1e+05,col = 3,lwd = 2,lty = 3
    )
    # shading the 95% ci area,
    
    mtext(expression(paste("x",10 ^ -5)),side = 3,adj = 0, cex = 0.8)
    title(main = title,sub = sub,cex.main = 1)
    legend(
        "topright",legend = c("Average","Upper 95% CI","Lower 95% CI"),col = c(1,2,3),lty =
            c(1,3,3),lwd = 3,bty = "n",cex = 0.7
    )
    #legend("topright",legend=c("Average","95% CI"),col=c("black","grey75"),lty=c(1),lwd=3,bty="n",cex=0.7)
}

multi_ggplot <-
    function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
        # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot   objects)
        # - cols:   Number of columns in layout
        # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
        #
        # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
        # then plot 1 will go in the upper left, 2 will go in the upper right, and
        # 3 will go all the way across the bottom.
        #
        library(grid)
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <-
                matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols, nrow = ceiling(numPlots / cols))
        }
        
        if (numPlots == 1) {
            print(plots[[1]])
            
        } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
                # Get the i,j matrix positions of the regions that contain this subplot
                matchidx <-
                    as.data.frame(which(layout == i, arr.ind = TRUE))
                
                print(
                    plots[[i]], vp = viewport(
                        layout.pos.row = matchidx$row,
                        layout.pos.col = matchidx$col
                    )
                )
            }
        }
    }


# function for identifying district name from param estimates
# matrice

get_district_name <- function(paramEstimate_matrix) {
    if (grepl("GOURCY|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "GOURCY"
    }else if (grepl("ORODARA|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "ORODARA"
    }else if (grepl("SEGUENEGA|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "SEGUENEGA"
    }else if (grepl("LENA|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "LENA"
    }else if (grepl("KARANGASSO VIGUE|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "KARANGASSO VIGUE"
    }else if (grepl("HOUNDÉ|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "HOUNDÉ"
    }else if (grepl("DAFRA|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "DAFRA"
    }else if (grepl("BOULSA|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "BOULSA"
    }else if (grepl("DANDE|",rownames(paramEstimate_matrix)[1],fixed = TRUE)) {
        district_name = "DANDE"
    }
    return(district_name)
}

### New functions since revision of the modelling paper manuscript
### 
# file containing experimental function to be moved to myfunction.R
# file in their final useful version.
# 
# A ggplot function to plot age structured model simulation output.
## ggplot options:
# A ggplot function to plot age structured model simulation output.
## ggplot options:
#library(ggplot2)

theme_opts = theme(
    axis.text = element_text(size = 14),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white"),
    legend.position = c("right"),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
    #panel.grid.major = element_line(colour = "grey40"),
    #panel.grid.minor = element_blank()
)

# a fonction to prepare model output data.frame for use with the ggplot function.
melt_any_data.frame = function(data.frame, x_axis_variable, yaxis_variables) {
    data.frame.subset = subset(data.frame, select = c(x_axis_variable, yaxis_variables))
    melt_out <-
        melt(
            data = data.frame.subset, id = x_axis_variable, variable.name = "Variable", value.name = "Value"
        )
    return(melt_out)
}

# for ploting line graph, especially melt time series data frames
# such as model outputs or incidence curves.
# all arguments expect the data must correspond to a column name in the melt data_frame

ggplot_line_graph <- function(melt_data, x_colname , value_colname  , variable_colname){
    # all arguments expect the data must correspond to a column name in the melt data_frame
    time = as.symbol(x_colname)
    value = as.symbol(value_colname)
    variable = as.symbol(variable_colname)
    p <- qplot(x = eval(time), y = eval(value), data = melt_data, geom = "line", 
               linetype = eval(variable))
    p = p + theme_bw()
    p = p + theme_opts # My custom options as stored in the variable theme_opts
    #p+scale_color_manual(name = variable_colname, values = c(1:4))
    p = p + guides(linetype = guide_legend(title = variable_colname))
    p 
    update_labels(p, labels = list(x = x_colname, y = value_colname))
    
} # end function.

ggplot_costum_x_axis<-function(melt_data, time_step, tick_label_evry){
    axis_ticks = round(seq(0, max(melt_data[,1]), time_step))
    ticks_label = index(axis_ticks)-1
    if(time_step>year/52){
    scale_x_continuous(
        breaks = axis_ticks*tick_label_evry, labels = ticks_label*tick_label_evry, name = "Time (Years)")
        }else{
            scale_x_continuous(
                breaks = axis_ticks*tick_label_evry, labels = ticks_label*tick_label_evry, name = "Time (Calendar weeks)")
        } #end if
} # end function ggplot_costum_x_axis

# costum poportion or percentage barchart with ggplot.
gg_prop <- function(data = data.frame()){ggplot(data = data
                                                , aes(x = `Age group`, y = Population)) + 
        geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3,
                 fill="white", colour="black", width = .5) +  
        scale_y_continuous(labels = percent) +    
        scale_fill_few('medium', drop = FALSE) +  theme_bw() +            # keep levels, if data is filtered
        labs(x = 'Age groups', y = NULL, fill = 'Legend'
             , title = 'Age distribution of Burkina Faso')
}

#=================================
# a function to plot model simulation output after subseting and metling
# the simulation output. It is a wrapper for my ggplot_line_graph function

ggplot_sim_output<-function(sim_output_melt, 
                            time_step = year,
                            tick_label_evry=1){
    p<-ggplot_line_graph(sim_output_melt, "time", value_colname = "Value", variable_colname = "Variable")
    p <- p + ggplot_costum_x_axis(sim_output_melt, time_step = time_step, tick_label_evry = tick_label_evry)
    p <- p + labs(title="Number of individuals over time")
    p <- p + scale_y_continuous(name="Counts")
    return(p)
} 

#==
# define a function to append objects to an existing list.
lappend <- function (lst, ...){
    lst <- c(lst, list(...))
    return(lst)
}
# define a function to find out health centers in each district.
list_district_health_centers<-function(data.frame=NULL){
    district_health_centers_list = list()
    districts.names<-as.vector(districts.names)
    for(i in seq_along(districts.names)){
        data_subset = subset(data.frame, district==districts.names[i])
        health_centers = unique(data_subset$fs)
        district_health_centers_list <- lappend(district_health_centers_list,health_centers)
    }
    names(district_health_centers_list)<-districts.names
    return (district_health_centers_list)
}


# A function to sum age-specific estimates of the model variables by row to get total estimates

add_row_totals_per_model_variable<-function(age_structure_model_output){
  age_structure_model_output$total_Susc<- rowSums(age_structure_model_output[,c("Susc1",  "Susc2" ,    "Susc3" ,   "Susc4")])
  age_structure_model_output$total_Carrier<- rowSums(age_structure_model_output[,c("Carrier1",  "Carrier2" ,    "Carrier3" ,   "Carrier4")])
  age_structure_model_output$total_Ill<- rowSums(age_structure_model_output[,c("Ill1",  "Ill2" ,    "Ill3" ,   "Ill4")])
  age_structure_model_output$total_newI<- rowSums(age_structure_model_output[,c("newI1",  "newI2" ,    "newI3" ,   "newI4")])
  age_structure_model_output$total_Recov<- rowSums(age_structure_model_output[,c("Recov1",  "Recov2" ,    "Recov3" ,   "Recov4")])
  return(age_structure_model_output)
}



# remoove first and last row of data which are NAs
non_missing_data<-function(data){
  # replace NA at the begining and end of the data time series by 0
  data = as.data.frame(data)
  data[is.na(data)] <- 0
  #non_missing_data=data.frame[complete.cases(data.frame)]
  return(data)
}


# na.zero <- function (x) {
#   x[is.na(coredata(x))] <- 0
#   return(x)
# }


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

population_size_per_age_grp = function(population_data_per_age) {
        # function to get population age stats in a given country in a given year, 
        # then aggregate it according to age groups of interest
        population_data_per_age$`Age class` <- NA
        population_data_per_age[c("0":"4") + 1,]$`Age class` <-
                "[0-4]"
        population_data_per_age[c("5":"12") + 1,]$`Age class` <-
                "[5-12]"
        population_data_per_age[c("13":"19") + 1,]$`Age class` <-
                "[13-19]"
        population_data_per_age[c(21:89),]$`Age class` <- "20+"
        
        data_by_age_group = aggregate(
                population_data_per_age[,c(2:4)], by = list("Age group" = population_data_per_age$`Age class`), sum
        )
        
        data_by_age_group$`Both sex proportion` <-
                (data_by_age_group$`Both sex`) / sum(data_by_age_group$`Both sex`)
        
        data_by_age_group$`Male proportion` <-
                (data_by_age_group$Male) / sum(data_by_age_group$`Both sex`)
        
        data_by_age_group$`Female proportion` <-
                (data_by_age_group$Female) / sum(data_by_age_group$`Both sex`)
        
        age_group_data_melt = melt(data_by_age_group, value.name = "Population", id.vars = "Age group")
        
        return(
                list(
                        data_by_age_group = data_by_age_group, age_group_data_melt = age_group_data_melt
                )
        )
}

# function to get the proportion of male, female or total proportion of both sex per age groups
get_age_group_fraction = function(data_by_age_group, colname){
        return(data_by_age_group[, colname])
}

get_age_grp_proportion = function(country = "UV", year_now){
        # get the proportion of each of the defined age class in the general population
        # of Burkina-faso
        year_spec_age_data = get_population_age_stat_data(country,year_now)
        year_spec_age_group_data = population_size_per_age_grp(year_spec_age_data)
        get_age_grp = year_spec_age_group_data$data_by_age_group
        age_group_fraction = get_age_group_fraction(get_age_grp, "Both sex proportion")
        names(age_group_fraction)<- c("<5 years","5-12 years","13-19 years", "20+ years")
        return(age_group_fraction)
}

#==========================================================
# for mle estimates matrix

mle_computeModelPredictionAndPerformStat <-
  function(paramEstimatesMatrice) {
    # A function to compute model prediction based on best fitted parameters
    # estimate obtain from optimisation routine it also compute some summary
    # estimates and statistics from the model predictions based on
    # optimized parameters.
    # @ parameters : takes the matrix of estimated parameters (which
    # contain estimated parameters values for each health center-year)
    peakWeeklyIncid <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    peakWeeklyCarriagePrev <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    PredictedIncidList <- list()
    PredictedCarriageList <- list()
    foldInc <- list()
    incidRange <-
      matrix(
        NA,
        nrow = nrow(paramEstimatesMatrice),
        ncol = 2,
        byrow = T,
        dimnames = list(c(1:dim(
          paramEstimatesMatrice
        )[1]), c("Incidmin", "Incidmax"))
      )
    carriageRange <-
      matrix(
        NA,
        nrow = nrow(paramEstimatesMatrice),
        ncol = 2,
        byrow = T,
        dimnames = list(c(1:dim(
          paramEstimatesMatrice
        )[1]), c("Carriagemin", "Carriagemax"))
      )
    
    # vectors for storing the computed values of model performence statistics
    
    RMSE_stat <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    RSR_stat <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    PBIAS_stat <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    Rsquared <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    a_fold <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    beta_fold <-
      vector(mode = "numeric", length = nrow(paramEstimatesMatrice))
    
    current_parms_combination = vparameters
    
    for (i in seq_along(paramEstimatesMatrice[, "district"])) {
      current_parms_combination["beta0"] = paramEstimatesMatrice[i, "beta0"]
      current_parms_combination["alpha"] = paramEstimatesMatrice[i, "alpha"]
      current_parms_combination["phi"] = paramEstimatesMatrice[i, "phi"]
      current_parms_combination["Susc0"] = paramEstimatesMatrice[i, "Susc0"]
      current_parms_combination["CarrierProp"] = paramEstimatesMatrice[i, "CarrierProp"]
      current_parms_combination["teta"] = paramEstimatesMatrice[i, "teta"]
      current_parms_combination["a0"] = paramEstimatesMatrice[i, "a0"]
      
      if (any("epsilon_a" %in% colnames(paramEstimatesMatrice)) &
          !any("epsilon_b" %in% colnames(paramEstimatesMatrice))) {
        current_parms_combination["epsilon_a"] = paramEstimatesMatrice[i, "epsilon_a"]
        ## comute fold increase of the invasion parameter.
        range_a = range(
          at(
            current_parms_combination["a0"] * year,
            current_parms_combination["epsilon_a"],
            current_parms_combination["teta"]
          )
        )
        range_b = NA
      }
      else if (any("epsilon_b" %in% colnames(paramEstimatesMatrice)) &
               !any("epsilon_a" %in% colnames(paramEstimatesMatrice))) {
        current_parms_combination["epsilon_b"] = paramEstimatesMatrice[i, "epsilon_b"]
        ## comute fold increase of transmission parameter.
        range_b = range(
          betat(
            current_parms_combination["beta0"] * year,
            current_parms_combination["epsilon_b"],
            current_parms_combination["teta"]
          )
        )
        range_a = NA
      } else{
        current_parms_combination["epsilon_a"] = paramEstimatesMatrice[i, "epsilon_a"]
        current_parms_combination["epsilon_b"] = paramEstimatesMatrice[i, "epsilon_b"]
        range_a = range(
          at(
            current_parms_combination["a0"] * year,
            current_parms_combination["epsilon_a"],
            current_parms_combination["teta"]
          )
        )
        range_b = range(
          betat(
            current_parms_combination["beta0"] * year,
            current_parms_combination["epsilon_b"],
            current_parms_combination["teta"]
          )
        )
      }
      
      # multiply beta0*year in range_a and range_b
      # because the betat function take beta0 argument in per year unit not per day unit.
      ## see beta0.harmonic function used inside betat function for details. Same explanation goes
      ## for why i used a0*year.
      
      #select data corresponding
      year_now = paramEstimatesMatrice[i, "year"]
      district_now = paramEstimatesMatrice[i, "district"]
      # get AICc and BIC for current health center
      AICc_selected_hc<- round(paramEstimatesMatrice[i, "AICc"])
      BIC_selected_hc<- round(paramEstimatesMatrice[i, "BIC"])
      
      
      # may need to moove this function from the local scope of
      # the computeModelPredictionAndPerformStat function to
      # a general scope so that it can be extended without breaking
      # this function code as mor health center year data becomes availaible.
      select_data <- function(year_now, district_now) {
        if (year_now == 2006 & district_now == 1) {
          data = seguen_2006
          N = populations_in_this_district(seguen_2006, "2006")
        } else if (year_now == 2007 & district_now == 1) {
          data = seguen_2007
          N = populations_in_this_district(seguen_2007, "2007")
        } else if (year_now == 2008 & district_now == 1) {
          data = seguen_2008
          N = populations_in_this_district(seguen_2008, "2008")
        } else if (year_now == 2009 & district_now == 1) {
          data = seguen_2009
          N = populations_in_this_district(seguen_2009, "2009")
        } else if (year_now == 2010 & district_now == 1) {
          data = seguen_2010
          N = populations_in_this_district(seguen_2010, "2010")
        } else if (year_now == 2004 & district_now == 2) {
          data = hounde_2004
          N = populations_in_this_district(hounde_2004, "2004")
        } else if (year_now == 2005 & district_now == 2) {
          data = hounde_2005
          N = populations_in_this_district(hounde_2005, "2005")
        } else if (year_now == 2006 & district_now == 2) {
          data = hounde_2006
          N = populations_in_this_district(hounde_2006, "2006")
        } else if (year_now == 2007 & district_now == 2) {
          data = hounde_2007
          N = populations_in_this_district(hounde_2007, "2007")
        } else if (year_now == 2008 & district_now == 2) {
          data = hounde_2008
          N = populations_in_this_district(hounde_2008, "2008")
        } else if (year_now == 2009 & district_now == 2) {
          data = hounde_2009
          N = populations_in_this_district(hounde_2009, "2009")
        } else if (year_now == 2010 & district_now == 2) {
          data = hounde_2010
          N = populations_in_this_district(hounde_2010, "2010")
        } else if (year_now == 2004 & district_now == 3) {
          data = lena_2004
          N = populations_in_this_district(lena_2004, "2004")
        } else if (year_now == 2005 & district_now == 3) {
          data = lena_2005
          N = populations_in_this_district(lena_2005, "2005")
        } else if (year_now == 2006 & district_now == 3) {
          data = lena_2006
          N = populations_in_this_district(lena_2006, "2006")
        } else if (year_now == 2007 & district_now == 3) {
          data = lena_2007
          N = populations_in_this_district(lena_2007, "2007")
        } else if (year_now == 2008 & district_now == 3) {
          data = lena_2008
          N = populations_in_this_district(lena_2008, "2008")
        } else if (year_now == 2009 & district_now == 3) {
          data = lena_2009
          N = populations_in_this_district(lena_2009, "2009")
        } else if (year_now == 2010 & district_now == 3) {
          data = lena_2010
          N = populations_in_this_district(lena_2010, "2010")
        } else if (year_now == 2008 & district_now == 4) {
          data = Kvigue_2008
          N = populations_in_this_district(Kvigue_2008, "2008")
        } else if (year_now == 2009 & district_now == 4) {
          data = Kvigue_2009
          N = populations_in_this_district(Kvigue_2009, "2009")
        } else if (year_now == 2010 & district_now == 4) {
          data = Kvigue_2010
          N = populations_in_this_district(Kvigue_2009, "2009")
        }
        return (list(data=data, N=unlist(N)))
      } # ends select data function
      
      data = select_data(year_now, district_now)
      current_hc_index = paramEstimatesMatrice[i, "hc"] # select the index of current health center
      
      selected_hc_data = data$data[, current_hc_index]
      coredata = coredata(selected_hc_data)
      
      
      # Compute model predictions based on the estimated parameters combination.
      nbYearSimulated = 1
      modelPredict <-
        sim.SCIRS_harmonic(inits, current_parms_combination, nd = nbYearSimulated *
                             year)
      modelPredict = tail(modelPredict, length(coredata))
      modelPredict <-
        subset(modelPredict, select = c(time, Susc, Carrier, Ill, Recov, newI))
      
      modelPredict[, c(2:6)]<- modelPredict[, c(2:6)]/as.numeric(data$N[current_hc_index]) # devide the counts by the appropriate population size
      carriageVector = modelPredict$Carrier # variable to plot later
      carriageVector = sapply(carriageVector, replace_negligeable_negative_values)
      
      
      
      # check if ineq_constrain on carriage worked
      #fold_change_prev_week_6_8_and_44_46 = mean(c(carriageVector[44:46])) / mean(c(carriageVector[8:6]))
      
      # COMPUTING SOMME PERFORMANCE STATISTIQUES
      
      ## function to compute percent bias and SRS statistic
      
      computeModelPerformanceStats <-
        function(selectedhcData, PredictedNewI) {
          # 1st compute residuals between model predictions and data
          residuals = PredictedNewI - selectedhcData
          
          #print(residuals)
          
          #We'll also need the number of observation data points
          
          ## nDataPoints = length (selectedhcData); ## This will underestimate the final estimate as it doesn't remoove missing data
          
          nDataPoints = length(residuals[!is.na(residuals) == TRUE]) # this is for handling missing data which might create problem in division and sqrt
          
          # To compute the RMSE
          sumOfSquaredREsiduals = sum(residuals * residuals, na.rm = TRUE) # also called sum of squared errors
          RMSE = sqrt(sumOfSquaredREsiduals / nDataPoints)
          # single value returned
          
          #compute the RSR statistic
          RSR = RMSE / (sd(selectedhcData, na.rm = TRUE)) # retuen a single value
          
          #I would interpret bias (systematic error) as
          #the average residual, which should be zero for random errors, i.e.
          #"noise"
          
          ## PBIAS = 100 * [ sum( sim - obs ) / sum( obs ) ]
          
          PBIAS = pbias(PredictedNewI, selectedhcData, na.rm = TRUE)  # compute the percent bias based on the two specified vector of data and predictions
          
          #computing the R-squared coef
          meanObsData = mean(selectedhcData, na.rm = TRUE)
          totalSumOfSquares = sum((selectedhcData - meanObsData) ^ 2, na.rm =
                                    TRUE)
          #sumOfSquaredError = sumOfSquaredREsiduals
          Rsquared = 1 - (sumOfSquaredREsiduals / totalSumOfSquares)
          
          ModelPerformanceStatsVector = c(RMSE, RSR, PBIAS, Rsquared)
          return (ModelPerformanceStatsVector)
        } # ends computeModelPerformanceStats function
      
      ModelPerformanceStats = computeModelPerformanceStats(coredata, modelPredict$newI) # compute the actual performence stats
      
      ## store the computed stats in their respective vector.
      RMSE_stat[i] <-
        ModelPerformanceStats[1] # store first value in the vector
      RSR_stat[i] <- ModelPerformanceStats[2]
      PBIAS_stat[i] <- ModelPerformanceStats[3]
      Rsquared[i] <- ModelPerformanceStats[4]
      
      peakWeeklyIncid[i] <- max(modelPredict$newI)
      peakWeeklyCarriagePrev[i] <- max(modelPredict$Carrier)
      
      PredictedIncidList[[i]] <- modelPredict$newI
      PredictedCarriageList[[i]] <- modelPredict$Carrier
      
      incidRange[i, ] <-
        c(range(modelPredict$newI)[1], range(modelPredict$newI)[2])
      carriageRange[i, ] <-
        c(range(modelPredict$Carrier)[1],
          range(modelPredict$Carrier)[2])
      
      
      
      ####################################################################################
      #plot observed data against prediction
      ####################################################################################
      ## find max value in the observed time serie.
      #na.data.index = which(is.na(coredata))
      
      # selecting health center name to add to plot
      hc_name = names(data$data)[current_hc_index]
      
      
      peak_value_data = max(coredata, na.rm = TRUE)
      
      
      if (max(modelPredict$newI) > peak_value_data) {
        peak_value = max(modelPredict$newI)
      } else{
        peak_value = peak_value_data
      }
      
      
      
      plotOnlyPositiveRsquared = function(doPlot) {
        if (doPlot == TRUE) {
          hc_with_very_poor_fit = Rsquared[i] < 0
          doPlot = !hc_with_very_poor_fit
        } else{
          doPlot = !doPlot
        }
        return(doPlot)
      }
      
      doPlot = plotOnlyPositiveRsquared(FALSE)
      if (doPlot) {
        # Plot observed data
        plot(
          coredata * 1e+05,
          las = 1,
          pch = 19,
          col = "black",
          type = "p",
          ylab = "",
          xlab = "",
          ylim = c(0, 1.1 * peak_value * 1e+05)
        )
        # plot model incidence predictions
        lines(
          modelPredict$newI * 1e+05,
          col = "black",
          type = "l",
          pch = 20,
          lwd = 3
        )
        
        title(
          main = paste(hc_name),
          xlab = paste("Calendar Weeks", year_now),
          ylab = "Incidence per 100,000"
        )
        
        # adding the plot of model predictions of carriers to the previous plot
        addCarriagePlot = function() {
          par(new = T)
          carriagePrev = carriageVector * 1e+02
          plot(
            carriagePrev,
            col = "azure4",
            type = "l",
            lwd = 2,
            lty = 2,
            axes = FALSE,
            bty = "n",
            xlab = "",
            ylab = ""
          )
          axis(
            side = 4,
            at = pretty(range(carriagePrev)),
            col.axis = "azure4",
            las = 1
          )
          mtext(
            "Carriers (%)",
            side = 4,
            line = 2.4,
            cex = 0.7,
            col = "azure4"
          )
          
          legend(
            "topright", #inset = 0.1, seg.len = 5, x.intersp= 2,  y.intersp= 1.5,
            legend = c("Data","Model","C"),
            lty = c(0,1,2), pch = c(19, NA, NA),
            lwd = c(NA,2,2),
            col = c("black","black","azure4"),
            bty = "n",bg = "transparent",horiz = F, merge = TRUE
            # "topright",
            # legend = c("Data", "Cases", "Carriers"),
            # lty = c(0, 1, 2),
            # pch = c(20, NA, NA),
            # lwd = c(NA, 3, 3),
            # col = c("black", "black", "azure4"),
            # bty = "n",
            # bg = "transparent",
            # horiz = FALSE,
            # merge =
            #   TRUE
          ) #horiz = TRUE for horizontal legend
        } # end addCarriagePlot function
        ## IMPORTANT !! CALLING THE addCarriagePlot function
        addCarriagePlot() # call the function
        
        #add fold increase in invasion and or transmission to plot
        addFoldIncrease = function(range_a = NA,
                                   range_b = NA,
                                   Rsquared = NULL,
                                   AICc_selected_hc) {
          a_fold = round(range_a[2] / range_a[1], 1)
          beta_fold = round(range_b[2] / range_b[1], 1)
          Rsquared = round(Rsquared * 100)
          #AICc_selected_hc = AICc_selected_hc
          if (is.na(beta_fold)) {
            title(
              sub = bquote(paste(
                a[0], '-fold = ', .(a_fold),
                
                "; ", AIC[c], ' = ', .(AICc_selected_hc)
              )),
              col.sub = "black",
              cex.sub = 1,
              line = 3.6
              
            )
          } else if (is.na(a_fold)) {
            title(
              sub = bquote(paste(
                beta[0],
                '-fold = ',
                .(beta_fold),
                "; ",
                AIC[c],
                ' = ',
                .(AICc_selected_hc)
              )),
              col.sub = "black",
              cex.sub = 1,
              line = 3.5
              
            )
          } else{
            title(
              sub = bquote(
                paste(
                  a[0],
                  '-fold = ',
                  .(a_fold),
                  "; ",
                  beta[0],
                  '-fold = ',
                  .(beta_fold),
                  "; ",
                  AIC[c],
                  ' = ',
                  .(AICc_selected_hc)
                )
              ),
              col.sub = "black",
              cex.sub = 1,
              line = 3.5
              
            )
          }
        }
        
        addFoldIncrease(range_a = range_a, range_b= range_b, AICc_selected_hc = AICc_selected_hc) # add fold increase to the plot
      }# end if(!hc_with_very_poor_fit) statement
      a_fold[i] <- round(range_a[2] / range_a[1], 1)
      beta_fold[i] <- round(range_b[2] / range_b[1], 1)
    } # end for loop
    #dev.off() # end of printing plots to pdf.
    
    return(
      list(
        a_fold = a_fold,
        beta_fold = beta_fold,
        peakWeeklyIncid = peakWeeklyIncid,
        peakWeeklyCarriagePrev =
          peakWeeklyCarriagePrev,
        PredictedIncidList = PredictedIncidList,
        PredictedCarriageList =
          PredictedCarriageList,
        incidRange = incidRange,
        carriageRange = carriageRange,
        RMSE_stat =
          RMSE_stat,
        RSR_stat = RSR_stat,
        PBIAS_stat = PBIAS_stat,
        Rsquared = Rsquared,
        AICc_selected_hc = AICc_selected_hc,
        BIC_selected_hc = BIC_selected_hc
      )
    )
  } # computeModelPrediction function
####################################################

# placing legend above graph
#legend("topright",
#inset = c(0, -0.2),  x.intersp=0.1, y.intersp=-.5,bty="n",
#xjust=0,yjust=0,lty=c(1,2,3),col=c("black","black","black"),
#lwd=2, cex = 0.75, xpd = TRUE, horiz=T,
#legend=c(expression(epsilon[a]==1,epsilon[a]==0,epsilon[a]==0.11)))
