##################################################################################
##################################################################################
# an R script to fit the parameters of the SCIRS model to to meningitis weekly epidemic
# data by minimizing the Least Squares statistic
#
# Author:  Thibaut koutangni
#          koutibaut@yahoo.fr
# Created: Feb  23, 2015
#
# This script is not guaranteed to be free of bugs and/or errors
#
##################################################################################
require("chron")
par(pch=20)  # the dot style for the plots
source("R/SCIRS_harmonic_CI.R")

##################################################################################
# The Burkina Faso data is weekly, with the date of each point corresponding to the
# date of the end of the week over which the data were collected.
# converting these dates to time in days, relative to Jan 1, 2007
# I will be using this vector of dates, vday, to obtain the model estimates
# of the incidence at that time.
##################################################################################


##################################################################################
# first way to compute Julian days since December 29, 2003

seguenega_hyperendemic_2007_2011$julian_day= julian(x=time(seguenega_hyperendemic_2007_2011), origin=as.Date("2003-12-29"))
#vday = append(min(seguenega_hyperendemic_2007_2011$julian_day)-7,seguenega_hyperendemic_2007_2011$julian_day)
vday = c(coredata(seguenega_hyperendemic_2007_2011$julian_day))


# second way to compute Julian days since December 29, 2003 (using number of weeks since January 1st 2004 )
#seguenega_hyperendemic_2007_2011$julian_day=julian(as.Date("2004-01-05"))+(coredata(seguenega_hyperendemic_2007_2011$week_num)-1)*7-julian(as.Date("2003-12-29"))
kalsaka_ts = seguenega_hyperendemic_2007_2011[,c("csps kalsaka","week_num","julian_day")]

data_2007_2010 = coredata(subset(kalsaka_ts[,"csps kalsaka"],year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010))
vday_2007_2010 = coredata(subset(kalsaka_ts[,3],year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010))
replace_missing = mean(data_2007_2010[which(is.na(data_2007_2010))[1]-1],data_2007_2010[which(is.na(data_2007_2010))[1]+1])
data_2007_2010[which(is.na(data_2007_2010))]=replace_missing

#weekly_kalsaka_data = coredata(seguenega_hyperendemic_2007_2011[,"csps kalsaka"])
# replacing the two missing values of the data verctor with mean value of the week before,
# and after them respectively.
#replace_missing = mean(weekly_kalsaka_data[which(is.na(weekly_kalsaka_data))[1]-1],weekly_kalsaka_data[which(is.na(weekly_kalsaka_data))[1]+1])
#weekly_kalsaka_data[which(is.na(weekly_kalsaka_data))] = replace_missing
#########################################################################################
# now using the vday vector, we can find  model predicted weekly incidence that matches
# available incidence data point.

# modelA_predict$time%in%vday returns the indices of the elements of modelA_predict$time that
# are also found in vday
# weekly_incidence = modelA_predict$newI.Carrier[modelA_predict$time%in%vday]
########################################################################################

##################################################################################
# now overlaying the model on the data, and calculating the least-squares
# statistic that compares the data to this model calculated
# under differente hypothesis value and combinations of the unknown parameter values.
##################################################################################


##################################################################################
# set up the model parameters
##################################################################################
# model parameters already defined in the appropriate file.


##################################################################################
# iterate a whole bunch of times and sample a different value of beta0 and alpha and phi
# at each iteration.  Compare the predicted weekly incidence to the observed data, and obtain
# the least squares statistic.  Put the information
# into vectors, to enable a nice summary plot at the end.
##################################################################################
beta0 = numeric(0)
alpha0 = numeric(0)
phi = numeric(0)
epsilon_a = numeric(0)

vbest_leastsq_fit = numeric(0) # this will contain the model estimate of the best least squares fit

vleastsq = numeric(0) # this will containt the least-squared statistic calculated for the beta0, alpha and phi hypotheses

vbeta0 = numeric(0)
valpha = numeric(0)
vphi = numeric(0)
vepsilon_a = numeric(0)

amin_least = 1e10 # this will be used to check if each model is a better least squares fit than obtained from other beta0, alpha and phi hypotheses.

niter = 30000  # number of different beta0, alpha and phi hypotheses we should test
for (iter in 1:niter){
    
    if (iter%%100==0) cat("Doing iteration ",iter," out of ",niter,"\n")  # inform user the script is doing something, and not hung
    
    beta0 = runif(n=1, min=50/year,max=200/year) # randomly sample beta0 uniformly 
    alpha = runif(n=1,min=1/year,max=52/year)    # randomly sample alpha uniformly 
    phi = runif(n=1,min=0.2/year,max=52/year)    # randomly sample phi uniformly
    epsilon_a = runif(n=1, min=0, max=1)         # randomly sample phi uniformly
    
    #alpha = as.integer(alpha = runif(n=1,min=1/year,max=52/year)) # randomly sample alpha uniformly 
    # (use of integer hypothesis values of immunity duration)
    
    
    # knowledge about the distribution of beta0 we may sample close to a mean hypothesis value
    #beta0 = rnorm(n=1, mean=50/year, sd = ??) # preferential sampling close to a certain value
    
    
    ##################################################################################
    # set up the parameter vector and initial values, then solve the SCIRS
    # system of equations numerically with lsoda in the deSolve package.  Put
    # the results in modelA_predict
    ##################################################################################
    parms=c(  # vector of parameters to estimate
            beta0 = beta0,
            alpha = alpha,
            phi = phi,
            epsilon_a = epsilon_a
        )
    
    print(parms,sep=" ")
    
    modelA_parms=PARMS_SCIRS_f # Vector with model parameters (fixed param and param to estimate)
    for(i in 1:length(names(parms))){ # using the current combination of hypothesis values with fixed parmaeters values
        modelA_parms[[names(parms)[i]]]=parms[[i]]
    }
    
    #print(cbind(modelA_parms)) # print the current parameters combination being evaluated by optim
    # run model for the the current parameter combination
    modelA_predict = sim.SCIRS_harmonic(inits,modelA_parms,nd=10*year)
    
    weekly_incidence = modelA_predict$newI.Carrier[modelA_predict$time%in%vday_2007_2010]
        
    data = data_2007_2010
    # assuming cases notification data are not exaustive, we can compute the fraction of case
    # that are actually notified then apply that fraction to model estimate of incidence so that
    # area under the model curve equals the sum of the notified cases data incidence. 
    frac_notified = sum(data)/sum(weekly_incidence) 
    weekly_incidence = weekly_incidence*frac_notified # normalize the model prediction so area under curve
    # equals the sum of the data incidence
    #weekly_incidence = modelA_predict$newI.Carrier
    
    
    ##################################################################################
    # now let's overlay the model on the data, and calculate the least-squares
    # statistic that compares the data to this model calculated
    # under a particular hypothesis of parameters being estimated
    ##################################################################################
   
    if (length(weekly_incidence)>1&!is.na(sum(weekly_incidence))){
        if (min(weekly_incidence)>=0&length(weekly_incidence)==length(data)){
            
            ######################################################################## 
            # calculate the least squares statistic
            ######################################################################## 
            least_squares = sum((weekly_incidence-data)^2)/mean(weekly_incidence)
            #least_squares = sum((weekly_incidence-data)^2)
            
            
            vbeta0 = append(vbeta0,beta0)
            valpha = append(valpha,alpha)
            vphi = append(vphi,phi)
            vepsilon_a = append(vepsilon_a,epsilon_a)
            vleastsq = append(vleastsq,least_squares)
            
            if (least_squares<amin_least){
                amin_least = least_squares
                vbest_leastsq_fit = weekly_incidence
                if (length(vbeta0)>100){
                    ######################################################################## 
                    # plot the results for the least squares fit
                    ######################################################################## 
                    par(mfrow=c(2,2)) # set the plotting area to one plot per page
                    amax = 100 # cut off for the least squares for the plot
                    plot(vbeta0[vleastsq<amax],vleastsq[vleastsq<amax],xlab="beta0 hypothesis",ylab="Least squares goodness of fit statistic")
                    points(vbeta0[vleastsq==min(vleastsq)],vleastsq[vleastsq==min(vleastsq)],col=4,cex=1.5) # indicate the best-fit value on the plot
                    legend("topright",legend=c("best fit value"),col=c(4),lwd=3,bty="n",cex=0.7)
                    
                    plot(valpha[vleastsq<amax],vleastsq[vleastsq<amax],xlab="Alpha hypothesis",ylab="Least squares goodness of fit statistic")
                    points(valpha[vleastsq==min(vleastsq)],vleastsq[vleastsq==min(vleastsq)],col=4,cex=1.5) # indicate the best-fit value on the plot
                    
                    plot(vphi[vleastsq<amax],vleastsq[vleastsq<amax],xlab="phi hypothesis",ylab="Least squares goodness of fit statistic")
                    points(vphi[vleastsq==min(vleastsq)],vleastsq[vleastsq==min(vleastsq)],col=4,cex=1.5) # indicate the best-fit value on the plot
                    
                    plot(vepsilon_a[vleastsq<amax],vleastsq[vleastsq<amax],xlab="Epsilon_a hypothesis",ylab="Least squares goodness of fit statistic")
                    points(vepsilon_a[vleastsq==min(vleastsq)],vleastsq[vleastsq==min(vleastsq)],col=4,cex=1.5) # indicate the best-fit value on the plot
                    
                    par(mfrow=c(1,1)) # set the plotting area to one plot per page
                    plot(vday_2007_2010,data_2007_2010,ylim=c(0,1.5*max(data_2007_2010)),xlab="Time, in days relative to Jan 1, 2007",ylab="Incidence",cex=2,main="Weekly Incidence of notified cases\n 2007-2010")
                    lines(vday_2007_2010,vbest_leastsq_fit,col=2,lwd=5) # overlay the model
                    points(vday_2007_2010,data_2007_2010,cex=2)
                    
                    legend("topleft",legend=c("Data","Least-squares best-fit modelA prediction"),col=c(1,2),lwd=3,bty="n",cex=0.7)
                    
                    ######################################################################## 
                    # print the least squares info on the plot
                    ########################################################################
                    # CHOOSE VALUES OF X and Y THAT ARE WITHING RANGE OF DATE AND DATA
                    #text(x,y,paste(" When beta0 = ",round(vbeta0[which.min(vleastsq)],2),sep=""),adj=0,cex=0.6)
                    #text(x,y,paste(" and alpha = ",round(valpha[which.min(vleastsq)],2),sep=""),adj=0,cex=0.6)
                    #text(x,y,paste(" and phi = ",round(vphi[which.min(vleastsq)],2),sep=""),adj=0,cex=0.6)
                    #text(x,y,paste(" and epsilon_a = ",round(vepsilon_a[which.min(vleastsq)],2),sep=""),adj=0,cex=0.6)
                    #text(x,y,paste(" the least-squares \n statistic is = ",round(min(vleastsq),1),sep=""),adj=0,cex=0.6)
                    
                    
                }
            }
            
            
        } # end check that the length of the incidence vector matches the length of the data vector
    } # end check that the incidence vector actually has some data in it
} # end loop over iterations



########################################################################
# write the results out to a file for later reference
########################################################################
zdat = data.frame(beta0=vbeta0,alpha=valpha,phi=vphi,epsilon_a=vepsilon_a,leastsq=vleastsq)
write.table(zdat,"results_of_least_squares_fit_to_meningitis_data.txt",row.names=F)

# Printing best fit parameters to the console
cat(" When beta0 = ",round(vbeta0[which.min(vleastsq)],5),sep="")
cat(" and alpha = ",round(valpha[which.min(vleastsq)],5),sep="")
cat(" and phi = ",round(vphi[which.min(vleastsq)],5),sep="")
cat(" and epsilon_a = ",round(vepsilon_a[which.min(vleastsq)],5),sep="")
cat(" the least-squares \n statistic is = ",round(min(vleastsq),5),sep="")


