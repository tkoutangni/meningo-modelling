##################################################################################
##################################################################################
# an R script to fit the parameters of the SCIRS model to to meningitis weekly epidemic
# data by minimizing the Least Squares statistic
#
# Author:  Thibaut koutangni
#          koutibaut@yahoo.fr
# Created: Feb  23, 2015
#
#
##################################################################################
require("chron")
par(pch=20)  # the dot style for the plots
source("R/SCIRS_harmonic_CI.R")
source("R/rpackages.R")
source("R/loadFunctions.R")
source("R/models_parameters.R")

##################################################################################
# The Burkina Faso data is weekly, (assuming) the date of each point corresponding to the
# date of the end of the week over which the data were collected.
# converting these dates to time in days, relative to Jan 1, 2004
# I will be using this vector of dates, vday, to obtain the model estimates
# of the incidence at that time.
##################################################################################


##################################################################################
# first compute Julian days since December 28, 2003 (the last last week of year 2003
wdat.seguen$julian_day= julian(x=time(wdat.seguen), origin=as.Date("2003-12-28"))
#vday = append(min(seguenega_hyperendemic_2007_2011$julian_day)-7,seguenega_hyperendemic_2007_2011$julian_day)
vday = c(coredata(wdat.seguen$julian_day))

wdat.seguen.core = c(coredata(wdat.seguen[,1]))

#########################################################################################
# now using the vday vector, I can find  model predicted weekly incidence that matches
# available incidence data point.

# modelA_predict$time%in%vday returns the indices of the elements of modelA_predict$time that
# are also found in vday
# weekly_incidence = modelA_predict$newI.Carrier[modelA_predict$time%in%vday]
########################################################################################

##################################################################################
# set up the model parameters
##################################################################################
# model parameters already defined in the appropriate file.

##################################################################################
# iterate a whole bunch of times and sample a different value of beta0 and alpha and phi
# at each iteration from a uniform distribution.Compare the predicted weekly incidence 
# to the observed data, and obtain the least squares statistic.  Put the information
# into vectors, to enable a summary plot at the end.
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

niter = 5000  # number of different beta0, alpha and phi hypotheses to try.

ptm <- proc.time() #get system time at start of exucution of the for loop
# this is to compute the total excusion time of my for loop

for (iter in 1:niter){ # for loop start.
    
    if (iter%%100==0) cat("Doing iteration ",iter," out of ",niter,"\n")  # inform me the script is doing something, and not hung
    
    beta0 = runif(n=1, min=50/year,max=200/year) # randomly sample beta0 uniformly 
    alpha = runif(n=1,min=1/year,max=52/year)    # randomly sample alpha uniformly 
    phi = runif(n=1,min=0.2/year,max=52/year)    # randomly sample phi uniformly
    #epsilon_a = runif(n=1, min=0, max=1)         # randomly sample epsilon_a uniformly
    
    ### narrowing down after a first simulation try
    
    #beta0 = runif(n=1, min=73/year,max=140/year) # randomly sample beta0 uniformly 
    #alpha = runif(n=1,min=1/year,max=73/year)    # randomly sample alpha uniformly 
    #phi = runif(n=1,min=0.5/year,max=73/year)    # randomly sample phi uniformly
    #epsilon_a = runif(n=1, min=0.8, max=1) 
    
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
        #epsilon_a = epsilon_a,
        phi = phi 
    )
    
    #print(parms,sep=" ")
    
    modelA_parms=PARMS_SCIRS_f # Vector with model parameters (fixed param and param to estimate)
    modelA_parms["epsilon_b"]=1
    for(i in 1:length(names(parms))){ # using the current combination of hypothesis values with fixed parmaeters values
        modelA_parms[[names(parms)[i]]]=parms[[i]]
    }
    
    #print(cbind(modelA_parms)) # print the current parameters combination being evaluated
    # run model for the the current parameter combination
    modelA_predict = sim.SCIRS_harmonic(inits,modelA_parms,nd=10*year)
    
    weekly_incidence = modelA_predict$newI.Carrier[modelA_predict$time%in%vday]
    
    data = wdat.seguen.core
    # assuming cases notification data are not exaustive, we can compute the fraction of case
    # that are actually notified then apply that fraction to model estimate of incidence so that
    # area under the model curve equals the sum of the notified cases data incidence. 
    #frac_notified = sum(data)/sum(weekly_incidence) 
    #weekly_incidence = weekly_incidence*frac_notified # normalize the model prediction so area under curve
    # equals the sum of the data incidence
        
    
    ##################################################################################
    # now overlaying the model on the data, and calculate the least-squares
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
            #vepsilon_a = append(vepsilon_a,epsilon_a)
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
                    
                    #plot(vepsilon_a[vleastsq<amax],vleastsq[vleastsq<amax],xlab="Epsilon_a hypothesis",ylab="Least squares goodness of fit statistic")
                    #points(vepsilon_a[vleastsq==min(vleastsq)],vleastsq[vleastsq==min(vleastsq)],col=4,cex=1.5) # indicate the best-fit value on the plot
                    
                    par(mfrow=c(1,1)) # set the plotting area to one plot per page
                    plot(vday,wdat.seguen.core,ylim=c(0,1.5*max(wdat.seguen.core)),xlab="Time, in days relative to Jan 1, 2006",ylab="Incidence",cex=2,main="Weekly Incidence \n 2006-2010")
                    lines(vday,vbest_leastsq_fit,col=2,lwd=5) # overlay the model
                    points(vday,wdat.seguen.core,cex=1.5)
                    
                    legend("topleft",legend=c("Data","Least-squares best-fit modelB prediction"),col=c(1,2),lwd=3,bty="n",cex=0.7)
                    
                    ######################################################################## 
                    # print the least squares info on the plot
                    ########################################################################
                    # I  NEED TO CHOOSE VALUES OF X and Y THAT ARE WITHING RANGE OF DATE AND DATA
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


runtime = proc.time() - ptm  # compute the runtime of this piece of nested for loop code
runtime_minutes = runtime[3]/60 # converting runtime from seconds to minutes
cat("Le temps de calcul de ce code est estimé à: ",runtime_minutes, "minutes\n", "Soit: ",runtime[3], "Seconds")




########################################################################
# write the results out to a file for later reference
########################################################################
zdat = data.frame(beta0=vbeta0,alpha=valpha,phi=vphi,epsilon_a=vepsilon_a,leastsq=vleastsq)
#zdat = data.frame(beta0=vbeta0,alpha=valpha,phi=vphi,leastsq=vleastsq)
write.table(zdat,"data/processed_data/least_squares_fit_to_meningitis_data.txt",row.names=F)



# Printing best fit parameters to the console
cat(" When beta0 = ",round(vbeta0[which.min(vleastsq)],5),sep="")
cat(" and alpha = ",round(valpha[which.min(vleastsq)],5),sep="")
cat(" and phi = ",round(vphi[which.min(vleastsq)],5),sep="")
cat(" and epsilon_a = ",round(vepsilon_a[which.min(vleastsq)],5),sep="")
cat(" the least-squares \n statistic is = ",round(min(vleastsq),5),sep="")



### END OF MODEL PARAMETERS OPTIMISATION CODE #############################




## Model simulation with the best fit values.
## Parameters from best fit 
modelA_parms.temp = PARMS_SCIRS_f
modelA_parms.temp["beta0"]=0.37214
modelA_parms.temp["alpha"]=0.09802
modelA_parms.temp["phi"]=0.00066
#modelA_parms.temp["phi"]=0.5/year
modelA_parms.temp["epsilon_a"]=0.2224*4 # 20 fois

## Simulation
modelA = sim.SCIRS_harmonic(inits,modelA_parms.temp,nd=10*year)
wincid = modelA$newI.Carrier[modelA$time%in%vday]
carriage = modelA$Carrier[modelA$time%in%vday]
round(range(carriage*100),3)
round(range(wincid*1e+05),3)

td =time(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010])

plot(td,wincid*1e+05,type="l",lwd=3,xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5)),cex=.9)
points(td,wdat.seguen.core*1e+05,pch=16,cex=1.5,col="gray")

#plot.var_vs_t(var.name="newI.Carrier",x=modelA,eq_time=90*year,everyx=2,lty=1)


### Model response to change in epsilon_a  for a given value of immunity

## Parameters from best fit 
modelA_parms.temp = PARMS_SCIRS_f
modelA_parms.temp["beta0"]=0.37214 # estimated
modelA_parms.temp["alpha"]=0.09802 # estimated
modelA_parms.temp["phi"]=0.00066   # estimated
#modelA_parms.temp["phi"]=0.5/year # 6 months
modelA_parms.temp["epsilon_a"]=0.1112*6 # 90 fois

## Simulation
modelA1 = sim.SCIRS_harmonic(inits,modelA_parms.temp,nd=10*year)
wincid1 = modelA1$newI.Carrier[modelA$time%in%vday]

plot(td,wincid1*1e+05,type="l",lwd=3,xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5," ",epsilon[a]==0.8896,"   ",90,"-fold")),cex=.9)
points(td,wdat.seguen.core*1e+05,pch=16,cex=1,col="gray")

plot(td,wdat.seguen.core*1e+05,pch=16,cex=1,col="gray",xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5," ",epsilon[a]==0.8896,"   ",90,"-fold")),cex=.9)
points(td,wincid1*1e+05,type="l",lwd=3,col="blue")


# black : epsilon = 0.1112*8
# green : epsilon = 0.1112*2
# red : epsilon = 0.1112*4
# blue : epsilon = 0.1112*6

legend("topright",legend=c(
    expression(paste(epsilon[a]==0.8896,"  (90-fold)")),
    expression(paste(epsilon[a]==0.2224,"  (30-fold)")),
    expression(paste(epsilon[a]==0.4448,"  (50-fold)")),
    expression(paste(epsilon[a]==0.6672,"  (60-fold)"))
    ),
    col=c("black","green","red","blue"),lty=c(1,1,1,1),lwd=3,bty="n",cex=0.7)
title(main="Immunity is fixed (4 years)")



## ModelA behavior when epsilon is fixed and imunity is changed
## Parameters from best fit 
modelA_parms.temp = PARMS_SCIRS_f
modelA_parms.temp["beta0"]=0.37214 # estimated
modelA_parms.temp["alpha"]=0.09802 # estimated
#modelA_parms.temp["phi"]=0.00066   # estimated
modelA_parms.temp["phi"]=0.5/year # 6 months
modelA_parms.temp["epsilon_a"]=0.11017 # estimated from fitting

## Simulation
modelA2 = sim.SCIRS_harmonic(inits,modelA_parms.temp,nd=10*year)
wincid2 = modelA2$newI.Carrier[modelA$time%in%vday]
carriage = modelA2$Carrier[modelA$time%in%vday]
round(range(carriage*100),3)
round(range(wincid2*1e+05),3)


plot(td,wdat.seguen.core*1e+05,pch=16,cex=1,col="gray",xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5," ",epsilon[a]==0.11017,"   ",20,"-fold")),cex=.9)
points(td,wincid2*1e+05,type="l",lwd=3,col="blue")


legend("topright",legend=c(
    expression(paste(phi1==0.5,"  (2 years)")),
    expression(paste(phi1==1,"    (1 years)")),
    expression(paste(phi1==2,"    (6 months)")),
    expression(paste(phi1==4,"    (3 months)"))
),
col=c("black","green","red","blue"),lty=c(1,1,1,1),lwd=3,bty="n",cex=0.7)
title(main="Seasonal forcing is fixed \n and Immunity changes",cex=0.8)

###===================

## Model simulation with the best fit values.
## Parameters from best fit 
modelB_parms.temp = PARMS_SCIRS_f
modelB_parms.temp["beta0"]=0.14213
modelB_parms.temp["alpha"]=0.11959
modelB_parms.temp["phi"]=0.13521
#modelB_parms.temp["phi"]=0.5/year
modelB_parms.temp["epsilon_b"]=0# 20 fois

## Simulation
modelB = sim.SCIRS_harmonic(inits,modelB_parms.temp,nd=10*year)
wincid = modelB$newI.Carrier[modelB$time%in%vday]
carriage = modelB$Carrier[modelB$time%in%vday]
range(carriage*100)
range(wincid*1e+05)

td =time(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010])

plot(td,wincid*1e+05,type="l",las=1,lwd=3,xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5)),cex=.9)
points(td,wdat.seguen.core*1e+05,pch=16,cex=1.5,col="gray")

#plot.var_vs_t(var.name="newI.Carrier",x=modelA,eq_time=90*year,everyx=2,lty=1)



###===================
# epsilon_b = 0.499 (2.5 fold increase)

## Model simulation with the best fit values.
## Parameters from best fit 
modelB_parms.temp = PARMS_SCIRS_f
modelB_parms.temp["beta0"]=0.14778
modelB_parms.temp["alpha"]=0.12828
modelB_parms.temp["phi"]=0.12194
#modelB_parms.temp["phi"]=0.5/year
modelB_parms.temp["epsilon_b"]=0.499

## Simulation
modelB = sim.SCIRS_harmonic(inits,modelB_parms.temp,nd=10*year)
wincid = modelB$newI.Carrier[modelB$time%in%vday]
carriage = modelB$Carrier[modelB$time%in%vday]
range(carriage*100)
range(wincid*1e+05)

td =time(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010])

plot(td,wincid*1e+05,type="l",lwd=3,xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5)),cex=.9)
points(td,wdat.seguen.core*1e+05,pch=16,cex=1.5,col="gray")

#plot.var_vs_t(var.name="newI.Carrier",x=modelA,eq_time=90*year,everyx=2,lty=1)

##========

###===================
# epsilon_b = 1 (3 fold increase)

## Model simulation with the best fit values.
## Parameters from best fit 
modelB_parms.temp = PARMS_SCIRS_f
modelB_parms.temp["beta0"]=0.15635
modelB_parms.temp["alpha"]=0.14213
modelB_parms.temp["phi"]=0.11806
#modelB_parms.temp["phi"]=0.5/year
modelB_parms.temp["epsilon_b"]=1

## Simulation
modelB = sim.SCIRS_harmonic(inits,modelB_parms.temp,nd=10*year)
wincid = modelB$newI.Carrier[modelB$time%in%vday]
carriage = modelB$Carrier[modelB$time%in%vday]
range(carriage*100)
range(wincid*1e+05)

td =time(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010])

plot(td,wincid*1e+05,type="l",las=1,lwd=3,xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5)),cex=.9)
points(td,wdat.seguen.core*1e+05,pch=16,cex=1.5,col="gray")

#lines(td,wincid*1e+05,type="l",lwd=3,col="red",xlab="", ylab="")

legend("topright",legend=c(
    expression(paste(espilon[b]==0,"  (2-fold)")),
    expression(paste(espilon[b]==0.499,"    (2.5-fold)")),
    expression(paste(espilon[b]==1,"    (3-fold)"))
    
),
col=c("red","blue","black"),lty=c(1,1,1),lwd=3,bty="n",cex=0.7)
title(main="Seasonal forcing transmission rate \n (1 - 3- fold increase)",cex=0.7)

# Effect of immunity on carriage and disease.

## Model simulation with the best fit values.
## Parameters from best fit 
modelB_parms.temp = PARMS_SCIRS_f
modelB_parms.temp["beta0"]=0.14778
modelB_parms.temp["alpha"]=0.12828
modelB_parms.temp["phi"]=2/year # 6 mois
#modelB_parms.temp["phi"]=0.5/year
modelB_parms.temp["epsilon_b"]=0

## Simulation
modelB = sim.SCIRS_harmonic(inits,modelB_parms.temp,nd=10*year)
wincid = modelB$newI.Carrier[modelB$time%in%vday]
carriage = modelB$Carrier[modelB$time%in%vday]
range(carriage*100)
range(wincid*1e+05)

plot(td,wincid*1e+05,type="l",las=1,lwd=3,xlab="Years / Calendar Weeks", ylab="Weekly Incidence")
mtext(side=3,adj=0, expression(paste("x",10^-5)),cex=.9)


### 1 year immunity 

## Parameters from best fit 
modelB_parms.temp = PARMS_SCIRS_f
modelB_parms.temp["beta0"]=0.14778
modelB_parms.temp["alpha"]=0.12828
modelB_parms.temp["phi"]=1/year # 1 year
#modelB_parms.temp["phi"]=0.5/year
modelB_parms.temp["epsilon_b"]=0

## Simulation
modelB = sim.SCIRS_harmonic(inits,modelB_parms.temp,nd=10*year)
wincid = modelB$newI.Carrier[modelB$time%in%vday]
carriage = modelB$Carrier[modelB$time%in%vday]
range(carriage*100)
range(wincid*1e+05)
lines(td,wincid*1e+05,type="l",lwd=3,col="red",xlab="", ylab="")
mtext(side=3,adj=1, expression(paste(epsilon[b]==0, "  (2-fold)")),cex=.9)


legend("topright",legend=c(
    expression(paste(phi1==2,"  (6 Months)")),
    expression(paste(phi1==1,"  (1 year)")),
    expression(paste(phi1==0.5,"  (2 years)"))
    
),
col=c("black","blue", "red"),lty=c(1,1,1),lwd=3,bty="n",cex=0.7)
title(main="Seasonal forcing transmission rate: \n effect of immunity.",cex=0.7)


## ===  Plotting transmission rate seasonal function
plot(unlist(betat(beta0=0.14213*year, epsilon_b=1)[1]),type="l", ylab="Transmission rate",xlab="time in days",las=1)
lines(unlist(betat(beta0=0.14213*year, epsilon_b=0)[1]),type="l", ylab="Transmission rate",xlab="time in days",las=1,col="blue")
lines(unlist(betat(beta0=0.14213*year, epsilon_b=0.499)[1]),type="l", ylab="Transmission rate",xlab="time in days",las=1,col="red")

legend("topright",legend=c(
    expression(paste(beta[0]==0.14,"  Baseline")),
    expression(paste(epsilon[b]==0,"  (2- fold)")),
    expression(paste(epsilon[b]==0.499,"  (2.5- fold)")),
    expression(paste(epsilon[b]==1,"  (3- fold)"))
    
),
col=c("green","blue", "red", "black"),lty=c(1,1,1,1),lwd=3,bty="n",cex=0.7)
title(main="Transmission rate seasonality",cex=0.7)
