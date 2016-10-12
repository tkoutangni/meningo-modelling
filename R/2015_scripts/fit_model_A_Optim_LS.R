
## Model fitting to data.
require(deSolve)
# source("R/loadFunctions.R")

# testing model fit with a health center with complete time series weekly data

load("data/clean_data/seguenega_hyperendemic_2007_2011.RData")
seguenega_hyperendemic_2007_2011$week_num=seq(157,417,1)
    
# Plotting the data to fit.

plot(seguenega_hyperendemic_2007_2011[,"csps kalsaka"]*1e+05,ylab="",xlab="Time (Calendar years)",
     sub="Non-epidemic years following 2006", type="o",pch=16)
title(main="Seguenega District: kalsaka health center (2007 - 2011)",ylab="Weekly incidence \n per 100,000", las=2)

# plotting data with week number
plot(seguenega_hyperendemic_2007_2011[,"week_num"],pch=16,
     seguenega_hyperendemic_2007_2011[,"csps kalsaka"]*1e+05, type="o",ylab="",xlab="")
title(ylab="Weekly Incidence", xlab="Time in weeks relative to January 2004",sub="Non-epidemic years following 2006")

#Estimating Parameters
# The Goal: Try to find values of unknown parameters that provide a curve close to that of the data

#Data Fitting
# Goal: Estimate values of unknown parameters by fitting model predition to data. 
# We will use a least squares approximation
#We wish to minimize the following least_square statistic value:

# sum_error^2 = sum(I(t) - I(hat)(t))^2
# least_square_stat = sum_error^2/mean(I(hat)(t))
    
#I(t) is observed values of infectives
#I(hat)(t) is model solution for estimates of unknown parameters value

# sum_error^2 is the sum of the errors squared.
# Preparing data by converting time vector to number of days since january first 2007.
    
    
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
# Creating the least_square function taking 2 arguments: the parameters to be optimized over
# and data vector containing weekly incidence to fit the model with.
least_square<-function(parms, data){ # parms is the vector of parameters to estimate.
    PARMS_SCIRS_f["epsilon_a"]=0.11 # 20 fold increase in the invasion rate
    modelA_parms=PARMS_SCIRS_f # Vector with model parameters (fixed param and param to estimate)
    for(i in 1:length(names(parms))){
        modelA_parms[[names(parms)[i]]]=parms[[i]]
    }
    
    #print(cbind(modelA_parms)) # print the current parameters combination being evaluated by optim
    # run model for the the current parameter combination
    modelA_predict = sim.SCIRS_harmonic(inits,modelA_parms,nd=10*year)
    
    weekly_incidence = modelA_predict$newI.Carrier[modelA_predict$time%in%vday_2007_2010]
    # assuming cases notification data are not exaustive, we can compute the fraction of case
    # that are actually notified then apply that fraction to model estimate of incidence so that
    # area under the model curve equals the sum of the notified cases data incidence. 
    # frac_notified = sum(data)/sum(weekly_incidence) 
    # weekly_incidence = weekly_incidence*frac_notified # normalize the model prediction so area under curve
    # equals the sum of the data incidence
    #weekly_incidence = modelA_predict$newI.Carrier
    if (length(weekly_incidence)>1&!is.na(sum(weekly_incidence))){
        if (min(weekly_incidence)>=0&length(weekly_incidence)==length(data)){
            
            ######################################################################## 
            # calculate the least squares statistic
            ######################################################################## 
            least_squares = sum((weekly_incidence-data)^2)/mean(weekly_incidence)
            #least_squares = sum((weekly_incidence-data)^2)
            
        } # end check that the length of the weekly_incidence vector matches the length of the data vector
    } # end check that the weekly_incidence vector actually has some data in it
    return(least_squares)
} # end least square function

## Now running the optimisation algorithm.

#Initial values of the parameters to be optimized over

parms=c(
    beta0 = 55/year,
    #epsilon_a = 0.11,
    alpha=26/year,
    phi = 12/year
    #teta = 91
    #a0=0.012/year
)

lower_bounds=c(beta0 = 50/year,alpha=1/year,phi = 0.2/year)
upper_bounds=c(beta0 = 200/year,alpha=52/year,phi = 52/year)


fit_A = optim(
    parms, # initial parameters to start optimisation.
    least_square,
    method="L-BFGS-B",
    #method="Nelder-Mead",
    #lower=lower_bounds,
    #upper=upper_bounds,
    control = list(trace = 2, maxit = 500),
    data = data_2007_2010 # data vector
    )


fit_A = optim( # rerun the optimisation algorithm a second time.
    fit_A$par, # start the optimisation with parameters values estimated after first run.
    least_square,
    method="L-BFGS-B",
    lower=lower_bounds,
    upper=upper_bounds,
    control = list(trace = 2, maxit = 500),
    data = data_2007_2010    
)

print(c(fit_A$par,LS_value=fit_A$value))

# overlaying model prediction with data.
fit_parms = PARMS_SCIRS_f
#for (i in length(names(fit_A$par))){fit_parms[names(fit_A$par)[i]]=fit_A$par[i]}
#cbind(fit_parms)
#fit_parms["beta0"] = fit_A$par["beta0"]
#fit_parms["epsilon_a"] = fit_A$par["epsilon_a"]
#fit_parms["alpha"] = fit_A$par["alpha"]
#fit_parms["phi"] = fit_A$par["phi"]
#fit_parms["epsilon_a"]=0.5

fit_parms["beta0"] = estim_par[1,"beta0"]
fit_parms["alpha"] = estim_par[1,"alpha"]
fit_parms["phi"] = estim_par[1,"phi"]
fit_parms["epsilon_a"]=0

test1 = sim.SCIRS_harmonic(inits,fit_parms,nd=10*year)
test1.incid = test1$newI.Carrier[test1$time%in%vday_2007_2010]
test1.carriage = test1$Carrier[test1$time%in%vday_2007_2010]
# changing value of epsilon_a above

test1 = sim.SCIRS_harmonic(inits,fit_parms,nd=10*year)
test1.incid2 = test1$newI.Carrier[test1$time%in%vday_2007_2010]
test1.carriage2 = test1$Carrier[test1$time%in%vday_2007_2010]


par(gfx2)
#layout.show(2)

plot(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],data_2007_2010*100000,pch=16,type="b",
     ylab = "Weekly Incidence \n per 100,000", xlab="Time (Calendar Weeks/Years)")

lines(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],test1.incid*100000,type = "l",lty=1,lwd=3, col="black",cex=2.8)

## Overlaying model prediction and data ( for different value of epsilon_a)

fit_parms["beta0"] = estim_par[1,"beta0"]
fit_parms["alpha"] = estim_par[1,"alpha"]
#fit_parms["phi"] = estim_par[1,"phi"]
fit_parms["phi"] = 0.4/year
fit_parms["epsilon_a"]=0.11

test1 = sim.SCIRS_harmonic(inits,fit_parms,nd=10*year)
test1.incid = test1$newI.Carrier[test1$time%in%vday_2007_2010]
test1.carriage = test1$Carrier[test1$time%in%vday_2007_2010]
# changing value of epsilon_a above

plot(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],data_2007_2010*100000,pch=16,type="b",
     ylab = "Weekly Incidence \n per 100,000", xlab="Time (Calendar Weeks/Years)")

lines(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],test1.incid*100000,type = "l",lty=1,lwd=3, col="gray",cex=2.8) # 0.11
lines(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],test1.incid*100000,type = "l",lty=2,lwd=3, col="blue",cex=2.8) # 0.3
lines(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],test1.incid*100000,type = "l",lty=3,lwd=3, col="green",cex=2.8) # 0.5




plot(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],seq(0,85,length=209),type="n",las=1,
     ylab = "Weekly Incidence \n per 100,000", xlab="Time (Calendar Weeks/Years)")

fit_parms["beta0"] = estim_par[1,"beta0"]
fit_parms["alpha"] = estim_par[1,"alpha"]
fit_parms["phi"] = estim_par[1,"phi"]
fit_parms["epsilon_a"]=0.11

test1 = sim.SCIRS_harmonic(inits,fit_parms,nd=10*year)
test1.incid = test1$newI.Carrier[test1$time%in%vday_2007_2010]
test1.carriage = test1$Carrier[test1$time%in%vday_2007_2010]

lines(time(kalsaka_ts)[year(kalsaka_ts)>=2007&year(kalsaka_ts)<=2010],test1.incid*100000,type = "l",lwd=3,lty=3, col="red",cex=2.8)
#axis(2, at = c(round(range(test1.incid*100000)[2],1)),las=1,col.lab="blue",col.ticks="red",cex.axis=0.8,yaxs="i",tck=0.05)
#axis(2, at = c(round(range(test1.incid2*100000)[1],1)),las=1,col.lab="blue",col.ticks="blue",cex.axis=0.8,tck=0.05,mgp=c(1,-1.5,0))
axis(2, at = c(round(range(test1.incid*100000)[2],1)),las=1,col.lab="blue",col.ticks="blue",cex.axis=0.8,tck=0.05,mgp=c(1,-1.5,0))


#mtext(side=3,line=0,adj = 0,cex=0.9, expression(paste("Prediction",(epsilon[a]==0.8))))

#par(new=TRUE)
#plot(test1.carriage*100,type = "o",pch =21, col="red",xaxt="n",yaxt="n",ylab="",xlab="")
#axis(4,las=1)
#mtext("Carriage",side=4,line=3)

legend("topright",
       inset = c(0, -0.1),  x.intersp=0.5, y.intersp=0.9,bty="n",
       xjust=0.5,yjust=0,col=c("gray","black", "lightskyblue"),lty=c(1),lwd=c(3),
       cex = 1, xpd = TRUE, horiz=T,
       #legend=c("Data",expression(epsilon[a]==0),expression(epsilon[a]==0.11)))
       legend=c(expression(epsilon[a]==0,
                           epsilon[a]==0.3,
                           epsilon[a]==0.5)
                ))

range(data_2007_2010*100000)

range(test1.incid*100000)
range(test1.carriage*100)

print(c(fit_A$par,LS_value=fit_A$value))


#legend("topleft", inset=.05, title="", c("data","model"),
       #lty=c(1, 2), pch=c(15, 17), col=c("black", "blue"))


#==============================================================================
#### The seasonality of the transmission rate:

#==============================================================================

toto = betat(beta0=53.69, epsilon_b=1)
toto1 = betat(beta0=53.69, epsilon_b=0)

par(gfx2)
plot.var(a_t.time[-1],toto$beta_t,evry_xyear=1)
title(ylab=expression("Transmission Rate " (day^{-1})))
seq_xaxis = seq(0,length(t_range.year),by=evry_xyear)
t_xaxis =t_xaxis[-1]
axis(1,at=c(0,t_range.year[t_xaxis]), labels=c(0,t_xaxis))

lines(a_t.time[-1],toto1$beta_t,type="l",lty=3,ylab="",xlab="")

abline(h=53.69/year, col="blue")

# placing legend above graph
legend("topright",
       inset = c(0, -0.2),  x.intersp=0.1, y.intersp=-.5,bty="n",
       xjust=0,yjust=0,lty=c(1,1,2,3),col=c("blue","black","black","black"),
       lwd=2, cex = 0.75, xpd = TRUE, horiz=T,
       legend=c(expression(beta[0]==0.014, epsilon[b]==1,epsilon[b]==0,epsilon[b]==0.7)))

## Model B simulations.


PARMS_SICRS_modelB = fit_parms
PARMS_SICRS_modelB["epsilon_a"]= 0
PARMS_SICRS_modelB["epsilon_b"]=1
PARMS_SICRS_modelB["phi"]= 2/year

par(gfx)
par(mfrow=c(2,2))

toto = sim.SCIRS_harmonic(inits,PARMS_SICRS_modelB,nd=50*year)
toto_1=subset(toto,time>=40*year)
seq_xaxis = seq(range(toto_1["time"])[1],range(toto_1["time"])[2],5*year)

plot(toto_1$time,toto_1$newI*1e+05,type="l",las=1,xaxt="n",xlab="Time (Years)",ylab="Weekly Incidence\n per 100,000")
axis(1,at=seq_xaxis, labels=seq(0,length.out = length(seq_xaxis),by=5))
abline(v=seq_xaxis, col='grey', lwd=1)
writelabel("d)",at=-0.1,cex=1) # adding a label for the figure
mtext(side=3, bquote(paste(epsilon[b]==1,"  ; " ,1/phi1==6," month Immunity")),cex=0.7)

round(range(toto_1$newI*100000),2)
round(range(toto_1$Carrier*100),2)





