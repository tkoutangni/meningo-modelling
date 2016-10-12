#==================================================
# function for two sided centred mooving average.
# take a time serie x as first argument and the
# number of time units to compute the centered mva 
# over
#==================================================
doPlot = FALSE
mav <- function(x,n=3){
    filter(x,rep(1/n,n), sides=2) # sides = 2 indicates that observation i becomes
    # the simple arithmetic average of observations i-1, i, and i+1
}

#######

mva_function<-function(data,period=2){
    #no_missing_data= data[!is.na(data)] # removing all missing data points,
    
    ## Creating an empty zoo matrix for storing the smoothed zoo time series
    mva_data = zoo(x=matrix(nrow=dim(data)[1], 
                            ncol=dim(data)[2],
                            byrow=TRUE,
                            dimnames=list(c(time(data)),
                                          c(names(data)))),
                   order.by=time(data))
    
    # for loop to compute de simple mooving average over specified period
    
    for (i in seq_along(colnames(data))){
        mva_data[,i] = mav(data[,i],n=period) # make use of mva function for centred moving average computation i have created before.
    } # end for loop
    
    return(mva_data)
} # end function mva_function()


#computing julian days since a given time point. needed for model fit algo 
# and append a variable julian_day to the data_frame
# Julian_day is a function to compute julian days since an origin date
# - takes 2 arguments, "data" which must be an ordered time series or type "zoo" or "ts"
# - takes an "org" argument which is the origin date as character of the form "1906-01-01"

julian_day = function(data, orig ="1906-01-01"){
    data$julian_day= julian(x=time(data), origin=as.Date(orig))
    return(data)
} # end function julian_day()



# selecting health centers with "usable" data per year
## === SEGUENEGA DISTRICT === ##  (id = 2)

seguen_2006 = mva_function(df.no.epi[,c(1:8)][year(time(df.no.epi))==2006],period=3)
#computing and appending the julian_day time vector to data.frame
seguen_2006 = julian_day(data=seguen_2006, orig="2003-12-28")

seguen_2006_2007 = mva_function(df.no.epi[,c(1:6)][year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2007],period=3)
#computing and appending the julian_day time vector to data.frame
seguen_2006_2007 = julian_day(data=seguen_2006_2007, orig="2003-12-28")

seguen_2007 = mva_function(df.no.epi[,c(1:6)][year(time(df.no.epi))==2007],period=3)
seguen_2007 = julian_day(data=seguen_2007, orig="2003-12-28")

seguen_2008 = mva_function(df.no.epi[,c(1:6)][year(time(df.no.epi))==2008],period=3)
seguen_2008 = julian_day(data=seguen_2008, orig="2003-12-28")

seguen_2009 = mva_function(df.no.epi[,c(1:6)][year(time(df.no.epi))==2009],period=3)
seguen_2009 = julian_day(data=seguen_2009, orig="2003-12-28")

seguen_2010 = mva_function(df.no.epi[,c(1:6)][year(time(df.no.epi))==2010],period=3)
seguen_2010 = julian_day(data=seguen_2010, orig="2003-12-28")

seguen_2004_2010 = mva_function(df.no.epi[,c(1:6)][year(time(df.no.epi))<=2010],period=3)
seguen_2004_2010 = julian_day(data=seguen_2004_2010, orig="2003-12-28")

seguen_2006_2010 = seguen_2004_2010[year(time(seguen_2004_2010))>=2006]


### HOUNDE DISTRICT DATA PER YEAR  (id = 2)
hounde_2004 = mva_function(dfNoEpiHounde[,-c(6,11,12,14)][year(time(dfNoEpiHounde))==2004],period=3)
hounde_2004 = julian_day(data=hounde_2004, orig="2003-12-28")

hounde_2005 = mva_function(dfNoEpiHounde[,-c(6,11,12,14)][year(time(dfNoEpiHounde))==2005],period=3)
hounde_2005 = julian_day(data=hounde_2005, orig="2003-12-28")

hounde_2006 = mva_function(dfNoEpiHounde[,-c(6,11,12,14)][year(time(dfNoEpiHounde))==2006],period=3)
hounde_2006 = julian_day(data=hounde_2006, orig="2003-12-28")

hounde_2007 = mva_function(dfNoEpiHounde[year(time(dfNoEpiHounde))==2007],period=3)
hounde_2007 = julian_day(data=hounde_2007, orig="2003-12-28")

hounde_2008 = mva_function(dfNoEpiHounde[year(time(dfNoEpiHounde))==2008],period=3)
hounde_2008 = julian_day(data=hounde_2008, orig="2003-12-28")

hounde_2009 = mva_function(dfNoEpiHounde[year(time(dfNoEpiHounde))==2009],period=3)
hounde_2009 = julian_day(data=hounde_2009, orig="2003-12-28")

hounde_2010 = mva_function(dfNoEpiHounde[year(time(dfNoEpiHounde))==2010],period=3)
hounde_2010 = julian_day(data=hounde_2010, orig="2003-12-28")

## == LENA DISTRICT per year (id= 3) == ##

lena_2004 = mva_function(dfNoEpiLena[year(time(dfNoEpiLena))==2004],period=3)
lena_2004 = julian_day(data=lena_2004, orig="2003-12-28")

lena_2005 = mva_function(dfNoEpiLena[year(time(dfNoEpiLena))==2005],period=3)
lena_2005 = julian_day(data=lena_2005, orig="2003-12-28")

lena_2006 = mva_function(dfNoEpiLena[year(time(dfNoEpiLena))==2006],period=3)
lena_2006 = julian_day(data=lena_2006, orig="2003-12-28")

lena_2007 = mva_function(dfNoEpiLena[year(time(dfNoEpiLena))==2007],period=3)
lena_2007 = julian_day(data=lena_2007, orig="2003-12-28")

lena_2008 = mva_function(dfNoEpiLena[year(time(dfNoEpiLena))==2008],period=3)
lena_2008 = julian_day(data=lena_2008, orig="2003-12-28")

lena_2009 = mva_function(dfNoEpiLena[year(time(dfNoEpiLena))==2009],period=3)
lena_2009 = julian_day(data=lena_2009, orig="2003-12-28")

lena_2010 = mva_function(dfNoEpiLena[year(time(dfNoEpiLena))==2010],period=3)
lena_2010 = julian_day(data=lena_2010, orig="2003-12-28")

###== K.vigue district (id = 4)
Kvigue_2008 = mva_function(dfNoEpiKvigue[year(time(dfNoEpiKvigue))==2008],period=3)
#index to use c(2,3)
Kvigue_2008 = julian_day(data = Kvigue_2008, orig="2003-12-28")


Kvigue_2009 = mva_function(dfNoEpiKvigue[year(time(dfNoEpiKvigue))==2009],period=3)
# index to use none
Kvigue_2009 = julian_day(data = Kvigue_2009, orig="2003-12-28")

Kvigue_2010 = mva_function(dfNoEpiKvigue[year(time(dfNoEpiKvigue))==2010],period=3)
#index.to.use = c(3,5)
Kvigue_2010 = julian_day(data = Kvigue_2010, orig="2003-12-28")

### ==== Ouahigouya district (id = 5) =====###
ouahigouya_2004 = mva_function(dfNoEpiOuahigouya[,-c(5,7,8,12,14,29,34,45,46,47,52,55,57,58,59)][year(time(dfNoEpiOuahigouya))==2004],period=3)

#time.vector = c(coredata(seguen_2010$julian_day))
#seguen_2010$julian_day= julian(x=time(seguen_2010), origin=as.Date("1906-01-01"))
#time.vector = c(coredata(seguen_2010$julian_day))

##=================================================================================
# ENDS OF MAIN DATA SMOOTING AND COMPUTATION OF JULIAN DAYS SINCE Origin = "2003-12-28"
# THE START OF CALENDAR WEEK ONE OF YEAR 2004.
##=================================================================================
if(doPlot){
    op <- par(mar = c(5,7,4,2) + 0.1)
    par(op)
## Plotting for visulatisation of time series.
xyplot(seguen_2004_2010[,c(1:6)]*1e+05,las=1, 
       xlab = "Year/calendar week", 
       ylab = "Incid x100,000",layout=c(2,3))

xyplot(mva_function(df.no.epi[,c(7:11)][year(time(df.no.epi))<=2010]*1e+05,period=3),las=1, 
       xlab = "Year/calendar week", 
       ylab = "Incid x100,000",layout=c(2,3))
} # end of doPlot

##=================================================================================

if(doPlot){
    op <- par(mar = c(5,7,4,2) + 0.1)
    par(op)
    
    plot(mva_function(df.no.epi,period=3)*1e+05,type="b",pch=20,ylab="",xlab="time",main="")
    title(main="Weekly incidence Data: Seguenega district health \n centers", sub="Health centres did not crossed epidemic treshold",
          ylab="Weekly Incid /100,000")
    
    
    plot(mva_function(df.epi,period=3)*1e+05,type="b",pch=20,ylab="",xlab="time",main="")
    title(main="Weekly incidence Data: Seguenega district health \n centers", sub="Health centres crossed epidemic treshold",
          ylab="Weekly Incid \n per 100,000")
    
    plot(mva_function(df.no.epi[,c(1:6)][year(time(df.no.epi))==2007],period=3)*1e+05,type="b",pch=20,ylab="",xlab="time",main="")
    title(main="Weekly incidence Data: Seguenega district health \n centers", sub="Health centres did not crossed epidemic treshold",
          ylab="Weekly Incid per 100,000")
    # plotting smoothed time serie for 2006, 2007 and 2010
    
    
    par(mfrow=c(2,2))
    plot(seguen_2006_2007*1e+05,plot.type="single",col=palette("default"),pch=20, type="b",lwd=2,las=1,ylab="Weekly Incidence",xlab="Weeks/Years", main="Year 2006 and 2007")
    plot(seguen_2010*1e+05,plot.type="single",col=palette("default"),pch=20, type="b",lwd=2,las=1,ylab="Weekly Incidence",xlab="Weeks/Months", main="Year 2010")
    
# computing the number of julian days since 100 years before start of time serie
# This julian time vector will be used for trajectory matching when we try to fit model
# to data. We want to start fitting the model output to data after discarding 
# # model transcient dynamics. This way we expect parameters estimates to be less dependants
    # on initial conditions.
   
#===========================================================
# Another Way to compute the simple mooving average with ts class time series
#===========================================================
    
    # time serie from core data using ts function.
    ts_wdat.seguen = ts(wdat.seguen.core,start=c(2006,1,1),end=c(2010,12,26), freq=52)
    
    plot(ts_wdat.seguen*1e+05,pch=20,type="b",ylim=c(0,1.5*max(ts_wdat.seguen*1e+05)),las=1,ylab="",xlab="")
    mtext(expression(paste("x",10^-5)),side=3,cex=.9, adj=0)
    title(main="Original data",ylab="Weekly Incidence",xlab="Calendar Weeks/Year")
    
    mav_wdat.seguen = mav(ts_wdat.seguen,3) # 3 data points to compute the moving average.
    plot(mav_wdat.seguen*1e+05,col="red",lwd=3,type="b")
    
    
    ## ==============================================================================
    
    ## Decomposition of time serie, into, Seasonal, Trend and random component
    ## using "decompose" function in R base.
    #ts_data = ts(data, freq=52) # creating a weekly time serie from data values
    #decomp_toto = decompose(ts_data*1e+05,type="additive")
    #plot(decomp_toto)
    #plot(decomp_toto$x,main="")
    #lines(decomp_toto$seasonal,col="blue")
    #lines(decomp_toto$trend,col="red")
    #par(mfrow=c(2,1))
    
} # end doPlot conditional

doTestTTR=FALSE
if(doTestTTR){
    #==============================================================================
    ## Computing the simple mooving average using the SMA function in "TTR" package
    #==============================================================================
    
    library("TTR")
    ## Smoothing the data prior to model fit using simple mooving average
    # Creating a function to compute the Simple moving average over the columns of 
    # an ordered time series data (a zoo matrix in this case)
    # takes an argument "data" which is the time serie to smooth.
    # takes an argument "period" which is the Number of periods to average over.
    # n default to 2 time unit.
    # uses package {TTR} (SMA function)
    # Because SMA function compute mooving average on time series with non missing data
    # the function will remoove all missing data points from the serie before computation
    
    sma_function<-function(data,period=2){
        #no_missing_data= data[!is.na(data)] # removing all missing data points,
        
        ## Creating an empty zoo matrix for storing the smoothed zoo time series
        SMA_data = zoo(x=matrix(nrow=dim(data)[1], 
                                ncol=dim(data)[2],
                                byrow=TRUE,
                                dimnames=list(c(time(data)),
                                              c(names(data)))),
                       order.by=time(data))
        
        # for loop to compute de simple mooving average over specified period
        
        for (i in seq_along(colnames(data))){
            SMA_data[,i] = SMA(data[,i],n=period)
        } # end for loop
        
        return(SMA_data)
        #plot(mva_function(df.no.epi[,c(1:6)],period=3)*1e+05,type="b",pch=20,ylab="",xlab="time",main="")
        #title(main="Weekly incidence Data: Seguenega district health \n centers", sub="Health centres did not cross epidemic treshold")
        
    } # end function
    
    
    data_seguen_non_fe_fs = incid.menin[,district.index$seguenega][,group_fs_seguenega$non_fe_fs.index]
    no_missing_data_seguen_non_fe_fs = data_seguen_non_fe_fs[!is.na(data_seguen_non_fe_fs)]
    no_missing_data_seguen_non_fe_fs = cbind(no_missing_data_seguen_non_fe_fs[,c(1,2,4,5,6,7,9)]) #remove Health Centre with unsuable data 
    
    # Smoothing time serie with Simple Moving average function from package {TTR} 
    sma_no_missing_data_seguen_non_fe_fs=
        sma_function(no_missing_data_seguen_non_fe_fs,period=2)
    
    
    incid_plot(sma_no_missing_data_seguen_non_fe_fs,
               main="Seguenega District:\n Health centre with no outbreak from 2004 - 2010")
    
    for (i in seq_along(unique(year(time(sma_no_missing_data_seguen_non_fe_fs))))){
        actual_year[i] = unique(year(time(sma_no_missing_data_seguen_non_fe_fs)))[i]
        print(actual_year[i])
        incid_plot(sma_no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==actual_year[i]],
                   main="Seguenega District:\n Health centre with no outbreak from 2004 - 2010")  
    } # end for loop
    
    pdf(file="figs/plot-march/seguenega_non_epidemic_hs_year2.pdf")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2004],
               main="Seguenega District:\n Health centre with no outbreak (2004)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2005],
               main="Seguenega District:\n Health centre with no outbreak (2005)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2006],
               main="Seguenega District:\n Health centre with no outbreak (2006)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2007],
               main="Seguenega District:\n Health centre with no outbreak (2007)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2008],
               main="Seguenega District:\n Health centre with no outbreak (2008)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2009],
               main="Seguenega District:\n Health centre with no outbreak (2009)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2010],
               main="Seguenega District:\n Health centre with no outbreak (2010)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2011],
               main="Seguenega District:\n Health centre with no outbreak (2011)")
    incid_plot(no_missing_data_seguen_non_fe_fs[year(time(sma_no_missing_data_seguen_non_fe_fs))==2012],
               main="Seguenega District:\n Health centre with no outbreak (2012)")
    
    dev.off()
} # end doTestTTR for mooving average computation

#toto = mva_function(data = dfEpiHounde[year(dfEpiHounde)==2006],period=2)
