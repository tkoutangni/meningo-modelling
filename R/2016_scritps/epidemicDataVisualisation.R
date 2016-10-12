source("data.cleaning.R")

op <- par(fig=c(0,1,0,1),mfrow=c(2,3),
          mar=c(5,4,3,1),mgp=c(2,1,0))

## Script for all Foyer Epidemic data visualization.

## plotting the data for the Foyer epidemic in each district with usable data.
## defining a function for plotting

plotFoyerEpidemic = function(dfEpi, districtName =''){
    dfEpi = dfEpi
    districtName = districtName
    plot(dfEpi[year(dfEpi)==2006]*1e+05,plot.type="s",
         las=1,col=palette(rainbow(ncol(dfEpi))),
         xlab= "Calendar Weeks (2006)",
         ylab="",
         #ylab="Incid. per 100,000",
         main = paste(districtName , '\n','(' , ncol(dfEpi) ,"health centres",')')
    )
}

# A function to visualise the data per each health centre
# - takes a data frame argument and a epiYear (numeric) argument
plotEachFoyerEpidemic = function (dfEpi,epiYear){
    xyplot(dfEpi[year(dfEpi)==epiYear]*1e+05,
           ylab="Incid. per 100,000",xlab= paste("Calendar Week ", "(", epiYear, ")"),
           type="b",col="black")
}


## hounde District

plotFoyerEpidemic(dfEpiHounde,'Houndé')
title(ylab="Incid. per 100,000", line=3)

# Seguenega district.
plotFoyerEpidemic(dfEpiSeguenega,'Seguenega')

## Lena district
plotFoyerEpidemic(dfEpiLena,'Lena')

# Kvigué District
plotFoyerEpidemic(dfEpiKvigue,'K.Vigué')
title(ylab="Incid. per 100,000", line=3)

## Dafra District
plotFoyerEpidemic(dfEpiDafra,'Dafra')


## Dandé district
plotFoyerEpidemic(dfEpiDande,'Dandé')


## Gourcy District;
#plotFoyerEpidemic(dfEpiGourcy,'Gourcy')
#title(ylab="Incid. per 100,000", line=3)


# Orodora district
#plotFoyerEpidemic(dfEpiOrodora,'Orodora')

dev.print(png, file = "../figs/plotSeptember2015/graphFoyerEpidemic.png", width = 700, height = 500, bg = "transparent")


##=====================================================================================
## Looking at the individual health centre data and see if data is too noisy; in which 
## case we will need to smooth out the noise a bit.


# Houdé District health centres.
plotEachFoyerEpidemic(dfEpiHounde, 2006)
dev.print(png, file = "../figs/plotSeptember2015/graphFoyerEpidemicHounde.png", width = 700, height = 500, bg = "transparent")

# Seguenega District health centres.
plotEachFoyerEpidemic(dfEpiSeguenega,2006)
dev.print(png, file = "../figs/plotSeptember2015/graphFoyerEpidemicSeguenega.png", width = 700, height = 500, bg = "transparent")

## Lena
plotEachFoyerEpidemic(dfEpiLena,2006)
dev.print(png, file = "../figs/plotSeptember2015/graphFoyerEpidemicLena.png", width = 700, height = 500, bg = "transparent")

## K.Vigué.
plotEachFoyerEpidemic(dfEpiKvigue,2006)
dev.print(png, file = "../figs/plotSeptember2015/graphFoyerEpidemicKvigue.png", width = 700, height = 500, bg = "transparent")

## Dafra.
plotEachFoyerEpidemic(dfEpiDafra,2006)
dev.print(png, file = "../figs/plotSeptember2015/graphFoyerEpidemicDafra.png", width = 700, height = 500, bg = "transparent")


## Dandé.
plotEachFoyerEpidemic(dfEpiDande,2006)
dev.print(png, file = "../figs/plotSeptember2015/graphFoyerEpidemicDande.png", width = 700, height = 500, bg = "transparent")

##======================================================================================
## Some health center data are too noisy and may need smoothing before fitting the model 
## parameters
##======================================================================================

# identify health centre epidemic data that need smoothing.

interpol <- approxfun(time(dfEpiSeguenega),dfEpiSeguenega[,1],rule=2,method='constant')

smoothed.rain$data = sapply(smoothed.rain$time,interpol)

plot(coredata(dfEpiSeguenega[year(dfEpiSeguenega)==2006,1]),col='black',cex=2,log='',type="b")
lines(data~time,data=smoothed.rain,col='red')

#toto = mva_function(data = dfEpiHounde[year(dfEpiHounde)==2006],period=2)
#plotEachFoyerEpidemic(toto,2006)

