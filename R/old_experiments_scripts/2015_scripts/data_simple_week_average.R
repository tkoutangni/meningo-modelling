
source("data.cleaning.R") # get the cleaned data.
source("plotfunctions.R")


# Function to compute the mean weekly incidence data
# takes dataframe as argument

row.mean<-function(df){
    row.mean<-numeric(0)
    for(i in seq_along(df[,1])){
        row.mean[i] = mean(as.numeric(coredata(df[i,])),na.rm=TRUE)
    }
    return(row.mean)
}

# Function to compute standard deviation of the mean weekly incidence data.
# takes dataframe as argument.

row.std<-function(df){
    std<-numeric(0)
    for(i in seq_along(df[,1])){
        std[i] = sd(as.numeric(coredata(df[i,])),na.rm=TRUE)
    }
    return(std)
}

# Function to compute the 95% CI of the mean weekly incidence data for each calendar week
# takes dataframe as argument
## 95% CI computation assuming a gaussian distribution of weekly incidence case report.
ci_95<-function(df){
    error = qnorm(0.975)*row.std(df)/sqrt(dim(df)[2])
    lbound = row.mean(df)-error
    hbound = row.mean(df)+error
    data = cbind(lbound, mean=row.mean(df), hbound)
    return(data)
}


## graphical display parameters.
par(pch=20, las=1, cex=1)
#layout(matrix(c(1, 1, 1, 1), 2, 2, byrow = TRUE),widths=c(3, 3), heights=c(2, 2)) # 1 rows 1 column
#layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE),widths=c(3, 3), heights=c(2, 2)) # 2 rows 1 column
#layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE),widths=c(3, 3), heights=c(2, 2)) # 2 rows 2 column

########################################################################
# This script aims at smoothing out the noise in data at individual health center
# by computing the average or median and sd of the weekly incidence data curve in 
# non epidemic and epidemic situations in a given district.
# 
#  Deux possiblilités: 
#    - Le calcul peut se faire sur les fs n'ayant pas connu de foyer
#    - Il peut egalement se faire sur les années non epidemic des series avec au moins 1 foyer
#
#######################################################################



#==========================
### SEGUENEGA DISTRICT
#==========================
# checking the class of variables in the data.frame
cbind(sapply(incid.menin[,district.index$seguenega][,group_fs_seguenega$non_fe_fs.index], class)) # To see the class of all column in the data.frame
# visualizing non_epidemic health centers
incid_plot(incid.menin[,district.index$seguenega][,group_fs_seguenega$non_fe_fs.index]) 



png(file="../figs/plot-march/segenega_mean_incid.png",width=800,height=600,res=100)
par(mar=c(5,7,4,2))
par(mfrow=c(2,2))
layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = FALSE),widths=c(3, 3), heights=c(2, 2)) # 2 rows 2 column
plot.mean.weekly(df.no.epi,title="Segunega Health Centres \n with no outbreak")
plot.mean.weekly(df.epi,title="Segunega Health Centres \n with outbreak(s)")
dev.off()


# subseting from 2006 to 2010
plot.mean.weekly(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010],title="Segunega Health Centres \n with no outbreak")
plot(row.mean(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010]*1e+05),col="black",ylab="Average Weekly \n Incidence",xlab="Calendar Weeks")
title=(ylab="Average Weekly \n Incidence")
mtext(side=3,adj=0,expression(paste("x",10^-5)),cex=0.8)

# time serie for use in models fittings (subseting from 2006 to 2010)

wdat.seguen<-zoo(
    row.mean(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010]),
    order.by=time(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010])
    )
wdat.seguen.core=coredata(wdat.seguen)

julian_day = zoo(
    numeric(0),
    order.by=time(df.no.epi[year(time(df.no.epi))>=2006&year(time(df.no.epi))<=2010])
)

wdat.seguen=merge.zoo(wdat.seguen,julian_day)

#==========================
### DO DISTRICT
#==========================
# checking the class of variables in the data.frame
cbind(sapply(incid.menin[,district.index$do][,group_fs_do$non_fe_fs.index], class)) # To see the class of all column in the data.frame

# visualizing non_epidemic health centers
incid_plot(incid.menin[,district.index$do][,group_fs_do$non_fe_fs.index]) 

df.no.epi.do = incid.menin[,district.index$do][,group_fs_do$non_fe_fs.index]
df.epi.do = incid.menin[,district.index$do][,group_fs_do$fe_fs_index]

png(file="../figs/plot-march/do_mean_incid.png",width=800,height=600,res=100)
par(mar=c(5,7,4,2))
par(mfrow=c(2,2))
layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE),widths=c(3, 3), heights=c(2, 2)) # 2 rows 2 column
plot.mean.weekly(df.no.epi.do,title="DÔ Health Centres \n with no outbreak")
#plot.mean.weekly(df.epi.do,title="DÔ Health Centres \n with outbreak(s)")
dev.off()

#===================================================================================

# instead of a simple weekly average we can also smooth out noise by computing a
# mooving average.
# Exemple using rollapply function from zoo package. Another R script is dedicated
# to the mooving average computation.

# incid_plot(rollapply(incid.menin[,district.index$seguenega][,group_fs_seguenega$fe_fs_index],2,mean))


