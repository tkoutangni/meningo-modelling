source("myFunctions.R",verbose = FALSE)
source("yearSpecificFittingAlgo_nov_2015.R",verbose = FALSE)
load("../data/processed_data/new_processed_data/a0ForcEstimates_matrice.RData")
load("../data/processed_data/new_processed_data/beta0ForcEstimates_matrice.RData")
load("../data/processed_data/new_processed_data/a0_beta0_ForcEstimates_matrice.RData")
library(reshape2)
#dev.off() # clear all graphical devices and plots in the plotting area.
 
graphSettings()
# find index of health centers that are in a0ForcEstimates_matrice but are missing
# in beta0ForcEstimates_matrice and a0_beta0_ForcEstimates_matrice
# the goal is to make sure we compare all three model based on the same health centers.

missing_hc = c(setdiff(row.names(beta0ForcEstimates_matrice),row.names(a0ForcEstimates_matrice)))
#setdiff(row.names(a0_beta0_ForcEstimates_matrice),row.names(a0ForcEstimates_matrice))
# only two health center are missing. Otherwise all other health centers are shared accross
# all parameters estimation matrices.

# remoove the missing health centers in a0ForcEstimates_matrice from beta0ForcEstimates_matrice
# and a0_beta0_ForcEstimates_matrice.

beta0ForcEstimates_matrice = beta0ForcEstimates_matrice[which(row.names(beta0ForcEstimates_matrice)!=missing_hc),]
a0_beta0_ForcEstimates_matrice = a0_beta0_ForcEstimates_matrice[which(row.names(a0_beta0_ForcEstimates_matrice)!=missing_hc),]

# compute and srore the model predicitons and performance stats
graphSettings(); is_a0Constant = FALSE
model_1_performence = computeModelPredictionAndPerformStat(a0ForcEstimates_matrice)

is_a0Constant = TRUE;
model_2_performence = computeModelPredictionAndPerformStat(beta0ForcEstimates_matrice)

is_a0Constant = FALSE;
model_3_performence = computeModelPredictionAndPerformStat(a0_beta0_ForcEstimates_matrice)

###

# run the model performance only for the 42 health centre year with positive R squared.

#hc_with_positive_Rsquared = row.names(a0ForcEstimates_matrice[which(model_1_performence$Rsquared>0.5),])

#graphSettings(); is_a0Constant = FALSE;
#test0 =  computeModelPredictionAndPerformStat(a0ForcEstimates_matrice[hc_with_positive_Rsquared,])


#graphSettings(); is_a0Constant = TRUE;
#test1 =  computeModelPredictionAndPerformStat(beta0ForcEstimates_matrice[hc_with_positive_Rsquared,])

#graphSettings(); is_a0Constant = FALSE;
#test2 =  computeModelPredictionAndPerformStat(a0_beta0_ForcEstimates_matrice[hc_with_positive_Rsquared,])

sapply(list(model_1_performence,model_2_performence,model_3_performence),
       sumarize_performance_stat)

#percentiles(model_3_performence$Rsquared, c(.25, .95))
quantile(model_3_performence$Rsquared)[c(2:4)]

# carriage predictions summary for model 1.
mean(model_1_performence$carriageRange[,"Carriagemin"])*100
mean(model_1_performence$carriageRange[,"Carriagemax"])*100
sapply(model_1_performence$PredictedCarriageList,mean)*100
mean(sapply(model_1_performence$PredictedCarriageList,mean)*100)
sd(sapply(model_1_performence$PredictedCarriageList,mean)*100)


# carriage predictions summary for model 2.
mean(model_2_performence$carriageRange[,"Carriagemin"])*100
mean(model_2_performence$carriageRange[,"Carriagemax"])*100
#sapply(model_2_performence$PredictedCarriageList,mean)*100
mean(sapply(model_2_performence$PredictedCarriageList,mean)*100)
sd(sapply(model_2_performence$PredictedCarriageList,mean)*100)


# carriage predictions summary for model 3.
mean(model_3_performence$carriageRange[,"Carriagemin"])*100
mean(model_3_performence$carriageRange[,"Carriagemax"])*100
#sapply(model_3_performence$PredictedCarriageList,mean)*100
mean(sapply(model_3_performence$PredictedCarriageList,mean)*100)
sd(sapply(model_3_performence$PredictedCarriageList,mean)*100)


#Peak weakly carriage prevalence summary statistics:
mean(model_1_performence$peakWeeklyCarriagePrev)*100
sd(model_1_performence$peakWeeklyCarriagePrev)*100
mean(model_2_performence$peakWeeklyCarriagePrev)*100
sd(model_2_performence$peakWeeklyCarriagePrev)*100
mean(model_3_performence$peakWeeklyCarriagePrev)*100
sd(model_3_performence$peakWeeklyCarriagePrev)*100

graphSettings()
boxplot(cbind(Model1 = sapply(model_1_performence$PredictedCarriageList,mean)*100,
              Model2 = sapply(model_2_performence$PredictedCarriageList,mean)*100,
              Model3 = sapply(model_3_performence$PredictedCarriageList,mean)*100), 
        ylab="Weekly Carriage Prev (%)", main="Annual Mean",horizontal=FALSE,notch=TRUE, las=1)
writelabel("A", cex = 0.6)
boxplot(cbind(Model1 = model_1_performence$peakWeeklyCarriagePrev*100,
          Model2 = model_2_performence$peakWeeklyCarriagePrev*100,
          Model3 = model_3_performence$peakWeeklyCarriagePrev*100), 
        ylab="Weekly Carriage Prev (%)",main="Annual Peak",
        horizontal=FALSE,notch=TRUE, las=1)
writelabel("B", cex = 0.6)


summary(model_1_performence$peakWeeklyCarriagePrev)*100
summary(model_2_performence$peakWeeklyCarriagePrev)*100
summary(model_3_performence$peakWeeklyCarriagePrev)*100

# Variations of the invasion and transmission rates. in model 1 and 2
boxplot(cbind("Invasion" = log(model_1_performence$a_fold),
              "Transmission" = log(model_2_performence$beta_fold)),
        ylab = "Fold increase (log)", main ="Model 1 and 2", las=1)
writelabel("C", cex = 0.6)

# Variations of the invasion and transmission rates. in model 3
boxplot(cbind("Invasion" = log(model_3_performence$a_fold[1:64]),
              "Transmission" = log(model_3_performence$beta_fold)[1:64]), 
        horizontal = FALSE, notch = FALSE, ylab = "Fold increase (log)", 
        main ="Model 3", las=1)
writelabel("D", cex = 0.6)
# incidence summary

##==============================================
plotRates = FALSE

if(!plotRates){
    a0ForcEstimates_matrice$alpha = (1/a0ForcEstimates_matrice$alpha)/7
    a0ForcEstimates_matrice$phi = (1/a0ForcEstimates_matrice$phi)/year
    a0ForcEstimates_matrice$teta = (a0ForcEstimates_matrice$teta)/7
    
    beta0ForcEstimates_matrice$alpha = (1/beta0ForcEstimates_matrice$alpha)/7
    beta0ForcEstimates_matrice$phi = (1/beta0ForcEstimates_matrice$phi)/year
    beta0ForcEstimates_matrice$teta = (beta0ForcEstimates_matrice$teta)/7
    
    a0_beta0_ForcEstimates_matrice$alpha = (1/a0_beta0_ForcEstimates_matrice$alpha)/7
    a0_beta0_ForcEstimates_matrice$phi = (1/a0_beta0_ForcEstimates_matrice$phi)/year
    a0_beta0_ForcEstimates_matrice$teta = (a0_beta0_ForcEstimates_matrice$teta)/7
}

a0ForcEstimates_matrice.m <- melt(a0ForcEstimates_matrice, id.var = "district")
a0ForcEstimates_matrice.m$model = "model1"

a0_beta0_ForcEstimates_matrice.m <- melt(a0_beta0_ForcEstimates_matrice, id.var = "district")
a0_beta0_ForcEstimates_matrice.m$model = "model3"


beta0ForcEstimates_matrice.m <- melt(beta0ForcEstimates_matrice, id.var = "district")
beta0ForcEstimates_matrice.m$model = "model2"


merged.estimates = rbind(a0ForcEstimates_matrice.m,
                         beta0ForcEstimates_matrice.m, 
                         a0_beta0_ForcEstimates_matrice.m)
newdata <- subset(merged.estimates, variable == 'beta0' | variable=='alpha'
                  | variable=='phi'| variable=='a0'| variable=='teta'
                  | variable=='epsilon_a'| variable=='epsilon_b'| variable=='Susc0'
                  | variable=='CarrierProp',
                  select=c(model, variable, value))



if(plotRates){
    levels(newdata$variable)<-c("hc","year", "Mean Transmission Rate/day",
                                'Carriage Loss Rate/day','Imunity Loss Rate',
                                'Initial Suseptibles Prop', 'Initial Carriers Prop',
                                'Peak time (days)','Invasion forc.amp.',
                                'Mean Invasion /day',"f_val","Transmission forc.amp.")
    
}else{
    levels(newdata$variable)<-c("hc","year", "Mean transmission /day",
                                'Carriage duration (weeks)','Imunity duration (years)',
                                'Initial Suseptibles Prop', 'Initial Carriers Prop',
                                'Peak time (week number)','Invasion forc.amp.',
                                'Mean Invasion /day',"f_val","Transmission forc.amp.")
}


require(ggplot2)
# simple boxplot
#ggplot(data = newdata, aes(x=variable, y=value)) + geom_boxplot(aes(fill=model))
# boxplot with more costumized options.
p <- ggplot(data = newdata, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=model))
# if you want color for points replace group with colour=Label
p <- p + geom_point(aes(y=value, group=model), position = position_dodge(width=0.75))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + xlab("Boxplot (colored) and estimated values(dots) are plotted") + ylab("Value") + ggtitle("Estimated parameters values distribution\n accross the 64 health centers-years of 4 sanitary districts, Burkina-Faso")
                                                                
p <- p + guides(fill=guide_legend(title="Models"))
p # important to run until this line to show the plot.



##================================================

# Visualize the model prediction and data for each health centers
#by plotting mean and std of model prediction and data
graphSettings();is_a0Constant = FALSE
lolo = computeModelPredictionAndPerformStat(a0ForcEstimates_matrice)

installPackage("Rmisc")
require("Rmisc")


summary_SE = function(){
    summary_table = matrix(NA,ncol=6,nrow = 64)
    colnames(summary_table)<-c('.id','N','mean','sd','se','ci')
    for (i in 1:64){
        values = as.numeric(summarySE(data.frame("1" = unlist(lolo$PredictedIncidList[i])),measurevar = "X1"))
        summary_table[i,] = values
    }
    
    return(summary_table)
}


compute_summary_table = as.data.frame(summary_SE())
compute_summary_table$.id <- seq(1,64)
compute_summary_table$mean<-compute_summary_table$mean*1e+05
compute_summary_table$se<-compute_summary_table$se*1e+05
compute_summary_table$sd<-compute_summary_table$sd*1e+05
compute_summary_table$ci<-compute_summary_table$ci*1e+05



dev.off()
graphSettings()
par(mfrow=c(2,1))

x = 1:64*2-1
CI.up = as.numeric(compute_summary_table$mean)+as.numeric(compute_summary_table$se)
CI.dn = as.numeric(compute_summary_table$mean)-as.numeric(compute_summary_table$se)

plot(compute_summary_table$mean~x,xaxt='n',
     ylim=c(min(CI.dn),max(CI.up)),
     xlab='Health centre-years',ylab='Week Incid. \n per 10000', main='Model 1',
     col='black',pch=16)
axis(1, at=x, labels=names)
arrows(x,CI.dn,x,CI.up,code=3,length=0.1,angle=90,col='black')
#legend("bottomleft",paste(names,": S.E=",data$se),ncol=6,text.width=1)


tib1 = subset(a0ForcEstimates_matrice,district==1&hc==6, 
       select=c(beta0,a0,phi,year))#a0,epsilon_a,phi,
tib1$model<-"mode1"
tib2 = subset(beta0ForcEstimates_matrice,district==1&hc==6, 
                    select=c(beta0,a0,phi,year)) #,epsilon_b,phi
tib2$model<-"model2"

tib3 = subset(a0_beta0_ForcEstimates_matrice,district==1&hc==6, 
             select=c(beta0,a0,phi,year))
tib3$model<-"model3"

tib = rbind(tib1,tib2,tib3)
graphSettings()
# Use the original data frame, but put factor() directly in the plot specification
p<-ggplot(data=tib, aes(x=factor(year), y=beta0, group=model, colour=model)) + #group=supp,
    geom_line() +
    geom_point()
p<- p + xlab("Year") +ylab("Mean transmission rate/day") + ggtitle("Annual variation of the mean transmission")
p


w<-ggplot(data=tib, aes(x=factor(year), y=a0, group=model, colour=model)) + #group=supp,
    geom_line() +
    geom_point()
w<- w + xlab("Year") +ylab("Mean invasion rate/day") + ggtitle("Annual variation of the mean invasion")
w

z<-ggplot(data=tib, aes(x=factor(year), y=phi, group=model, colour=model)) + #group=supp,
    geom_line() +
    geom_point()
z<- z + xlab("Year") +ylab("Imunity loss") + ggtitle("Annual variation of Imunity loss")
z
