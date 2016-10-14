#source("check_path_to_working_directory.R")
source("loadFunctions.R")
load("../data/processed_data/new_processed_february_2016/a0ForcEstimates_matrice.RData")
load("../data/processed_data/new_processed_february_2016/beta0ForcEstimates_matrice.RData")
load("../data/processed_data/new_processed_february_2016/a0_beta0_ForcEstimates_matrice.RData")

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

graphSettings();is_a0Constant = TRUE;
model_2_performence = computeModelPredictionAndPerformStat(beta0ForcEstimates_matrice)

graphSettings();is_a0Constant = FALSE;
model_3_performence = computeModelPredictionAndPerformStat(a0_beta0_ForcEstimates_matrice)

###
# exemple visualisation of models qualitative performences based on 
# best and worse predictions of the models.

graphSettings();
#par(mfrow=c(3,2))
#par(mar=c(4.8, 9.1, 2.0, 9.1))
nf <- layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = F), respect = TRUE)
par(nf)
#layout(matrix(c(1,2,3,1,2,3), 3, 2, byrow = TRUE))

is_a0Constant = FALSE
quali_plot_data_model1 = a0ForcEstimates_matrice[c(11,64),]
quali_performence_model_1 = computeModelPredictionAndPerformStat(quali_plot_data_model1)
#writelabel('Model1-"inv"', cex = 0.6)
#title(main = 'Model1-"inv"')
mtext('Results1', side=3, line=20, outer=TRUE)
is_a0Constant = TRUE
quali_plot_data_model2 = beta0ForcEstimates_matrice[c(11,64),]
quali_performence_model_2 = computeModelPredictionAndPerformStat(quali_plot_data_model2)
#writelabel("M2", cex = 0.6)
#title(main = "Model2")
is_a0Constant = FALSE
quali_plot_data_model3 = a0_beta0_ForcEstimates_matrice[c(11,64),]
quali_performence_model_3 = computeModelPredictionAndPerformStat(quali_plot_data_model3)
#title(main = "Model3")
#writelabel("M3", cex = 0.6)

##==============================================
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


# Variations of the invasion and transmission rates. in model 1 and 2
boxplot(cbind("Invasion" = log(model_1_performence$a_fold),
              "Transmission" = log(model_2_performence$beta_fold)),
            notch = FALSE,
            ylab = "Fold increase(log)", main ="Model 1 and 2", las=1)
writelabel("C", cex = 0.6)

# Variations of the invasion and transmission rates. in model 3
boxplot(cbind("Invasion" = log(model_3_performence$a_fold[1:64]),
              "Transmission" = log(model_3_performence$beta_fold)[1:64]), 
        horizontal = FALSE, notch = FALSE, ylab = "Fold increase(log)", 
        main ="Model 3", las=1)
writelabel("D", cex = 0.6)

# Carriage summary
summary(model_1_performence$peakWeeklyCarriagePrev)*100
summary(model_2_performence$peakWeeklyCarriagePrev)*100
summary(model_3_performence$peakWeeklyCarriagePrev)*100

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


## adding the performance stats to the parameter estimate matrice
a0ForcEstimates_matrice$Rsquared = model_1_performence$Rsquared
a0ForcEstimates_matrice$RSR_stat = model_1_performence$RSR_stat
a0ForcEstimates_matrice$PBIAS_stat = model_1_performence$PBIAS_stat


beta0ForcEstimates_matrice$Rsquared = model_2_performence$Rsquared
beta0ForcEstimates_matrice$RSR_stat = model_2_performence$RSR_stat
beta0ForcEstimates_matrice$PBIAS_stat = model_2_performence$PBIAS_stat

a0_beta0_ForcEstimates_matrice$Rsquared = model_3_performence$Rsquared
a0_beta0_ForcEstimates_matrice$RSR_stat = model_3_performence$RSR_stat
a0_beta0_ForcEstimates_matrice$PBIAS_stat = model_3_performence$PBIAS_stat



library(reshape2)
a0ForcEstimates_matrice.m <- melt(a0ForcEstimates_matrice, id.var = "district")
a0ForcEstimates_matrice.m$model = "model1"

beta0ForcEstimates_matrice.m <- melt(beta0ForcEstimates_matrice, id.var = "district")
beta0ForcEstimates_matrice.m$model = "model2"

a0_beta0_ForcEstimates_matrice.m <- melt(a0_beta0_ForcEstimates_matrice, id.var = "district")
a0_beta0_ForcEstimates_matrice.m$model = "model3"



# merging the reshaped estimates matrices
merged.estimates = rbind(a0ForcEstimates_matrice.m,
                         beta0ForcEstimates_matrice.m, 
                         a0_beta0_ForcEstimates_matrice.m)

# choose parameters which distribution to draw (boxplot)
param_to_plot <- subset(merged.estimates, variable == 'beta0' | variable=='alpha'
                  | variable=='phi'| variable=='a0'| variable=='teta'
                  | variable=='epsilon_a'| variable=='epsilon_b'| variable=='Susc0'
                  | variable=='CarrierProp',
                  select=c(model, variable, value))

# select performence stats to boxplot.

perform_stats_to_plot <- subset(merged.estimates, variable == 'Rsquared' | variable=='RSR_stat'
                        | variable=='PBIAS_stat',
                        select=c(model, variable, value))

# labels that we want to display on the plot facet wraps.
param_to_plot_labels<-c('Mean transmission /day',
                        'Carriage duration (weeks)',
                        'Imunity duration (years)',
                        'Initial Suseptibles',
                        'Initial Carriers',
                        'Peak time (week number)',
                        'Invasion forc.amp.',
                        'Mean Invasion /day',
                        'Transmission forc.amp.')

param_to_plot$variable_label<-factor(param_to_plot$variable,labels=param_to_plot_labels)

perform_stats_to_plot_labels<-c('Rsquared','RSR','PB(%)')

perform_stats_to_plot$variable_label<-factor(perform_stats_to_plot$variable,labels=perform_stats_to_plot_labels)

##================================================
require(ggplot2)
plot1<-ggplot(data = perform_stats_to_plot, aes(x=variable_label, y=value, fill=model)) + 
geom_boxplot() + xlab("Performence Statistics")
plot1 <- plot1 + facet_wrap( ~ variable_label, scales="free",nrow = 1)
plot1 <- plot1 + xlab("") + ylab("Value") + ggtitle("")
plot1 <- plot1 + guides(fill=guide_legend(title="Models"))
plot1

plot2 <- ggplot(data = param_to_plot, aes(x=variable_label, y=value)) + 
    geom_boxplot(aes(fill=model))
# if you want color for points replace group with colour=Label
#plot2 <- plot2 + geom_point(aes(y=value, group=model), position = position_dodge(width=0.75))
plot2 <- plot2 + facet_wrap( ~ variable_label, scales="free",nrow = 4)
plot2 <- plot2 + xlab("") + ylab("Value") + ggtitle("")
plot2 <- plot2 + guides(fill=guide_legend(title="Models"))
plot2 # important to run until this line to show the plot.

nf <- layout(matrix(c(1,1,2,2),nrow=2,ncol=2,byrow = TRUE), widths=c(2,1), heights=c(1,2), TRUE)
layout.show(nf)
multi_ggplot(plot2,plot1,cols=1)


# Visualize the model prediction and data for each health centers
#by plotting mean and std of model prediction and data
#graphSettings();is_a0Constant = FALSE
#lolo = computeModelPredictionAndPerformStat(a0ForcEstimates_matrice)

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
              select=c(beta0,a0,phi,epsilon_a,year))#a0,epsilon_a,phi,
tib1$model<-"mode1"
tib2 = subset(beta0ForcEstimates_matrice,district==1&hc==6, 
              select=c(beta0,a0,phi,epsilon_b, year)) #,epsilon_b,phi
tib2$model<-"model2"

tib3 = subset(a0_beta0_ForcEstimates_matrice,district==1&hc==6, 
              select=c(beta0,a0,phi,epsilon_a, epsilon_b, year))
tib3$model<-"model3"

tib = rbind.fill(tib1,tib2,tib3)
graphSettings()
# Use the original data frame, but put factor() directly in the plot specification
p<-ggplot(data=tib, aes(x=factor(year), y=beta0*year, group=model, colour=model)) + #group=supp,
    geom_line() +
    geom_point()
p<- p + xlab("Year") +ylab("Mean transmission rate/year") + ggtitle("")


w2<-ggplot(data=tib, aes(x=factor(year), y=epsilon_b, group=model, colour=model)) + #group=supp,
    geom_line() +
    geom_point()
w2<- w2 + xlab("Year") +ylab("Transmission forc. amplitude") + ggtitle("")


w<-ggplot(data=tib, aes(x=factor(year), y=a0*year, group=model, colour=model)) + #group=supp,
    geom_line() +
    geom_point()
w<- w + xlab("Year") +ylab("Mean invasion rate/year") + ggtitle("")


w1<-ggplot(data=tib, aes(x=factor(year), y=epsilon_a, group=model, colour=model)) + #group=supp,
    geom_line() +
    geom_point()
w1<- w1 + xlab("Year") + ylab("Invasion forc. amplitude") + ggtitle("")

multi_ggplot(p, w, w2,w1,cols = 2)

# comparing the distributions of the parameters estimates
# across all three models

levels(param_to_plot$variable) # get the levels of the variable column in
# param_to_plot data.frame

mean_transmission_estimates = subset(param_to_plot, variable=='Mean transmission /day')
# perform the nonparametric distribution comparision test (the Kruskall walis test)
kruskal.test(value   ~ as.factor(model), data = mean_transmission_estimates)

mean_invasion_estimates = subset(param_to_plot, variable=="Mean Invasion /day")
# perform the nonparametric distribution comparision test (the Kruskall walis test)
kruskal.test(value   ~ as.factor(model), data = mean_invasion_estimates)

Imunity_duration_estimates_years = subset(param_to_plot, variable=="Imunity duration (years)")
# perform the nonparametric distribution comparision test (the Kruskall walis test)
kruskal.test(value   ~ as.factor(model), data = Imunity_duration_estimates_years)

peak_time_week_number = subset(param_to_plot, variable=="Peak time (week number)")
# perform the nonparametric distribution comparision test (the Kruskall walis test)
kruskal.test(value   ~ as.factor(model), data = peak_time_week_number)

invasion_forc_amp = subset(param_to_plot, variable=="Invasion forc.amp.")
# perform the nonparametric distribution comparision test (the Kruskall walis test)
kruskal.test(value   ~ as.factor(model), data = invasion_forc_amp)

transmission_forc_amp = subset(param_to_plot, variable=="Transmission forc.amp.")
# perform the nonparametric distribution comparision test (the Kruskall walis test)
kruskal.test(value   ~ as.factor(model), data = transmission_forc_amp)


##================================================
#SAME ANALYSIS AS ABOVE BUT ONLY USING THE TRESHOLD OF
#50/100,000
##================================================



# selction health centres that crossed a treshold of 50/100,000 in  a given year.

a0ForcEstimates_matrice[grep(pattern = "berenga",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern = "goubr",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern = "maro",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern ="gomb",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern ="sebedougou",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern ="wakuy",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern ="balla",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern ="kwekwesso",rownames(a0ForcEstimates_matrice)),]
a0ForcEstimates_matrice[grep(pattern ="tiarako",rownames(a0ForcEstimates_matrice)),]

# selecting the corresponding year to exclude from the parameters estimates matrice.


subset(a0ForcEstimates_matrice[grep(pattern = "berenga",rownames(a0ForcEstimates_matrice)),], district==1&year==2010)

subset(a0ForcEstimates_matrice[grep(pattern = "goubr",rownames(a0ForcEstimates_matrice)),], district==1&year==2010)

subset(a0ForcEstimates_matrice[grep(pattern = "maro",rownames(a0ForcEstimates_matrice)),], district==2&year==2006)

subset(a0ForcEstimates_matrice[grep(pattern = "gomb",rownames(a0ForcEstimates_matrice)),], district==2&year==2006)

subset(a0ForcEstimates_matrice[grep(pattern = "sebedougou",rownames(a0ForcEstimates_matrice)),], district==2&year==2008)

subset(a0ForcEstimates_matrice[grep(pattern = "wakuy",rownames(a0ForcEstimates_matrice)),], district==2&year==2006)


subset(a0ForcEstimates_matrice[grep(pattern = "balla",rownames(a0ForcEstimates_matrice)),], district==3&year==2006)


subset(a0ForcEstimates_matrice[grep(pattern = "kwekwesso",rownames(a0ForcEstimates_matrice)),], district==3&year==2006)


subset(a0ForcEstimates_matrice[grep(pattern = "tiarako",rownames(a0ForcEstimates_matrice)),], district==3&year==2006)


# removing the health centers selected above from the matrice of parameter values


a0ForcEstimates_matrice = subset(a0ForcEstimates_matrice, !(district==1&year==2010&hc==2 | district==1&year==2010&hc==5| district==2&year==2006&hc==5|district==2&year==2006&hc==10| district==2 & year==2006 &hc==14|
                                      district==2&year==2008&hc==16| district==3&year==2006&hc==1| district==3&year==2006&hc==2| district==3&year==2006&hc==4))

beta0ForcEstimates_matrice = subset(beta0ForcEstimates_matrice, !(district==1&year==2010&hc==2 | district==1&year==2010&hc==5| district==2&year==2006&hc==5|district==2&year==2006&hc==10| district==2 & year==2006 &hc==14|
                                         district==2&year==2008&hc==16| district==3&year==2006&hc==1| district==3&year==2006&hc==2| district==3&year==2006&hc==4))

a0_beta0_ForcEstimates_matrice = subset(a0_beta0_ForcEstimates_matrice, !(district==1&year==2010&hc==2 | district==1&year==2010&hc==5| district==2&year==2006&hc==5|district==2&year==2006&hc==10| district==2 & year==2006 &hc==14|
                                             district==2&year==2008&hc==16| district==3&year==2006&hc==1| district==3&year==2006&hc==2| district==3&year==2006&hc==4))

# Now rerun the analysis as above.
