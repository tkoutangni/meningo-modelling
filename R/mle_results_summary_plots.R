rm(list = ls())
load("data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")


insert_age_structure = FALSE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")
source("R/mle_computeModelPredictAndPerformStats.R")

base_text = "for 64 health center-years with complete data, across four health distrits of \nBurkina faso (2004-2010)."


# pdf options

#pdf.options(width = 8.267, height = 11.692, family="Times")
#"figs/final_figs/mle_hyperendemic_models_estimations_plots_updated1.pdf"

pdf(file = paste(addSysDate("figs/final_figs/mle_hyperendemic_models_estimations_plots_update"), 'pdf', sep = '.'), 
    paper = "a4" , family = "Times", pointsize =12)

layout(1) # set graphical area for text display
add_text_to_plot(paste("Results of model estimations using a poisson maximum likelihood approach.",""),font_cex = 1, valign = "center", halign = "left")


layout(1) # set graphical area for text display
model_1_fig_text = paste('Figure 1. Model1-"inv" trajectories matching plots of simulated and observed (weekly) \ndata, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.


# compute and srore the model predicitons and performance stats
graphSettings(); is_a0Constant = FALSE
model_1_performence = mle_computeModelPredictionAndPerformStat(mle_a0ForcEstimates_matrice)

# Model 2 plottings
layout(1) # set graphical area for text display
model_1_fig_text = paste('Figure 2. Model2-"transm" trajectories matching plots of simulated and observed (weekly) \ndata, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.

graphSettings();is_a0Constant = TRUE;
model_2_performence = mle_computeModelPredictionAndPerformStat(mle_beta0ForcEstimates_matrice)

# Model 3 plottings
layout(1) # set graphical area for text display
model_1_fig_text = paste('Figure 3. Model3-"inv-transm" trajectories matching plots of simulated and observed \n(weekly) data, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.


graphSettings();is_a0Constant = FALSE;
model_3_performence = mle_computeModelPredictionAndPerformStat(mle_a0_beta0_ForcEstimates_matrice)

# Variations of the invasion and transmission rates. in model 1 and 2

# boxplot(cbind("Invasion" = log(model_1_performence$a_fold),
#               "Transmission" = log(model_2_performence$beta_fold)),
#         notch = FALSE,
#         ylab = "Fold increase(log)", main ="Model 1 and 2", las=1)
# writelabel("C", cex = 0.6)

# Variations of the invasion and transmission rates. in model 3

# boxplot(cbind("Invasion" = log(model_3_performence$a_fold[1:64]),
#               "Transmission" = log(model_3_performence$beta_fold)[1:64]), 
#         horizontal = FALSE, notch = FALSE, ylab = "Fold increase(log)", 
#         main ="Model 3", las=1)
# writelabel("D", cex = 0.6)


##==============================================
plotRates = FALSE

if(!plotRates){
  mle_a0ForcEstimates_matrice$alpha = (1/mle_a0ForcEstimates_matrice$alpha)/7
  mle_a0ForcEstimates_matrice$phi = (1/mle_a0ForcEstimates_matrice$phi)/year
  mle_a0ForcEstimates_matrice$teta = (mle_a0ForcEstimates_matrice$teta)/7
  
  mle_beta0ForcEstimates_matrice$alpha = (1/mle_beta0ForcEstimates_matrice$alpha)/7
  mle_beta0ForcEstimates_matrice$phi = (1/mle_beta0ForcEstimates_matrice$phi)/year
  mle_beta0ForcEstimates_matrice$teta = (mle_beta0ForcEstimates_matrice$teta)/7
  
  mle_a0_beta0_ForcEstimates_matrice$alpha = (1/mle_a0_beta0_ForcEstimates_matrice$alpha)/7
  mle_a0_beta0_ForcEstimates_matrice$phi = (1/mle_a0_beta0_ForcEstimates_matrice$phi)/year
  mle_a0_beta0_ForcEstimates_matrice$teta = (mle_a0_beta0_ForcEstimates_matrice$teta)/7
}

## adding the performance stats to the parameter estimate matrice
mle_a0ForcEstimates_matrice$Rsquared = model_1_performence$Rsquared
mle_a0ForcEstimates_matrice$RSR_stat = model_1_performence$RSR_stat
mle_a0ForcEstimates_matrice$PBIAS_stat = model_1_performence$PBIAS_stat


mle_beta0ForcEstimates_matrice$Rsquared = model_2_performence$Rsquared
mle_beta0ForcEstimates_matrice$RSR_stat = model_2_performence$RSR_stat
mle_beta0ForcEstimates_matrice$PBIAS_stat = model_2_performence$PBIAS_stat

mle_a0_beta0_ForcEstimates_matrice$Rsquared = model_3_performence$Rsquared
mle_a0_beta0_ForcEstimates_matrice$RSR_stat = model_3_performence$RSR_stat
mle_a0_beta0_ForcEstimates_matrice$PBIAS_stat = model_3_performence$PBIAS_stat

#mle_a0ForcEstimates_matrice$AICc =mle_a0ForcEstimates_matrice[-c(15,29,30,54),]
#mle_beta0ForcEstimates_matrice = mle_beta0ForcEstimates_matrice[-c(15,29,30,54),]
#mle_a0_beta0_ForcEstimates_matrice = mle_a0_beta0_ForcEstimates_matrice[-c(15,29,30,54),]

#log transform the AIC and BIC for plot
# mle_a0ForcEstimates_matrice$AICc = log(mle_a0ForcEstimates_matrice$AICc)
# mle_beta0ForcEstimates_matrice$AICc = log(mle_beta0ForcEstimates_matrice$AICc)
# mle_a0_beta0_ForcEstimates_matrice$AICc = log(mle_a0_beta0_ForcEstimates_matrice$AICc)
# 
# mle_a0ForcEstimates_matrice$BIC = log(mle_a0ForcEstimates_matrice$BIC)
# mle_beta0ForcEstimates_matrice$BIC = log(mle_beta0ForcEstimates_matrice$BIC)
# mle_a0_beta0_ForcEstimates_matrice$BIC = log(mle_a0_beta0_ForcEstimates_matrice$BIC)

library(reshape2)
mle_a0ForcEstimates_matrice.m <- melt(mle_a0ForcEstimates_matrice, id.var = "district")
mle_a0ForcEstimates_matrice.m$model = "model1-Inv"

mle_beta0ForcEstimates_matrice.m <- melt(mle_beta0ForcEstimates_matrice, id.var = "district")
mle_beta0ForcEstimates_matrice.m$model = "model2-Transm"

mle_a0_beta0_ForcEstimates_matrice.m <- melt(mle_a0_beta0_ForcEstimates_matrice, id.var = "district")
mle_a0_beta0_ForcEstimates_matrice.m$model = "model3-Inv-Transm"


# merging the reshaped estimates matrices
merged.estimates = rbind(mle_a0ForcEstimates_matrice.m,
                         mle_beta0ForcEstimates_matrice.m, 
                         mle_a0_beta0_ForcEstimates_matrice.m)

# choose parameters which distribution to draw (boxplot)
param_to_plot <- subset(merged.estimates, variable == 'beta0' | variable=='alpha'
                        | variable=='phi'| variable=='a0'| variable=='teta'
                        | variable=='epsilon_a'| variable=='epsilon_b'| variable=='Susc0'
                        | variable=='CarrierProp', 
                        select=c(model, variable, value))

# select performence stats to boxplot.

perform_stats_to_plot <- subset(merged.estimates, variable == 'Rsquared' | variable=='RSR_stat'
                                | variable=='PBIAS_stat' | variable=='AICc' | variable=='BIC',
                                select=c(model, variable, value))

# labels that we want to display on the plot facet wraps.
param_to_plot_labels<-c('Mean transmission /day',
                        'Carriage duration (weeks)',
                        'Imunity duration (years)',
                        'Initial susceptibles',
                        'Initial carriers',
                        'Peak time (week number)',
                        'Seasonal forcing of invasion',
                        'Mean invasion /day',
                        'Seasonal forcing of transmission')

param_to_plot$variable_label<-factor(param_to_plot$variable,labels=param_to_plot_labels)

#perform_stats_to_plot_labels<-c('AICc', 'BIC', 'Rsquared','RSR','PB(%)')
perform_stats_to_plot_labels<-c('Akaike information criterion (AICc)', 
'Bayesian information criterion (BIC)', 'Proportion of variance in data captured by each model',
'Root Mean Squared Error/Data standard deviation (RSR)','Percent Bias (PB)')


perform_stats_to_plot$variable_label<-factor(perform_stats_to_plot$variable,labels=perform_stats_to_plot_labels)

##================================================
require(ggplot2)
plot1<-ggplot(data = perform_stats_to_plot, aes(x=variable_label, y=value, fill=model)) + 
  geom_boxplot() + xlab("Performence Statistics")
plot1 <- plot1 + facet_wrap( ~ variable_label, scales="free",nrow = 3, labeller= label_wrap_gen(width = 25))
plot1 <- plot1 + xlab(" ") + ylab("Value") + ggtitle("Distribution of model performance stats across the 64 health center−years")
plot1 <- plot1 + guides(fill=guide_legend(title="Models"))
plot1 <- plot1 + my_theme_for_facet
#plot1

#plot1


plot2 <- ggplot(data = param_to_plot, aes(x=variable_label, y=value)) + 
  geom_boxplot(aes(fill=model))
# if you want color for points replace group with colour=Label
#plot2 <- plot2 + geom_point(aes(y=value, group=model), position = position_dodge(width=0.75))
plot2 <- plot2 + facet_wrap( ~ variable_label, scales="free",nrow = 4, labeller= label_wrap_gen(width = 20))
plot2 <- plot2 + xlab("") + ylab("Value") + ggtitle("Distribution of parameters estimates across the 64 health center−years")
plot2 <- plot2 + guides(fill=guide_legend(title="Models"))
plot2<-plot2 + my_theme_for_facet

#nf <- layout(matrix(c(1,1,2,2),nrow=2,ncol=2,byrow = TRUE), widths=c(2,1), heights=c(2,1), TRUE)
#layout.show(nf)
#multi_ggplot(plot2,plot1,cols=1)

layout(1) # set graphical area for text display
model_1_fig_text = paste('Figure 4. Distribution of parameters estimates and models performance \nstats, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.

layout(1)
graphSettings()
print(plot1)

layout(1)
graphSettings() # set the graphical parameters.
print(plot2)


#install.packages("gridExtra")
#library("gridExtra")
# grid.arrange(plot2, plot1,
#              ncol=1, nrow=2, widths=c(2), heights=c(2,1))

dev.off() # end of producing pdf

warning("\n A PDF file named: ", paste(addSysDate("mle_hyperendemic_models_estimations_plots_update"), 'pdf', sep = '.'), ' is created and stored in the folder: figs/final_figs')


# install.packages('cowplot')
# library('cowplot')
# ggdraw() +
#   draw_plot_label(c("A", "B"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 15)
#===================================================================================================================
# plotting results to PDF


