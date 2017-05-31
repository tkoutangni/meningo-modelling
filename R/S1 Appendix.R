# Appendix A figs
rm(list = ls())
load("data/processed_data/new_processed_february_2016/mle_age_str_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_a0_beta0_ForcEstimates_matrice.RData")

insert_age_structure = TRUE; heterogenous_mixing=TRUE
source("R/run_first.R")
source("R/models_parameters.R")
source("R/mle_age_srt_computeModelPredictAndPerformStats_modif.R")

# Store the estimates for each model in a variable to use later for trajectories matching plot
model_1_params_estimates = mle_age_str_a0ForcEstimates_matrice
model_2_params_estimates = mle_age_str_beta0ForcEstimates_matrice
model_3_params_estimates = mle_age_str_a0_beta0_ForcEstimates_matrice
#============================================================================
# get names of the parameters estimates matrice columns
parm_matrice_colnames = colnames(mle_age_str_a0_beta0_ForcEstimates_matrice)
susc_colnames = parm_matrice_colnames[grep("Susc", parm_matrice_colnames)]
carrier_colnames = parm_matrice_colnames[grep("Carrier", parm_matrice_colnames)]

# compute and srore the model predicitons and performance stats
graphSettings(); is_a0Constant = FALSE
model_1_performence = mle_age_str_computeModelPredictionAndPerformStat(mle_age_str_a0ForcEstimates_matrice)


graphSettings();is_a0Constant = TRUE;
model_2_performence = mle_age_str_computeModelPredictionAndPerformStat(mle_age_str_beta0ForcEstimates_matrice)

graphSettings();is_a0Constant = FALSE;
model_3_performence = mle_age_str_computeModelPredictionAndPerformStat(mle_age_str_a0_beta0_ForcEstimates_matrice)

## adding the performance stats to the parameter estimate matrice
mle_age_str_a0ForcEstimates_matrice$Rsquared = model_1_performence$Rsquared
mle_age_str_a0ForcEstimates_matrice$RSR_stat = model_1_performence$RSR_stat
mle_age_str_a0ForcEstimates_matrice$PBIAS_stat = model_1_performence$PBIAS_stat


mle_age_str_beta0ForcEstimates_matrice$Rsquared = model_2_performence$Rsquared
mle_age_str_beta0ForcEstimates_matrice$RSR_stat = model_2_performence$RSR_stat
mle_age_str_beta0ForcEstimates_matrice$PBIAS_stat = model_2_performence$PBIAS_stat

mle_age_str_a0_beta0_ForcEstimates_matrice$Rsquared = model_3_performence$Rsquared
mle_age_str_a0_beta0_ForcEstimates_matrice$RSR_stat = model_3_performence$RSR_stat
mle_age_str_a0_beta0_ForcEstimates_matrice$PBIAS_stat = model_3_performence$PBIAS_stat



##==============================================
plotRates = FALSE

if(!plotRates){
        mle_age_str_a0ForcEstimates_matrice$alpha = (1/mle_age_str_a0ForcEstimates_matrice$alpha)/7
        mle_age_str_a0ForcEstimates_matrice$phi = (1/mle_age_str_a0ForcEstimates_matrice$phi)/year
        mle_age_str_a0ForcEstimates_matrice$teta = (mle_age_str_a0ForcEstimates_matrice$teta)/7
        mle_age_str_a0ForcEstimates_matrice[,susc_colnames[1]] = mle_age_str_a0ForcEstimates_matrice[,susc_colnames[1]]/1000
        mle_age_str_a0ForcEstimates_matrice[,susc_colnames[2]] = mle_age_str_a0ForcEstimates_matrice[,susc_colnames[2]]/1000
        mle_age_str_a0ForcEstimates_matrice[,susc_colnames[3]] = mle_age_str_a0ForcEstimates_matrice[,susc_colnames[3]]/1000
        mle_age_str_a0ForcEstimates_matrice[,susc_colnames[4]] = mle_age_str_a0ForcEstimates_matrice[,susc_colnames[4]]/1000
        mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[1]] = mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[1]]/100
        mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[2]] = mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[2]]/100
        mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[3]] = mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[3]]/100
        mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[4]] = mle_age_str_a0ForcEstimates_matrice[,carrier_colnames[4]]/100
        
        mle_age_str_beta0ForcEstimates_matrice$alpha = (1/mle_age_str_beta0ForcEstimates_matrice$alpha)/7
        mle_age_str_beta0ForcEstimates_matrice$phi = (1/mle_age_str_beta0ForcEstimates_matrice$phi)/year
        mle_age_str_beta0ForcEstimates_matrice$teta = (mle_age_str_beta0ForcEstimates_matrice$teta)/7
        mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[1]] = mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[1]]/1000
        mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[2]] = mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[2]]/1000
        mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[3]] = mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[3]]/1000
        mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[4]] = mle_age_str_beta0ForcEstimates_matrice[,susc_colnames[4]]/1000
        mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[1]] = mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[1]]/100
        mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[2]] = mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[2]]/100
        mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[3]] = mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[3]]/100
        mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[4]] = mle_age_str_beta0ForcEstimates_matrice[,carrier_colnames[4]]/100
        
        mle_age_str_a0_beta0_ForcEstimates_matrice$alpha = (1/mle_age_str_a0_beta0_ForcEstimates_matrice$alpha)/7
        mle_age_str_a0_beta0_ForcEstimates_matrice$phi = (1/mle_age_str_a0_beta0_ForcEstimates_matrice$phi)/year
        mle_age_str_a0_beta0_ForcEstimates_matrice$teta = (mle_age_str_a0_beta0_ForcEstimates_matrice$teta)/7
        
        mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[1]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[1]]/1000
        mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[2]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[2]]/1000
        mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[3]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[3]]/1000
        mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[4]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,susc_colnames[4]]/1000
        mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[1]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[1]]/100
        mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[2]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[2]]/100
        mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[3]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[3]]/100
        mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[4]] = mle_age_str_a0_beta0_ForcEstimates_matrice[,carrier_colnames[4]]/100
}

## prepared some data in ggplot format
library(reshape2)
mle_age_str_a0ForcEstimates_matrice.m <- melt(mle_age_str_a0ForcEstimates_matrice, id.var = "district")
mle_age_str_a0ForcEstimates_matrice.m$model = "model1-Inv    "

mle_age_str_beta0ForcEstimates_matrice.m <- melt(mle_age_str_beta0ForcEstimates_matrice, id.var = "district")
mle_age_str_beta0ForcEstimates_matrice.m$model = "model2-Transm    "

mle_age_str_a0_beta0_ForcEstimates_matrice.m <- melt(mle_age_str_a0_beta0_ForcEstimates_matrice, id.var = "district")
mle_age_str_a0_beta0_ForcEstimates_matrice.m$model = "model3-Inv-Transm    "


# merging the reshaped estimates matrices
merged.estimates = rbind(mle_age_str_a0ForcEstimates_matrice.m,
                         mle_age_str_beta0ForcEstimates_matrice.m, 
                         mle_age_str_a0_beta0_ForcEstimates_matrice.m)

# choose parameters which distribution to plot (boxplot)



param_to_plot <- subset(merged.estimates, variable == 'beta0' | variable=='alpha'
                        | variable=='phi'| variable=='a0'| variable=='teta'
                        | variable=='epsilon_a'| variable=='epsilon_b'| variable == susc_colnames[1]
                        | variable == susc_colnames[2] | variable == susc_colnames[3] | variable == susc_colnames[4]
                        | variable==carrier_colnames[1] | variable==carrier_colnames[2] | variable==carrier_colnames[3]
                        | variable==carrier_colnames[4], 
                        select=c(model, variable, value))

# select performence stats to boxplot.

perform_stats_to_plot <- subset(merged.estimates, variable == 'Rsquared' | variable=='RSR_stat'
                                | variable=='PBIAS_stat' | variable=='AICc', #| variable=='BIC',
                                select=c(model, variable, value))

# labels that we want to display on the plot facet wraps.

param_to_plot_labels<-c('Mean transmission/day',
                        'Carriage duration (weeks)',
                        'Imunity duration (years)',
                        'Initial susceptibles/1000 (<5 years)',
                        'Initial susceptibles/1000 (5-12 years)',
                        'Initial susceptibles/1000 (13-19 years)',
                        'Initial susceptibles/1000 (20+ years)',
                        'Initial carriers/100 (<5 years)',
                        'Initial carriers/100 (5-12 years)',
                        'Initial carriers/100 (13-19 years)',
                        'Initial carriers/100 (20+ years)',
                        'Peak time (week number)',
                        'Forcing amplitude for invasion',
                        'Mean invasion /day',
                        'Forcing amplitude for transmission')

param_to_plot$variable_label<-factor(param_to_plot$variable,labels=param_to_plot_labels)

perform_stats_to_plot_labels<-c('Akaike information criterion (AIC)', 
                                #'Bayesian information criterion (BIC)', 
                                'Proportion of variance in data explained by each model',
                                'Root Mean Squared Error/Data standard deviation (RSR)',
                                'Percent Bias (PB)')

perform_stats_to_plot$variable_label<-factor(perform_stats_to_plot$variable,labels=perform_stats_to_plot_labels)





###===========================PLOT START========================


pdf(file = paste(addSysDate("figs/final_figs/mle_age_str_mle_hyperendemic_models_estimations_plots"), 'pdf', sep = '.'), paper = "special" , family = "Times", pointsize =12)


layout(1) # set graphical area for text display
add_text_to_plot(paste("S1 Appendix. Resutls of simulations of the SCIRS age-structured models.",""),font_cex = 1, valign = "center", halign = "left")

##====================SUMMARY PLOTS==================
require(ggplot2)
plot1<-ggplot(data = perform_stats_to_plot, aes(x=variable_label, y=value, fill=model)) + 
        geom_boxplot() + xlab("Performence Statistics")
plot1 <- plot1 + facet_wrap( ~ variable_label, scales="free",nrow = 2, labeller= label_wrap_gen(width = 25))
plot1 <- plot1 + xlab(" ") + ylab("Value") + ggtitle("(A) Distribution of age-structured models performance stats")
plot1 <- plot1 + guides(fill=guide_legend(title=" "))
plot1 <- plot1 + my_theme_for_facet
plot1<- plot1 + scale_fill_manual(name = "",
                                  values = c("#ffffff", "#bdbdbd", "#636363"),
                                  labels = c("Model1-Inv    ", "Model2-Transm    ",
                                             "Model3-Inv-Transm    ")
)
#plot1


plot2 <- ggplot(data = param_to_plot, aes(x=variable_label, y=value)) + 
        geom_boxplot(aes(fill=model))
# if you want color for points replace group with colour=Label
#plot2 <- plot2 + geom_point(aes(y=value, group=model), position = position_dodge(width=0.75))
plot2 <- plot2 + facet_wrap( ~ variable_label, scales="free",nrow = 6, labeller= label_wrap_gen(width = 20))
plot2 <- plot2 + xlab("") + ylab("Value") + ggtitle("(B) Distribution of parameters estimates accross all health center years")
plot2 <- plot2 + guides(fill=guide_legend(title=""))
plot2<-plot2 + my_theme_for_facet
plot2<- plot2 + scale_fill_manual(name = "",
                                  values = c("#ffffff", "#bdbdbd", "#636363"),
                                  labels = c("Model1-Inv    ", "Model2-Transm    ",
                                             "Model3-Inv-Transm    ")
)
#plot2 # important to run until this line to show the plot.

#nf <- layout(matrix(c(1,1,2,2),nrow=2,ncol=2,byrow = TRUE), widths=c(2,1), heights=c(2,1), TRUE)
#layout.show(nf)
#multi_ggplot(plot2,plot1,cols=1)



# layout(1) # set graphical area for text display
# model_1_fig_text = paste('(D) Distribution of parameters estimates and models performance \nstats, ',base_text)
# add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
# graphSettings() # set the graphical parameters.

layout(1)
graphSettings()
print(plot1)

layout(1)
graphSettings() # set the graphical parameters.
print(plot2)


###==========================ADD PLOT FOR TRAJECTORIES MATCHINGS========================


base_text = "for 64 health center-years with complete data, accross four health distrits of \nBurkina faso (2004-2010)."


layout(1) # set graphical area for text display
model_1_fig_text = paste('(C) Model1-"inv" trajectories matching plots of simulated and observed (weekly) \ndata, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.


# compute and srore the model predicitons and performance stats
graphSettings(); is_a0Constant = FALSE
mle_age_str_computeModelPredictionAndPerformStat(model_1_params_estimates)

# Model 2 plottings
layout(1) # set graphical area for text display
model_1_fig_text = paste('(D) Model2-"transm" trajectories matching plots of simulated and observed (weekly) \ndata, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.

graphSettings();is_a0Constant = TRUE;
mle_age_str_computeModelPredictionAndPerformStat(model_2_params_estimates)

# Model 3 plottings
layout(1) # set graphical area for text display
model_1_fig_text = paste('(E) Model3-"inv-transm" trajectories matching plots of simulated and observed \n(weekly) data, ',base_text)
add_text_to_plot( model_1_fig_text, font_cex=1, valign = 'center', halign = 'left')
graphSettings() # set the graphical parameters.


graphSettings();is_a0Constant = FALSE;
mle_age_str_computeModelPredictionAndPerformStat(model_3_params_estimates)

dev.off() # end of producing pdf

warning("\n A PDF file named: ", paste(addSysDate('mle_age_str_hyperendemic_model_estimations_plots_updates'), 'pdf', sep = '.'), " is created and stored in the folder: figs/final_figs")

#===================================================================================================================
# plotting results to PDF
