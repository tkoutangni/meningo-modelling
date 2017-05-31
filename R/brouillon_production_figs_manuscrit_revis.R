#
# You have set the resolution, but you have not set the width/height.
# The res argument generally controls how many pixels per inch 
# (PPI which is often used similarly to DPI). So if you want 800 DPI and you
# want it to be a 4 x 4 inch graph something like:
        
# tiff(file = "R/temp.tiff", width = 3200, height = 3200, units = "px", res = 800)
# plot(1:10, 1:10)
# dev.off()

# This will make a file that is 3200 x 3200 pixels, with an 800
# resolution gives you 3200/800 = 4 inches.
# 
# I would also recommend choosing some sort of compression or you will
# end up with a rather large file.
#
## postscript("R/eps_file.eps", width = 5, height = 5, horizontal = FALSE, 
#            onefile = FALSE, paper = "special", colormodel = "rgb", 
#            family = "Times")
# 
# plot(1:20, type="b", pch=16, xlab="x label", ylab="ylab")
# dev.off()
#==============================================================
par(mar = c(3.5, 3.5, 1, 1), mgp = c(2, 0.7, 0), tck = -0.01)
opar<-par()

###==========================
rm(list = ls())
load("data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")


insert_age_structure = FALSE; heterogenous_mixing=FALSE
source("R/run_first.R")
source("R/models_parameters.R")
#source("R/mle_computeModelPredictAndPerformStats.R")
source("R/mle_computeModelPredictAndPerformance_modif.R")

# compute and srore the model predicitons and performance stats
is_a0Constant = FALSE; graphSettings()
model_1_performence = mle_computeModelPredictionAndPerformStat(mle_a0ForcEstimates_matrice)

graphSettings();is_a0Constant = TRUE;
model_2_performence = mle_computeModelPredictionAndPerformStat(mle_beta0ForcEstimates_matrice)

graphSettings();is_a0Constant = FALSE;
model_3_performence = mle_computeModelPredictionAndPerformStat(mle_a0_beta0_ForcEstimates_matrice)


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

plotRates = FALSE

if(!plotRates){
        mle_a0ForcEstimates_matrice$alpha = (1/mle_a0ForcEstimates_matrice$alpha)/7
        mle_a0ForcEstimates_matrice$phi = (1/mle_a0ForcEstimates_matrice$phi)/year
        mle_a0ForcEstimates_matrice$teta = (mle_a0ForcEstimates_matrice$teta)/7
        mle_a0ForcEstimates_matrice$Susc0 = (mle_a0ForcEstimates_matrice$Susc0)/1000
        mle_a0ForcEstimates_matrice$CarrierProp = (mle_a0ForcEstimates_matrice$CarrierProp)/100
        
        mle_beta0ForcEstimates_matrice$alpha = (1/mle_beta0ForcEstimates_matrice$alpha)/7
        mle_beta0ForcEstimates_matrice$phi = (1/mle_beta0ForcEstimates_matrice$phi)/year
        mle_beta0ForcEstimates_matrice$teta = (mle_beta0ForcEstimates_matrice$teta)/7
        mle_beta0ForcEstimates_matrice$Susc0 = (mle_beta0ForcEstimates_matrice$Susc0)/1000
        mle_beta0ForcEstimates_matrice$CarrierProp = (mle_beta0ForcEstimates_matrice$CarrierProp)/100
        
        mle_a0_beta0_ForcEstimates_matrice$alpha = (1/mle_a0_beta0_ForcEstimates_matrice$alpha)/7
        mle_a0_beta0_ForcEstimates_matrice$phi = (1/mle_a0_beta0_ForcEstimates_matrice$phi)/year
        mle_a0_beta0_ForcEstimates_matrice$teta = (mle_a0_beta0_ForcEstimates_matrice$teta)/7
        mle_a0_beta0_ForcEstimates_matrice$Susc0 = (mle_a0_beta0_ForcEstimates_matrice$Susc0)/1000
        mle_a0_beta0_ForcEstimates_matrice$CarrierProp = (mle_a0_beta0_ForcEstimates_matrice$CarrierProp)/100
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


library(reshape2)
mle_a0ForcEstimates_matrice.m <- melt(mle_a0ForcEstimates_matrice, id.var = "district")
mle_a0ForcEstimates_matrice.m$model = "Model1-Inv   "

mle_beta0ForcEstimates_matrice.m <- melt(mle_beta0ForcEstimates_matrice, id.var = "district")
mle_beta0ForcEstimates_matrice.m$model = "Model2-Transm   "

mle_a0_beta0_ForcEstimates_matrice.m <- melt(mle_a0_beta0_ForcEstimates_matrice, id.var = "district")
mle_a0_beta0_ForcEstimates_matrice.m$model = "Model3-Inv-Transm   "

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
                                | variable=='PBIAS_stat' | variable=='AIC' | variable=='BIC',
                                select=c(model, variable, value))

# labels that we want to display on the plot facet wraps.
param_to_plot_labels<-c('Mean transmission/day',
                        'Carriage duration (weeks)',
                        'Imunity duration (years)',
                        'Initial susceptibles/1000',
                        'Initial carriers/100',
                        'Peak time (week number)',
                        'Forcing amplitude for invasion',
                        'Mean invasion/day',
                        'Forcing amplitude for transmission')

param_to_plot$variable_label<-factor(param_to_plot$variable,labels=param_to_plot_labels)

#perform_stats_to_plot_labels<-c('AICc', 'BIC', 'Rsquared','RSR','PB(%)')
perform_stats_to_plot_labels<-c('Akaike information criterion (AIC)', 
                                'Bayesian information criterion (BIC)', 'Rsquared',
                                'RMSE/Data standard deviation (RSR)','Percent Bias (PB)')


perform_stats_to_plot$variable_label<-factor(perform_stats_to_plot$variable,labels=perform_stats_to_plot_labels)


tiff("R/fig_models_performances.tiff", 
     height = 5, 
     width = 5, 
     units = 'in', 
     compression = "lzw",
     type="quartz",
     pointsize = 12,
     res = 300)

par(pin=c(5, 5))
plot1<-ggplot(data = perform_stats_to_plot, aes(x=variable_label, y=value, fill=model)) + 
        geom_boxplot(outlier.size=1) + xlab("Performence Statistics")
plot1 <- plot1 + facet_wrap( ~ variable_label, scales="free",nrow = 3, labeller= label_wrap_gen(width = 25))
plot1 <- plot1 + xlab(" ") + ylab("Value") + ggtitle("")
plot1 <- plot1 + guides(fill=guide_legend(title=""))
plot1 <- plot1 + my_theme_for_facet
plot1<- plot1 + scale_fill_manual(name = "",
                                  values = c("#ffffff", "#bdbdbd", "#636363"),
                                  labels = c("Model1-Inv    ", "Model2-Transm    ",
                                             "Model3-Inv-Transm    ")
                                  )
plot1
dev.off()

#####################################
# Parameters estimates distribution
#####################################

tiff("R/fig3.tiff",
     width = 5,
     height = 5, 
     units = "in", 
     compression = "lzw",
     type="quartz",
     pointsize = 12,
     res = 300)

plot2 <- ggplot(data = param_to_plot, aes(x=variable_label, y=value)) + 
        geom_boxplot(aes(fill=model), outlier.size=1)
# if you want color for points replace group with colour=Label
#plot2 <- plot2 + geom_point(aes(y=value, group=model), position = position_dodge(width=0.75))
plot2 <- plot2 + facet_wrap( ~ variable_label, scales="free",nrow = 4, labeller= label_wrap_gen(width = 20))
plot2 <- plot2 + xlab("") + ylab("Value") + ggtitle("")
plot2 <- plot2 + guides(fill=guide_legend(title=""))
plot2<-plot2 + my_theme_for_facet
plot2<- plot2 + scale_fill_manual(name = "",
                                   values = c("#ffffff", "#bdbdbd", "#636363"),
                                   labels = c("Model1-Inv    ", "Model2-Transm    ",
                                              "Model3-Inv-Transm    ")
                                  )
plot2

dev.off()



#                 

## Best fitted health center years acrross the tree models
# 
# mle_a0ForcEstimates_matrice[as.numeric(which.min(mle_a0ForcEstimates_matrice$RSR_stat)),]
# mle_beta0ForcEstimates_matrice[as.numeric(which.min(mle_beta0ForcEstimates_matrice$RSR_stat)),]
# mle_a0_beta0_ForcEstimates_matrice[as.numeric(which.min(mle_a0_beta0_ForcEstimates_matrice$RSR_stat)),]
# 
# # mle_a0ForcEstimates_matrice[as.numeric(which.max(mle_a0ForcEstimates_matrice$Rsquared)),]
# # mle_beta0ForcEstimates_matrice[as.numeric(which.max(mle_beta0ForcEstimates_matrice$Rsquared)),]
# # mle_a0_beta0_ForcEstimates_matrice[as.numeric(which.max(mle_a0_beta0_ForcEstimates_matrice$Rsquared)),]
# 
# ## Poorest fitted health center years acrross the tree models
# 
# mle_a0ForcEstimates_matrice[as.numeric(which.max(mle_a0ForcEstimates_matrice$RSR_stat)),]
# mle_beta0ForcEstimates_matrice[as.numeric(which.max(mle_beta0ForcEstimates_matrice$RSR_stat)),]
# mle_a0_beta0_ForcEstimates_matrice[as.numeric(which.max(mle_a0_beta0_ForcEstimates_matrice$RSR_stat)),]

# mle_a0ForcEstimates_matrice[as.numeric(which.min(mle_a0ForcEstimates_matrice$Rsquared)),]
# mle_beta0ForcEstimates_matrice[as.numeric(which.min(mle_beta0ForcEstimates_matrice$Rsquared)),]
# mle_a0_beta0_ForcEstimates_matrice[as.numeric(which.min(mle_a0_beta0_ForcEstimates_matrice$Rsquared)),]
# 
# 
# 





