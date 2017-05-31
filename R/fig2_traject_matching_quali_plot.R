rm(list=ls())
par(mar = c(3.5, 3.5, 1, 1), mgp = c(2, 0.7, 0), tck = -0.01)
opar<-par()

###==========================
rm(list = ls())
load("data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")


insert_age_structure = FALSE; heterogenous_mixing = FALSE
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




# identify health center to plot
model_1_best_fitted_hc = as.numeric(which.min(mle_a0ForcEstimates_matrice$RSR_stat))
params_estimates1 = mle_a0ForcEstimates_matrice[model_1_best_fitted_hc,]
graphSettings();is_a0Constant = FALSE; mle_computeModelPredictionAndPerformStat(params_estimates1)

model_2_best_fitted_hc = as.numeric(which.min(mle_beta0ForcEstimates_matrice$RSR_stat))
params_estimates2 = mle_beta0ForcEstimates_matrice[model_2_best_fitted_hc,]
graphSettings();is_a0Constant = TRUE; mle_computeModelPredictionAndPerformStat(params_estimates2)

model_3_best_fitted_hc = as.numeric(which.min(mle_a0_beta0_ForcEstimates_matrice$RSR_stat))
params_estimates3 = mle_a0_beta0_ForcEstimates_matrice[model_3_best_fitted_hc,]
graphSettings();is_a0Constant = FALSE; mle_computeModelPredictionAndPerformStat(params_estimates3)


toto = list(params_estimates1, params_estimates2, params_estimates3)

lapply(toto, mle_computeModelPredictionAndPerformStat)






## Poorest fitted health center years acrross the tree models
model_1_poorest_fitted_hc = as.numeric(which.max(mle_a0ForcEstimates_matrice$RSR_stat))
params_estimates_1 = mle_a0ForcEstimates_matrice[model_1_poorest_fitted_hc,]
graphSettings();is_a0Constant = FALSE; mle_computeModelPredictionAndPerformStat(params_estimates_1)

model_2_poorest_fitted_hc = as.numeric(which.max(mle_beta0ForcEstimates_matrice$RSR_stat))
params_estimates_2 = mle_beta0ForcEstimates_matrice[model_2_poorest_fitted_hc,]
graphSettings();is_a0Constant = TRUE; mle_computeModelPredictionAndPerformStat(params_estimates_2)


model_3_poorest_fitted_hc = as.numeric(which.max(mle_a0_beta0_ForcEstimates_matrice$RSR_stat))
params_estimates_3 = mle_a0_beta0_ForcEstimates_matrice[model_3_poorest_fitted_hc,]
graphSettings();is_a0Constant = FALSE; mle_computeModelPredictionAndPerformStat(params_estimates_3)


## Best fitted health center years acrross the tree models
model_1_best_fitted_hc = as.numeric(which.min(mle_a0ForcEstimates_matrice$RSR_stat))
params_estimates_1 = mle_a0ForcEstimates_matrice[model_1_best_fitted_hc,]
graphSettings();is_a0Constant = FALSE; mle_computeModelPredictionAndPerformStat(params_estimates_1)

model_2_best_fitted_hc = as.numeric(which.min(mle_beta0ForcEstimates_matrice$RSR_stat))
params_estimates_2 = mle_beta0ForcEstimates_matrice[model_2_best_fitted_hc,]
graphSettings();is_a0Constant = TRUE; mle_computeModelPredictionAndPerformStat(params_estimates_2)


model_3_best_fitted_hc = as.numeric(which.min(mle_a0_beta0_ForcEstimates_matrice$RSR_stat))
params_estimates_3 = mle_a0_beta0_ForcEstimates_matrice[model_3_poorest_fitted_hc,]
graphSettings();is_a0Constant = FALSE; mle_computeModelPredictionAndPerformStat(params_estimates_3)


wrapper_mle_computeModelPredictionAndPerformStat = function(f, ...){return (f)}

quali_plot_to_compare_the_models<-function(hc_param_estimates_list, is_a0Constant){
        lapply(hc_param_estimates_list, function(x){
                wrapper_mle_computeModelPredictionAndPerformStat(
                        mle_computeModelPredictionAndPerformStat(x), is_a0Constant)
        })
}

plot_traject_matching<-function(rowIndex_of_health_center_to_plot){
        
        params_estimates_1 = mle_a0ForcEstimates_matrice[rowIndex_of_health_center_to_plot,]
        params_estimates_2 = mle_beta0ForcEstimates_matrice[rowIndex_of_health_center_to_plot,]
        params_estimates_3 = mle_a0_beta0_ForcEstimates_matrice[rowIndex_of_health_center_to_plot,]

        
        data = list(params_estimates_1, params_estimates_2, params_estimates_3)
        quali_plot_to_compare_the_models(hc_param_estimates_list = data, 
                                         is_a0Constant = c(F, T, F))  
}


graphSettings()

#poorest fitted hc based on the RSR
plot_traject_matching(model_1_poorest_fitted_hc)
#plot_traject_matching(model_2_poorest_fitted_hc)
#plot_traject_matching(model_3_poorest_fitted_hc)

# best fitted hc based on the RSR

#plot_traject_matching(model_1_best_fitted_hc)
plot_traject_matching(model_2_best_fitted_hc)

#plot_traject_matching(model_3_best_fitted_hc)
tiff("R/fig2.tiff",
     width = 6, 
     height = 4, 
     units = 'in', 
     compression = "lzw",
     type="quartz",
     pointsize = 12,
     res = 300)

graphSettings()
par(mfrow=c(2,3))
#par(mar = c(4.5, 4.5, 1, 1), mgp = c(2, 1, 1), tck = -0.01)
plot_traject_matching(model_1_poorest_fitted_hc)
#text(locator(1),"A")
#mtext("A", side=5, line=1, adj=0, font=2, cex=2)
plot_traject_matching(model_2_best_fitted_hc)

dev.off()






# my.locations <- c("topleft")
# 
# # Make the plots and append a figure letter to each
# # Note: put.fig.letter() sends additional parameters to
# # the text() function.
# labels<-c("A")
# for(i in 1) {
#         plot(x[,i], y[,i], pch=16, xlab="x", ylab="y")
#         my.label <- paste(labels[i], ".", sep="")
#         put.fig.letter(label=my.label, location=my.locations[i], font=2)
# }




# par(mfrow=c(1,2)) 
# plot(x~y) 
# text(locator(1),"A") 
# plot(x~z) 
# text(locator(1),"B")


RBIND_data_frames <- function(data_frame_list) {
        require(plyr)
        temp <- rbind.fill(datalist)
        rownames(temp) <- unlist(lapply(datalist, row.names))
        temp
}
RBIND_data_frames(list(x, y))
