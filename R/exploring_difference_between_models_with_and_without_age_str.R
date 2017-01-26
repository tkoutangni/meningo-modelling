# summarize the results after correction of fiting algo returning NAN in some health centers for the objective posisson likelihood
# reload the necessary data.
rm(list = ls())
source("R/run_first.R")
load("data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")

load("data/processed_data/new_processed_february_2016/mle_age_str_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_a0_beta0_ForcEstimates_matrice.RData")

# compute difference difference between objective functions values (the loglikelihood) between 
# the age structured and no homogenous models for each of the three models.

# average_squared_diff_obj_model1 = sum((-(mle_a0ForcEstimates_matrice$f_val) - (-(mle_age_str_a0ForcEstimates_matrice$f_val)))^2)/(length(mle_a0ForcEstimates_matrice$f_val))
# 
# average_squared_diff_obj_model1 = (-(mle_a0ForcEstimates_matrice$f_val) - (-(mle_age_str_a0ForcEstimates_matrice$f_val)))

boxplot(cbind("Model1 Homogenous Mixing" = mle_a0ForcEstimates_matrice$f_val, "Model1 Heterogenous Mixing"=mle_age_str_a0ForcEstimates_matrice$f_val),xlab="model", ylab="-Loglikelihood")

# verify that all health centers are the same for the three age_structured models
!any(rownames(mle_a0ForcEstimates_matrice)%in%rownames(mle_age_str_a0ForcEstimates_matrice))
!any(rownames(mle_beta0ForcEstimates_matrice)%in%rownames(mle_age_str_beta0ForcEstimates_matrice))
!any(rownames(mle_a0_beta0_ForcEstimates_matrice)%in%rownames(mle_age_str_a0_beta0_ForcEstimates_matrice))

# Get the value of the minusloglikelihood from each of the parameters and compute the diff
# between the homogenous and heterogenous model
        
        f_values_data_frame = data.frame(
                'Model 1' =  mle_a0ForcEstimates_matrice$f_val,
                'Age structured model 1' = mle_age_str_a0ForcEstimates_matrice$f_val,
                'Model 2' = mle_beta0ForcEstimates_matrice$f_val,
                'Age structured Model 2' = mle_a0_beta0_ForcEstimates_matrice$f_val,
                'Model 3' = mle_a0_beta0_ForcEstimates_matrice$f_val,
                'Age structured Model 3' = mle_age_str_a0_beta0_ForcEstimates_matrice$f_val,
                'Diff model 1' = mle_a0ForcEstimates_matrice$f_val - mle_age_str_a0ForcEstimates_matrice$f_val,
                'Diff model 2' = mle_beta0ForcEstimates_matrice$f_val - mle_age_str_a0_beta0_ForcEstimates_matrice$f_val,
                'Diff model 3' = mle_a0_beta0_ForcEstimates_matrice$f_val - mle_age_str_a0_beta0_ForcEstimates_matrice$f_val, 
                check.names = FALSE
        )

        
        pdf(file = paste(addSysDate("figs/exploratory_figs/difference_between_age_str_and_no_age_model"), 'pdf', sep = '.'), 
            paper = "a4" , family = "Times", pointsize =12)

        toto = melt(f_values_data_frame[,c(1:6)])
        layout(1)
        par(mar=c(5, 5, 4,5), oma=c(2,4,2,4), mfrow=c(1,1))
        boxplots.double = 
                boxplot(value~variable, data = toto, at = c(1, 1.8, 3.6, 4.4, 6.8, 7.6), xaxt='n', col = c('white', 'gray'), las=1)
        
        # c('Model1-"Inv"', 'Model2-"Transm"', 'Model3-"Inv-Transm"' ), srt = 45, xpd = TRUE, line=0.5, lwd=0, tick = TRUE
        # 1.4, 4, 7.2 | 1, 3.6, 6.8
        axis(side=1, at=c(1.4, 4, 7.2), labels=FALSE, tick = FALSE)
        text(x= c(1.1, 3.7, 6.9), y=c(-5,-5,-5), adj = 1,
                labels  = c('Model1-"Inv"', 'Model2-"Transm"', 'Model3-"Inv-Transm"'),par("usr")[3],
             srt = 15, pos = 1, xpd = TRUE)
        #title('Comparing model with or without age-structure')
        title(main = 'Comparing model without age structure \nvs.\n Model with age structure', ylab='-Loglikelihood')
        legend(x=3.6,y=60,legend=c("without age structure","with age structure"),fill=c("white", "grey"), title="Model")

# visualise the difference between -Loglikelihoods of the models

toto1 = melt(f_values_data_frame[,c(7:9)])

par(mar=c(5, 5, 4,5), oma=c(2,4,2,4))
boxplots.double = 
        boxplot(value~variable, data = toto1, xaxt='n', col = c('white', 'white', "white"), las=1)
title(main = 'Absolute difference in -Loglikelihood\n between model without and with \nage structure', ylab='-Loglikelihood')
#axis(side = 1, at=c(1,2,3) , labels = c('Model1-"Inv"', 'Model2-"Transm"', 'Model3-"Inv-Transm"'))
axis(side=1, at=c(1,2,3), labels=FALSE, tick = FALSE)
# 1.4, 4, 7.2 | 1, 3.6, 6.8
axis(side=1, at=c(1,2,3), labels=FALSE, tick = FALSE)
text(x= c(0.5,1.5,2.5), y=c(-32,-32,-32), adj = 1,
     labels  = c('Model1-"Inv"', 'Model2-"Transm"', 'Model3-"Inv-Transm"'),par("usr")[3],
     srt = 15, pos = 1, xpd = TRUE)
abline(h=0, col="blue")
#title('Comparing model with or without age-structure')
dev.off()

# legend(x=3.6,y=60,legend=c("Without age structure","With age structure"),fill=c("white", "grey"), title="Model structure")
        
        
        