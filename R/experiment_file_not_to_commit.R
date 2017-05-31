load("data/processed_data/new_processed_february_2016/mle_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")

load("data/processed_data/new_processed_february_2016/mle_age_str_a0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_beta0ForcEstimates_matrice.RData")
load("data/processed_data/new_processed_february_2016/mle_age_str_a0_beta0_ForcEstimates_matrice.RData")

ls()[startsWith(ls(), "mle_")]

# identidy health center years for which simulation needs to be rerun because the likelihood
# function returned NAN. (problem was fixed)

boxplot(mle_beta0ForcEstimates_matrice$f_val)
which(mle_beta0ForcEstimates_matrice$f_val>100)
# Names of those health centers and their column numbers in observed data
mle_a0ForcEstimates_matrice[which(mle_a0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

mle_beta0ForcEstimates_matrice[which(mle_beta0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

mle_a0_beta0_ForcEstimates_matrice[which(mle_a0_beta0_ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

mle_age_str_a0ForcEstimates_matrice[which(mle_age_str_a0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

mle_age_str_beta0ForcEstimates_matrice[which(mle_age_str_beta0ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]

# check to see if all health center years are present in all health center.
mle_age_str_a0_beta0_ForcEstimates_matrice[which(mle_age_str_a0_beta0_ForcEstimates_matrice$f_val>100), c("district", "hc", "year","f_val")]
# check to see if all hea
# 
!any(rownames(mle_a0ForcEstimates_matrice)%in%rownames(mle_age_str_a0ForcEstimates_matrice))
!any(rownames(mle_beta0ForcEstimates_matrice)%in%rownames(mle_age_str_beta0ForcEstimates_matrice))

!any(rownames(mle_a0_beta0_ForcEstimates_matrice)%in%rownames(mle_age_str_a0_beta0_ForcEstimates_matrice))

# Get the value of the minusloglikelihood from each of the parameters
# 
diff_in_model_1_f_values = mle_a0ForcEstimates_matrice$f_val - mle_age_str_a0ForcEstimates_matrice

diff_in_model_1_f_values = mle_a0ForcEstimates_matrice$f_val - mle_age_str_a0ForcEstimates_matrice

model_2_f_value = mle_a0ForcEstimates_matrice$

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

# plot the difference between the standard model and the age structured model
# 
# For model 1

plot(f_values_data_frame$`Diff model 1`)
abline(h = 0, col = "red")
abline(h = mean(f_values_data_frame$`Diff model 1`), col = "blue")

# Plot the densities of the distribution of the negative loglikelihood
plot(density(f_values_data_frame$`Age structured model 1`))
lines(density(f_values_data_frame$`Model 1`), col=2)

plot(density(f_values_data_frame$`Age structured Model 2`))
lines(density(f_values_data_frame$`Model 2`), col=2)

plot(f_values_data_frame$`Age structured model 1`, col = "red", pch=19)
points(f_values_data_frame$`Model 1`, pch=19, col="blue")

toto = melt(f_values_data_frame)
#boxplot
graphSettings()
boxplot(cbind(f_values_data_frame$`Age structured model 1`, f_values_data_frame$`Model 1`),col=c("blue", "red")); title(main = "model 1 -minusloglikelihood")
par(mfrow=c(2,2))




boxplot(cbind(f_values_data_frame$`Age structured model 1`,f_values_data_frame$`Model 1` ))


toto = melt(f_values_data_frame[,c(1:6)])
boxplots.double = 
        boxplot(value~variable, data = toto, at = c(1, 1.8, 2.6, 6, 6.8, 7.6), xaxt='n', col = c('white', 'gray'))



axis(side=1, at=c(1.8, 6.8), labels=c('Methane (ppb)\nNumber of Collections = 100', 'Carbon Dioxide (ppb)\nNumber of Collections = 120'), line=0.5, lwd=0)
title('Comparing Pollution in London, Los Angeles, and New York')

# sample code
# png('triple box plots with patterns.png')
# boxplots.triple = boxplot(value~location + pollutant, data = stacked.data, at = c(1, 1.8, 2.6, 6, 6.8, 7.6), xaxt='n', ylim = c(min(0, min(co2.ny, co2.london, co2.la)), max(ch4.ny, ch4.london, ch4.la, na.rm = T)), col = c('white', 'white', 'gray'))
# axis(side=1, at=c(1.8, 6.8), labels=c('Methane (ppb)\nNumber of Collections = 100', 'Carbon Dioxide (ppb)\nNumber of Collections = 120'), line=0.5, lwd=0)
# title('Comparing Pollution in London, Los Angeles, and New York')


png('triple box plots with patterns.png')



install.packages("WRS2")
library(WRS2)

#qcomhd(f_values_data_frame$`Diff model 1`, f_values_data_frame$`Diff model 1`, q = seq(.1, .9, by=.1))






