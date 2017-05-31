round(summary(model_1_performence$Rsquared),2)
# the standard deviation
sd(model_1_performence$Rsquared)

round(summary(model_2_performence$Rsquared),2)
# the standard deviation
sd(model_2_performence$Rsquared)


round(summary(model_3_performence$Rsquared),2)
# the standard deviation
sd(model_3_performence$Rsquared)

### PB
round(summary(model_1_performence$PBIAS_stat),2)
# the standard deviation
sd(model_1_performence$PBIAS_stat)

round(summary(model_2_performence$PBIAS_stat),2)
# the standard deviation
sd(model_2_performence$PBIAS_stat)


round(summary(model_3_performence$PBIAS_stat),2)
# the standard deviation
sd(model_3_performence$PBIAS_stat)


## RSR
round(summary(model_1_performence$RSR_stat),2)
# the standard deviation
sd(model_1_performence$RSR_stat)

round(summary(model_2_performence$RSR_stat),2)
# the standard deviation
sd(model_2_performence$RSR_stat)


round(summary(model_3_performence$RSR_stat),2)
# the standard deviation
sd(model_3_performence$RSR_stat)

## fold change invasion

round(summary(model_1_performence$a_fold),1)
round(sd(model_1_performence$a_fold),1)


round(summary(model_3_performence$a_fold),1)
round(sd(model_3_performence$a_fold),1)

## fold change transmission
## 
round(summary(model_2_performence$beta_fold),1)
round(sd(model_2_performence$beta_fold),1)


round(summary(model_3_performence$beta_fold),1)
round(sd(model_3_performence$beta_fold),1)


## Carriage estimates average min and average max values
## Model 1 
round(summary(model_1_performence$carriageRange[,'Carriagemin']),10)*100
sd(summary(model_1_performence$carriageRange[,'Carriagemin']),10)*100

round(summary(model_1_performence$carriageRange[,'Carriagemax']),10)*100
sd(summary(model_1_performence$carriageRange[,'Carriagemax']),10)*100

#Model2
round(summary(model_2_performence$carriageRange[,'Carriagemin']),10)*100
sd(summary(model_2_performence$carriageRange[,'Carriagemin']),10)*100

round(summary(model_2_performence$carriageRange[,'Carriagemax']),10)*100
sd(summary(model_2_performence$carriageRange[,'Carriagemax']),10)*100
#Mode3
round(summary(model_3_performence$carriageRange[,'Carriagemin']),10)*100
sd(summary(model_3_performence$carriageRange[,'Carriagemin']),10)*100

round(summary(model_3_performence$carriageRange[,'Carriagemax']),10)*100
sd(summary(model_3_performence$carriageRange[,'Carriagemax']),10)*100


# AIC
# 
round(summary(mle_a0ForcEstimates_matrice$AICc),2)
# the standard deviation
sd(mle_a0ForcEstimates_matrice$AICc)

round(summary(mle_beta0ForcEstimates_matrice$AICc),2)
# the standard deviation
sd(mle_beta0ForcEstimates_matrice$AIC)

round(summary(mle_a0_beta0_ForcEstimates_matrice$AICc),2)
# the standard deviation
sd(mle_a0_beta0_ForcEstimates_matrice$AICc)


