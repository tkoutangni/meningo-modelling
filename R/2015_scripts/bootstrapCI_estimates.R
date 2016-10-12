## bootstap estimate of confidence interval for the parameters estimates.

# Bootstrap 95% CI for regression coefficients
library(boot)
# function to obtain parameters estimates. This function return the vector of parameters estimates
# for which confidence interval is to be computed



#we want to compute bootstrap confidence interval for 
#the best fitted model only: which is the model including both invasion and transmission 
#forcing

a0Forcing=TRUE
beta0Forcing=TRUE 

bs <- function(data,indices) {
    yearSpecificData <- data[indices,] # allows boot to select sample of the same number of row of observation
    parmEstimates = computeEstimates(
                                    district_id=1,district_year_data = yearSpecificData[indices,], year_now = 2006, hc_vector= 1, #:4,6
                                    figureFile="")
    fittedParms<-parmEstimates[,c("beta0", "alpha", "phi", "Susc0","CarrierProp","teta","epsilon_a","epsilon_b","a0","Rsquared")]
    return(fittedParms)
}
# bootstrapping with 1000 replications. as number of Replication increases the algo 
# will take longer to complete.

results <- boot(data=seguen_2006, statistic=bs,R=300)

plot(results, index=1) # beta0 distribution
plot(results, index=2) # alpha distribution

# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # beta0
boot.ci(results, type="bca", index=2) # alpha
boot.ci(results, type="bca", index=3) # phi 


hsb2<-read.table("http://www.ats.ucla.edu/stat/data/hsb2.csv", sep=",", header=T) 

f <- function(d, i){
    d2 <- d[i,]
    #print(head(d2,5))
    return(cor(d2$write, d2$math))
}

bootcorr <- boot(hsb2, f, R=1000)
bootcorr
boot.ci(bootcorr, type="bca")
plot(bootcorr)



x <- c(.4,.48,.43,.42,.5,.46,.39,.41,.42,.44)
y <- c(.021,.03,.025,.022,.033,.027,.019,.021,.023,.025)

orange<-data.frame(x,y)
f<-function(data,i){
    toto = sum(data[i,]$y)/sum(data[i,]$x)
    print(toto)
    return(toto)
}
#set.seed = 123456
test = boot(orange,f,400)
test
boot.ci(test,type="bca")
