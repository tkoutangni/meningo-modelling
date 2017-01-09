## paramters optimisation start here.

data = floor(computeIntegralWeeklyCases(toto))

# defining the poisson log likelihood function.
poissonLoglik <- function (params,data){
    times<-c(0,seq(1, 1*year, 1))
    pred<-SCIRS_prediction(params,times)
    pred = pred[-1,]
    weeklyPred = floor(sumCasesWeekly(pred))
    matplot(cbind(weeklyPred,data),type=c("l","b"),col=c("blue","black"),pch=c(1,21))
    legend("topright",legend=c("prediction","data"),pch=c(1,21),col=c("blue","black"))
    sum(dpois(x=data,lambda=weeklyPred,log=TRUE))
}

### now the objective function

objectiveF<-function(guess_parms){
    #params[c("beta0","alpha")] <- exp(log.beta0,log.alpha) # untransform the parameters from log scale
    current_parms_combination = params
    parmset = names(guess_parms)
    #names(guess_parms) = parmset
    current_parms_combination[parmset] <- exp(guess_parms)
    print(current_parms_combination[parmset])
    negLogLik= -poissonLoglik(current_parms_combination,data)
    if(negLogLik==Inf){
        negLogLik<- 99999
        cat("\n Neg Log-likelihood: ",negLogLik)
    }
    return(negLogLik)
    
}

parnames(objectiveF)<-names(params)  # ! important for the mle2() function to know the names of the parameters


guess_parms<- list(beta0=log(70/year),
                   alpha=log(20/year)
)



fit0<-mle2(minuslogl = objectiveF, start = guess_parms,vecpar = TRUE)
fit<-mle2(minuslogl = objectiveF, start = as.list(coef(fit0)),vecpar = TRUE)
exp(coef(fit0))





