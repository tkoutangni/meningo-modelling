rm(list=ls())

closed.sir.model<- function(t,x,params){
    X <- x[1]
    Y <- x[2]
    Z <- x[3]
    beta <- params["beta"]
    gamma <- params["gamma"]
    pop <- params["popsize"]
    
    dXdt <- -beta*X*Y/pop
    dYdt <- beta*X*Y/pop - gamma*Y
    dZdt <- gamma*Y
    list(c(dXdt,dYdt,dZdt))
}

prediction <- function(params, times){
    xstart = params[c("X.0","Y.0","Z.0")]
    out<-lsoda(func=closed.sir.model,y=xstart, times = times, parms = params)
}

params <-c(X.0 = 0.999, Y.0 = 0.001, Z.0 = 0, popsize = 1, gamma = 365/13, beta = 400)
params1 <-c(X.0 = 999, Y.0 = 1, Z.0 = 0, popsize = 1000, gamma = 365/13, beta = 400)


times = seq(from=0,to=60/365,by=1/365/4)
toto =  prediction(params1,times)
toto2 = prediction(params,times)

matplot(cbind(toto2[,"Y.0"]*1000,toto[,"Y.0"]),col=c("blue","green"),lty=c(4,5),type="l")
