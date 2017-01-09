
# Fonction decrivant le changement d' état des variables S, C, I, et R  en fonction du 
# temps pour le model SCIRS_I. Elle retournera donc les valeurs de 
# dS/dt; dC/dt; dI/dt et dR/dt à l'instant t

# Les arguments de cette fonction sont: 
# - t (instant "t")
# - y (vecteur contenant les valeurs de S, C et I à l'instant "t")
# - parms(vecteur definissant les valeurs des parametres du modèle)

## -- Modèle 4: SCIRS_ALT model with birth and death --##

##  scirs_ALT.bd() fonction pour l'analyse numérique du modèle 3 --##

SCIRS_ALT.bd <- function(t,y,parms){
  with(c(as.list(y),parms),{
    
    beta <- beta.t(t,beta0,amp_b) # forcer la saisonalité du parametre "beta"
    a <- a.t(t,a0,amp_a)          # # forcer la saisonalité du parametre "a"
    
    f_SC=(1-delta)*beta*Susc*(Carrier+Ill)
    f_CI=a*Carrier
    f_IR=rho*Ill
    f_RS=phi*Recov
    f_CR=alpha*Carrier
    f_SI=delta*beta*Susc*(Ill+Carrier)
    
    #activate_compensation_meninge_death : 0 or 1 to activate Irving's compensation of meninge death throught extra births
    
    dSusc<- f_RS - f_SC -f_SI + mu - mu*Susc + activate_compensation_meninge_death*(gamma*Ill)
    
    dCarrier<- f_SC - f_CI - f_CR - mu*Carrier
    
    dIll<- f_CI + f_SI - f_IR - (mu+gamma)*Ill
    
    dRecov<- f_IR + f_CR - f_RS - mu*Recov
    
    list(c(dSusc, dCarrier, dIll, dRecov), c(newI=f_CI,
                                             f_CR=f_CR,f_IR=f_IR,f_SC=f_SC, f_RS=f_RS, f_SI))
  })
}

## Defintion de la fonction "beta.t" forçant la variation saisonnière de beta
year=365.25
beta.t <- function(x,beta0=1,amp_b=0,Per=year){
  beta0+beta0*amp_b*cos(x*(2*pi/Per))
}

## Defintion de la fonction "a.t" forçant la variation saisonnière de "a" 
a.t <- function(x,a0=1,amp_a=0,Per=year){
  a0+a0*amp_a*cos(x*(2*pi/Per))
}
# Function de simulation du model forced_SCIS. Cette fonction utilise 
# la function SCIS.bd defini plus haut.
# state= initial state of the model
# parms= parameters to be tested
# nbyears = number of years to run the model
# startingyear= starting point of the simulation
forced_SCIRS_ALT <- function(state, parms, nbyears=20, startingyear=1990 ){
  timestep=7.024038
  year=365.25
  t.SCIRS_ALT = seq((startingyear-1990)*year+1, (startingyear-1990+nbyears)*year, by=timestep)  # time steps to evaluate model at
 ts=(ode(y=state, times=t.SCIRS_ALT, func=SCIRS_ALT.bd, parms=parms))
 
 ts[,6]=ts[,6]*timestep
 return(ts)
}
