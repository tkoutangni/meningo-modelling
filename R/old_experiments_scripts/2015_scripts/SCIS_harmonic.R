
# Fonction decrivant le changement d' état des variables S, C, I, en fonction du 
# temps pour le model SCIS. Elle retournera donc les valeurs de 
# dS/dt; dC/dt; dI/dt à l'instant t

# Les arguments de cette fonction sont: 
# - t (instant "t")
# - y (vecteur contenant les valeurs de S, C et I à l'instant "t")
# - parms(vecteur definissant les valeurs des parametres du modèle)

## -- Modèle 1: SCIS model with birth and death --##

##  scis.bd() fonction pour l'analyse numérique du modèle 1 --##

SCIS.bd <- function(t,y,parms){
  with(c(as.list(y),parms),{
    
    beta <- beta.t(t,beta0,amp_b) # forcer la saisonalité du parametre "beta"
    a <- a.t(t,a0,amp_a)          # # forcer la saisonalité du parametre "a"
    
    f_CS=alpha*Carrier
    f_IS=rho*Ill
    f_SC=beta*Susc*(Carrier+Ill)
    #activate_compensation_meninge_death : 0 or 1 to activate Irving's compensation of meninge death throught extra births
    dSusc      <- f_IS + f_CS - f_SC + mu - mu*Susc + activate_compensation_meninge_death*(gamma*Ill)
    
    f_CI=a*Carrier
    dCarrier  <-  f_SC - f_CS - f_CI - mu*Carrier
    
    dIll <-   f_CI - f_IS - (mu+gamma)*Ill
    
    list(c(dSusc, dCarrier, dIll), c(newI=f_CI, 
                                     f_CS=f_CS,f_IS=f_IS,f_SC=f_SC))
  })
}

## Defintion de la fonction "beta.t" forçant la variation saisonnière de beta
year=365.25
beta.t <- function(x,beta0=1,amp=0,Per=year){
  beta0+beta0*amp*cos(x*(2*pi/Per))
}

## Defintion de la fonction "a.t" forçant la variation saisonnière de "a" 
a.t <- function(x,a0=1,amp=0,Per=year){
  a0+a0*amp*cos(x*(2*pi/Per))
}
# Function de simulation du model forced_SCIS. Cette fonction utilise 
# la function SCIS.bd defini plus haut.
# state= initial state of the model
# parms= parameters to be tested
# nbyears = number of years to run the model
# startingyear= starting point of the simulation
forced_SCIS <- function(state, parms, nbyears=20, startingyear=1990 ){
  timestep=7
  year=365.25
  t.SCIS = seq((startingyear-1990)*year+1, (startingyear-1990+nbyears)*year, by=timestep)  # time steps to evaluate model at
  return(ode(y=state, times=t.SCIS, func=SCIS.bd, parms=parms))
}
