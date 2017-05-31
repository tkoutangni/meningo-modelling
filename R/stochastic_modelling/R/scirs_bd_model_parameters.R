# --------------------
# parameters for the scirs_bd model
#-----------------------
npop = 10000
Carrier = npop*.01
Susc = npop - Carrier
Ill = 0
Recov = 0

scirs_bd.inits.values<-c(
  Susc = Susc,
  Carrier = Carrier,
  Ill = Ill,
  Recov = Recov,
  Incid = 0 # to track new cases
)


# The parameters in days{-1} and constant
# Constant

week = 7
year = 365
month = 30

# Uncertain parameters
beta0 = 80/year # average/baseline transmission 
a0 = .012/year # invasion/ rate of disease among assymptomatic carriers
alpha = 26/year # carriage clearance rate
phi = 1/(2*week) # assuming an average carriage duration of 2 weeks.
epsilon_a = 0.5 # amplitude of seasonal forcing of invasion 
epsilon_b = 0.5 # amplitude of seasonal forcing of transmission

# Parameter more or less certain infered or estimated from the litterature.

gamma = 5.2/year # meningitis death rate
mu = 1/54/year # natural death rate
rho = 1/week   #Disease recovery rate.Meningitis acute phase last on average a week.
teta = 91 # the average number of days from the begining of the calendar year when disease
# pick
# b = mu + gamma*Ill
act_comp_mening_death = 1 # this is constant to compansate birth with meningitis related death

scirs_bd.params<-list(
  beta0 = beta0,
  a0 = a0,
  alpha = alpha,
  phi = phi,
  epsilon_a = epsilon_a,
  epsilon_b = epsilon_b,
  gamma = gamma,
  mu = mu,
  rho = rho,
  teta = teta,
  act_comp_mening_death = act_comp_mening_death
)




