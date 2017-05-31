rm(list = ls())

# Paths are defined from the R folder of the project local repository 'meningo-modelling'
source('R/stochastic_modelling/R/my_functions.R')
source('R/stochastic_modelling/R/scirs_bd_model_parameters.R')
source('R/rpackages.R')

#-------------------SCIRS model with temporary immunity, birth, and death.


scirs_bd.transitions <- list(
  c(Susc = -1, Carrier = +1), # infection
  c(Carrier = -1, Ill = +1, Incid = +1),  # Invasion
  c(Carrier = -1, Recov = +1), # Carriage clearence
  c(Ill = -1, Recov = +1), # Disease clearence/recovery
  c(Recov = -1, Susc = +1), # Re-susceptibility
  c(Susc = +1), # new birth
  c(Susc = -1), # death of suceptible
  c(Recov = -1), # death of recovered/immune
  c(Ill = -1),  #  meningitis related death 
  c(Ill = -1), # natural death of case
  c(Carrier = -1) # death of a carrier
)

is_a0Constant = FALSE

set.seed(156829)
nb_years = 3
run<-ssa.adaptivetau(scirs_bd.inits.values,
                     scirs_bd.transitions,
                     scirs_bd.rate.func,
                     scirs_bd.params,
                     tf= year*nb_years)

# transform the return matrix into data.frame for ploting 
run <-data.frame(run)
run<-trace_new_cases(run)

par(mfrow=c(3,2))
plot(run$time, run$Susc, type = "l", col="black", las=1,pch=20, xlab = '', ylab = '')
title(xlab = "Time in days", ylab="Suceptibles")
plot(run$time, run$Carrier, type = "l", col="black", las=1,pch=20,  xlab = '', ylab = '')
title(xlab = "Time in days", ylab="Carriers")
plot(run$time, run$Ill, type = "b", col="black",las=1,pch=20,  xlab = '', ylab = '')
title(xlab = "Time in days", ylab="Prevalent cases")
plot(run$time, run$Recov, type = "l", col="black",las=1,pch=20, xlab = '', ylab = '')
title(xlab = "Time in days", ylab="Recovered")

run_I = approx(x = run$time, y= run$Incid, n = 52*nb_years, method = "constant")

plot(
  round(run_I$x), 
  run_I$y, type = "b", 
  col="black",las=1,pch=16,
  xlab = "",
  ylab="",
  xaxt="n")

axis(1, at = c(seq(0,year*nb_years,7.019231*5)), labels = c(seq(0,52*nb_years,5)))
title(xlab = "Time in weeks", ylab="Weekly Incid")

nb_years = 1; nsim = 20
scirs_bd.multi_run<- multiple_stoch_sim(
  inits.values = scirs_bd.inits.values,
  transitions = scirs_bd.transitions,
  params = scirs_bd.params,
  rates.func = scirs_bd.rate.func,
  tf = year*nb_years,
  out_time = 'weekly',
  nsim = nsim)

# plot the simulations results as a lattice graph

# lattice plot the results

plot_sims_var(
  df = scirs_bd.multi_run, 
  varname = 'Incid', 
  title = 'Weekly incidence of cases', 
  ylab = 'Weekly Cases',
  xlab = 'Time (weeks)')


plot_sims_var(
  df = scirs_bd.multi_run, 
  varname = 'Ill', 
  title = 'Cases prevalence', 
  ylab = 'Weekly prevalence',
  xlab = 'Time (weeks)')


# plot all simution trajectories on one plot with ggplot

p<-ggplot(data = scirs_bd.multi_run, aes(x=time, y=Incid, group=nsim)) + 
  geom_line(aes(colour = factor(nsim)), size = 1)

print(p)
#aes(linetype = nsim, size = 4)


