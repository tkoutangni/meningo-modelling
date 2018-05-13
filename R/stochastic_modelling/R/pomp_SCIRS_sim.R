rm(list = ls())
source("R/run_first.R")
source('R/stochastic_modelling/R/my_functions.R')
# data to use as sample 
source('R/stochastic_modelling/R/pomp_data_for_stoch_sim.R')
# load the C code of the deterministic and stochastic model as a string 
# for later use in the pomp_object construction
source('R/stochastic_modelling/R/pomp_SCIRS.c.R')

# load the model initial parameters needed to simulate the pomp object
source('R/stochastic_modelling/R/pomp_SCIRS_params.R')


## Model initial values draws : c code
# Optional; draws from the distribution of initial values of the unobserved Markov state process. Specifically, given a vector of parameters, params and an initial time, t0, the initializer determines the state vector at time t0

# SCIRS_initlz<-"
#   double m = population/(Susc_0+Carrier_0+Ill_0+Recov_0);
#   Susc = nearbyint(m*Susc_0);
#   Carrier = nearbyint(m*Carrier_0);
#   Ill = nearbyint(m*Ill_0);
#   Recov = nearbyint(m*Recov_0);
#   Inc = 0;
# "

# Intial state variables to initialize the model simulation.
SCIRS_initlz <-"
        Susc = nearbyint(population*Susc_0);
        Carrier = nearbyint(population*Carrier_0);
        Ill = nearbyint(population*Ill_0);
        Recov = nearbyint(population*Recov_0);
        //Recov = nearbyint(population - (Susc + Carrier + Ill));
        Inc = 0;
"

## define point observation probability density
SCIRS_dmeas.c<- "
        lik = dpois(meningitis_cases, Inc > 0 ? Inc : 0, give_log);
        
        // Print values of state variable if log likelihood return non-finite value.

        if (!R_FINITE(lik)){
                Rprintf(\"%lg %lg %lg %lg %lg %lg\\n\",Susc, Carrier, Ill, Recov, Inc,lik);
        }
        // Return the loglikihood if giv_log is true, expo(loglik) otherwise
        //lik = give_log ? lik : exp(lik);
"
## define sampling random point observations
SCIRS_rmeas.c<-"
        meningitis_cases = rpois(Inc > 0 ? Inc : 0);
"


# SCIRS_dmeas.c<- '
#         lik = dpois(meningitis_cases, nearbyint(Inc), give_log);
#         
#         // print values of state var if log likelihood return non-finite value.
#         if (!R_FINITE(lik)){
#                 Rprintf(\"%lg %lg %lg %lg %lg %lg\\n\",Susc, Carrier, Ill, Recov, Inc,lik);
#         }
#         
#         //lik = (give_log) ? lik : exp(lik);
#         
# '

# SCIRS_rmeas.c<-'
#         meningitis_cases = rpois(Inc);
#         if (meningitis_cases > 0.0) {
#                 meningitis_cases = nearbyint(meningitis_cases);
#         } else {
#                 meningitis_cases = 0;
#         }
# '


## Model tranformed params values c code
SCIRS_toEst.c<-"
        // fixed params
        Tgamma = log(gamma);
        Tmu    = log(mu);
        Trho   = log(rho);
        Tact_comp_mening_death = 1;
        // Unknown params
        Tbeta0 = log(beta0);
        Talpha = log(alpha);
        Tphi = log(phi);
        Tteta = log(teta);
        Tepsilon_a = logit(epsilon_a);
        Tepsilon_b = logit(epsilon_b);
        Ta0 = log(a0);
        to_log_barycentric (&TSusc_0, &Susc_0, 4);
"

## Untranformed params values c code
SCIRS_fromEst.c<- "
        // fixed params
        Tgamma = exp(gamma);
        Tmu    = exp(mu);
        Trho   = exp(rho);
        Tact_comp_mening_death = 1;
        // Unknown params
        Tbeta0 = exp(beta0);
        Talpha = exp(alpha);
        Tphi = exp(phi);
        Tteta = exp(teta);
        Tepsilon_a = expit(epsilon_a);
        Tepsilon_b = expit(epsilon_b);
        Ta0 = exp(a0);
        from_log_barycentric (&TSusc_0, &Susc_0, 4);
"

# Preparing data that will be used for model simulation and epidemiological parameters inference. 

# subset and store covariables (demographic data such as population size) in a variable
subset_demog_seguenega_bema<-subset(demog_seguenega_bema, time>=2006 & time<=2007, select = c(semaine, time, population))

# round the size of the population to an integer
subset_demog_seguenega_bema$population<-round(subset_demog_seguenega_bema$population)

# subset and store the data to fit in a variable
sample_data_set<-subset(ready_meningo_data_seguenega_bema, time>=2006 & time<=2007,
                        select =c(semaine, time,  meningitis_cases))


head(subset_demog_seguenega_bema)
head(sample_data_set)
 
# Pomp model specification for both deterministic and stochastic simulations
# name of the time column in the the data and covariables data.frames

time_colname<-"time"

        pomp(
             data = sample_data_set[,c(time_colname, "meningitis_cases")],
             times= time_colname,
             #t0 = with(sample_data_set,2*time[1]-time[2]),
             t0 = sample_data_set[,time_colname][1],
             rprocess=euler.sim(Csnippet(SCIRS_stock.c),delta.t=1/52), #stochastic version
             skeleton = vectorfield(Csnippet(SCIRS_determ.c)), # deterministic model
             dmeasure = Csnippet(SCIRS_dmeas.c),
             rmeasure = Csnippet(SCIRS_rmeas.c),
             toEstimationScale=Csnippet(SCIRS_toEst.c),
             fromEstimationScale=Csnippet(SCIRS_fromEst.c),
             initializer = Csnippet(SCIRS_initlz),
             covar = subset_demog_seguenega_bema,
             tcovar= time_colname, #timevector name corresponding in the covarible dataframe
             zeronames=c("Inc"),
             statenames = c("Susc", "Carrier", "Ill", "Recov", "Inc"),
             paramnames = c("gamma", "beta0", "a0", "alpha", "epsilon_a",
                            "epsilon_b", "mu", "teta", "rho", "phi", "act_comp_mening_death"
                            ,"Susc_0","Carrier_0","Ill_0","Recov_0"
                            )
        ) -> SCIRS_pomp

        
#visualizing the data to be fitted by the model
SCIRS_pomp %>% 
        as.data.frame() %>% 
        subset(select= -c(semaine)) %>%
        melt(id="time") %>%
        ggplot(aes(x=time,y=value))+
        geom_line()+ geom_point()+
        facet_grid(variable~.,scales="free_y")


# Simulate and plot determinsitic trajectory
SCIRS.traj <- trajectory(SCIRS_pomp, 
                         params = c(SCIRS_theta,SCIRS_init_state), 
                         as.data.frame = TRUE
                         #to = 2006.012
                         #,times=seq(2006.012,2006.99,by=0.019)
                         )

plotTraj(SCIRS.traj, colour = "black", data=sample_data_set[,c(time_colname, "meningitis_cases")], lines.data = FALSE)

# check to see if row sums at each step of simulation total to the population size
table(as.vector(rowSums(SCIRS.traj[,1:4])))

# The following will compute and plot some simulations. First, I plot n simulations and compare them with the data.

 SCIRS_pomp %>% 
        simulate(params=c(SCIRS_theta,SCIRS_init_state),
                 nsim=11,
                 as.data.frame=TRUE,
                 include.data=TRUE,
                 seed=43553
                 ) -> SCIRS_sim

SCIRS_sim %>%
        ggplot(aes(x=time,y=meningitis_cases,group=sim,color=(sim=="data")))+
        guides(color=FALSE)+
        geom_line()+ geom_point()+ facet_wrap(~sim,ncol=2)


 
plotTraj(
        SCIRS_sim[,which(!names(SCIRS_sim)%in%c("semaine", "population"))], 
        colour = "red", 
         data=sample_data_set[,c(time_colname, "meningitis_cases")],
        lines.data = FALSE
        )

# check if total population is correct at each time dt
head(rowSums(SCIRS.traj[,c(1:4)]))

#pfilter(SCIRS_pomp,params=c(SCIRS_theta,SCIRS_init_state),Np=10000) -> pf
#logLik(pf)


# Estimates the likelihood of the given parameters using a particle filter with Np = 100 particles.

library(foreach)
library(doMC)
registerDoMC()

set.seed(998468235L,kind="L'Ecuyer")
mcopts <- list(preschedule=FALSE,set.seed=TRUE)

time_taken(
        
        foreach(i=1:5,.packages="pomp",
                .options.multicore=mcopts) %dopar% {
                        pfilter(SCIRS_pomp,params=c(SCIRS_theta,SCIRS_init_state),Np=10000)
                } -> pfs

)

logmeanexp(sapply(pfs,logLik),se=TRUE)

# Fitting the deterministic POMP model by maximising the likelihood of 
# the observed data given the model results and parameters.


# Params I want to be fixed. 
fixed_pars<-c("gamma", "mu", "rho", "act_comp_mening_death")
fixed_pars_values<-SCIRS_theta[fixed_pars]
#fixed_pars<-c("act_comp_mening_death")

to_estimate_pars<- c(setdiff(c(names(SCIRS_theta), "Susc_0", "Carrier_0", "Recov_0"), fixed_pars))


guess.p<- c(SCIRS_theta, SCIRS_init_state)
  
time_taken(    
SCIRS.tm <- traj.match(SCIRS_pomp, 
                       start = guess.p, 
                       est = to_estimate_pars,
                       transform = TRUE,
                       method="Nelder-Mead"
                       ,maxit=10000
                       ,reltol=1e-8
                       #,control = list()
                       #,opts = nloptr_opts
                       )
)

#summary(SCIRS.tm)
# print the estimates from the fitted pomp deterministic model
# These estimates are in years or /year
print_fit_result(SCIRS.tm, to_estimate_pars)

# running the stochastic model with the fitted params from the maximum likelihood estimates
# from trajectory matching the deterministic model (SCIRS.tm)
# Then plot the distribution of the outpout as mean , median and 50 and 95th percentile

SCIRS.tm.sim <- simulate(SCIRS.tm, nsim = 1000, as.data.frame = TRUE, seed = 1234)
SCIRS.tm.sim<-subset(SCIRS.tm.sim, select = -c(semaine))

plotTraj(SCIRS.tm.sim, 
         data = subset(sample_data_set, select = -c(semaine)), 
         state.names = "meningitis_cases")


#===============================================================================================
#=FITTING THE STOCHASTIC MODEL BY ESTIMATING MAXIMUM LIKELIHOOD USING ITERATED FILTERING METHOD
# Mif2
#===============================================================================================
library(magrittr)
library(foreach)
library(doParallel)
registerDoParallel()


estpars <- to_estimate_pars
rw_sd = c(0.5, 0.001, 0.1, 0.1, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001)
names(rw_sd) = estpars

nbcores = 3 # the number of cores availaible on my laptop minus 1

library(tictoc)
tic();
foreach(i=1:nbcores,
        .inorder=FALSE,
        .options.multicore=list(set.seed=TRUE)
) %dopar%
{
        theta.guess <- c(SCIRS_theta, SCIRS_init_state)
        theta.guess[estpars] <- rlnorm(
                n=length(estpars),
                meanlog=log(theta.guess[estpars]),
                sdlog=1
        )
        #print(theta.guess[estpars])
        mif2(
                SCIRS_pomp,
                Nmif=50,
                start=theta.guess,
                transform=TRUE,
                rw.sd=rw.sd(
                        beta0=0.5,
                        a0=0.001,
                        alpha=0.1,
                        teta= 0.1,
                        epsilon_a=0.0001,
                        epsilon_b=0.0001,
                        phi= 0.0001,
                        Susc_0 = 0.0001,
                        Carrier_0= 0.0001,
                        Recov_0= 0.0001),
                
                cooling.fraction.50=0.95,
                Np=2000
        ) %>%
                continue(Nmif=50,cooling.fraction=0.8) %>%
                continue(Nmif=50,cooling.fraction=0.6) %>%
                continue(Nmif=50,cooling.fraction=0.2) -> m1
        ll <- replicate(n=nbcores,logLik(pfilter(m1,Np=10000)))
        list(mif=m1,ll=logmeanexp(ll,se=TRUE))
} -> mf
toc();


lls <- sapply(mf,getElement,"ll")
best <- which.max(sapply(mf,getElement,"ll")[1,])
theta.mif <- coef(mf[[best]]$mif)

replicate(nbcores,logLik(pfilter(SCIRS_pomp,params=theta.mif,Np=10000))) %>%
        logmeanexp(se=TRUE) -> pf.loglik.mif

#===========================================================================


## fitting the stochastic model with Maximum likelihood by iterated filtering (MIF)
#===============================================================================================
fit_stoch_model_with_mif_algo = TRUE  # swicth on and off, the 


if(fit_stoch_model_with_mif_algo){
        prop.sd <- rw_sd
        SCIRS.mf<-mif(
                SCIRS.tm,
                Nmif=50,
                #start=theta.guess,
                transform=TRUE,
                rw.sd= rw_sd,
                
                cooling.fraction.50=0.5,
                Np=1000
        )

        SCIRS.mf.sim <- simulate(SCIRS.mf, nsim = 10, as.data.frame = TRUE, include.data = TRUE)
        plotTraj(SCIRS.mf.sim, data = subset(sample_data_set, select = -c(semaine))
                 , state.names = "meningitis_cases")
}


#==============================================================================================
fixed_params <- with(SCIRS_pomp,fixed_pars_values)

install.packages("doRNG")
library(doRNG)
registerDoRNG(625904618)
bake(file="pf2.rds",{
        foreach(i=1:10,.packages='pomp',
                .combine=c,
                .export=c("SCIRS_pomp","fixed_params")
        ) %dopar% {
                pfilter(SCIRS_pomp,params=c(SCIRS_theta,SCIRS_init_state),Np=10000)
        }
}) -> pf
L_pf <- logmeanexp(sapply(pf,logLik),se=TRUE)
# save results to excel
results <- as.data.frame(as.list(c(coef(pf[[1]]),loglik=L_pf[1],loglik=L_pf[2])))
write.csv(results,file="SCIRS_params.csv",row.names=FALSE)


library(foreach)
library(doParallel)
registerDoParallel()

detach("package:gridExtra", unload=TRUE)
detach("package:dplyr", unload=TRUE)
detach("package:gdata", unload=TRUE)


library(doRNG)
registerDoRNG(482947940)
bake(file="box_search_local3.rds",{
        foreach(i=1:3,
                .packages='pomp',
                .combine=c,
                .multicombine = TRUE,
                .export=c("SCIRS_pomp","fixed_params")
        ) %dopar%  
        {
                mif2(
                        SCIRS_pomp,
                        start=c(SCIRS_theta, SCIRS_init_state),
                        Np=2000,
                        Nmif=50,
                        cooling.type="geometric",
                        cooling.fraction.50=0.5,
                        transform=TRUE,
                        rw.sd=rw.sd(
                                beta0=0.5,
                                a0=0.001,
                                alpha=0.1,
                                teta= 0.1,
                                epsilon_a=0.0001,
                                epsilon_b=0.0001,
                                phi= 0.0001,
                                Susc_0 = 0.0001,
                                Carrier_0= 0.0001,
                                Recov_0= 0.0001
                        )
                )
        }
}) -> mifs_local

ggplot(data=melt(conv.rec(mifs_local)),
       aes(x=iteration,y=value,group=L1,color=factor(L1)))+
        geom_line()+
        guides(color=FALSE)+
        facet_wrap(~variable,scales="free_y")+
        theme_bw()






guesses <- sobolDesign(lower=c(beta0= 7.5000000000,
                               a0 = 0.0001220131,
                               alpha = 0.0935247912,
                               teta = 13,
                               epsilon_a = 0.0010000000,
                               epsilon_b = 0.0010000000,
                               phi = 0.0023160179,
                               Susc_0 = 0.999000000000,
                               Carrier_0 = 0.010000,
                               gamma = 5.2 ,
                               mu    = (1 / 54) ,
                               rho   = 52 ,
                               act_comp_mening_death = 1), # dimensionless
                       
                       upper=c(beta0= Inf,
                               a0 = 1,
                               alpha = 1,
                               teta = 16,
                               epsilon_a = 1,
                               epsilon_b = 1,
                               phi = 1,
                               Susc_0 = 1,
                               Carrier_0 = 0.1,
                               gamma = 5.2 ,
                               mu    = (1 / 54) ,
                               rho   = 52 ,
                               act_comp_mening_death = 1), # dimensionless
                       nseq=100)

foreach (guess=iter(guesses,"row"),.combine=rbind,
         .packages=c("pomp","magrittr"),.errorhandling="remove",
         .export="SCIRS_pomp",.inorder=FALSE) %dopar% {
                 SCIRS_pomp %>% 
                         mif2(
                                 start=unlist(guess),
                                 #start= c(SCIRS_theta, SCIRS_init_state),
                                 Nmif=100,Np=1000,transform=TRUE,
                              cooling.fraction.50=0.001,cooling.type="geometric",
                              rw.sd=rw.sd(beta0= 7.5000000000,
                                          a0 = 0.0001220131,
                                          alpha = 0.0935247912,
                                          teta = 13,
                                          epsilon_a = 0.0010000000,
                                          epsilon_b = 0.0010000000,
                                          phi = 0.0023160179,
                                          Susc_0 = 0.1000000000,
                                          Carrier_0 = 0.10000,
                                          gamma = 0.1 ,
                                          mu    = 0.0003561254 ,
                                          rho   = 0.01 ,
                                          act_comp_mening_death = 1)) %>%
                         mif2() -> mf
                 # ll <- logmeanexp(replicate(5,logLik(pfilter(mf))),se=TRUE)
                 # data.frame(loglik=ll[1],loglik.se=ll[2],as.list(coef(mf)))
                 
         } 


