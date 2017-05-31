rm(list = ls())
source("R/run_first.R")
# data to use as sample 
source('R/stochastic_modelling/R/pomp_data_for_stoch_sim.R')
# load the C code of the deterministic and stochastic model as a string 
# for later use in the pomp_object construction
source('R/stochastic_modelling/R/pomp_SCIRS.c.R')

# load the model initial parameters needed to simulate the pomp object
source('R/stochastic_modelling/R/pomp_SCIRS_params.R')

## Model initial values draws : c code
# Optional; draws from the distribution of initial values of the unobserved Markov state process. Specifically, given a vector of parameters, params and an initial time, t0, the initializer determines the state vector at time t0

SCIRS_initlz <-"
        Susc = nearbyint(population*Susc_0);
        Carrier = nearbyint(population*Carrier_0);
        Ill = nearbyint(population*Ill_0);
        Recov = nearbyint(population*Recov_0);
        Inc = 0;
"

## define point observation probability density
SCIRS_dmeas.c<- '
        double tol = 0.0;
        if(meningitis_cases >0.0){
                lik = dpois(meningitis_cases+tol, Inc, give_log);
        }else{ 
                lik = dpois(meningitis_cases+tol, Inc, give_log);
        }
        

        // print values of state var when log liklihood return non-finite value. just for debuging
        if (!R_FINITE(lik)){ 
                Rprintf(\"%lg %lg %lg %lg %lg %lg\\n\",Susc, Carrier, Ill, Recov, Inc,lik);
        }
        
'

## define sampling random point observations
SCIRS_rmeas.c<-'
        meningitis_cases = rpois(Inc);
        if (meningitis_cases > 0.0) {
                meningitis_cases = nearbyint(meningitis_cases);
        } else {
                meningitis_cases = 0.0;
        }
'

## Model tranformed params values c code
SCIRS_toEst.c<- '
        // fixed params
        Tgamma = log(gamma);
        Tmu    = log(mu);
        Trho   = log(rho);
        Tact_comp_mening_death = 1;
        // unknown params
        Tbeta0 = log(beta0);
        Talpha = log(alpha);
        Tphi = log(phi);
        Tteta = log(teta);
        Tepsilon_a = logit(epsilon_a);
        Tepsilon_b = logit(epsilon_b);
        Ta0 = log(a0);
        to_log_barycentric (&TSusc_0, &Susc_0, 4);
'

## Model untranformed params values c code
SCIRS_fromEst.c<- '
        // fixed params
        Tgamma = exp(gamma);
        Tmu    = exp(mu);
        Trho   = exp(rho);
        Tact_comp_mening_death = 1;
        // unknown params
        Tbeta0 = exp(beta0);
        Talpha = exp(alpha);
        Tphi = exp(phi);
        Tteta = exp(teta);
        Tepsilon_a = expit(epsilon_a);
        Tepsilon_b = expit(epsilon_b);
        Ta0 = exp(a0);
        from_log_barycentric (&TSusc_0, &Susc_0, 4);
'

subset_demog_seguenega_bema<-subset(demog_seguenega_bema, time>=2006 & time<=2007, select = c(semaine, time, population))

sample_data_set<-subset(ready_meningo_data_seguenega_bema, time>=2006 & time<=2007,
                        select =c(semaine, time,  meningitis_cases))


head(subset_demog_seguenega_bema)
head(sample_data_set)

#colnames(subset_demog_seguenega_bema)[1]<-c("time")
#colnames(sample_data_set)[1]<-c("time")

# new pomp model specification for both deterministic and stochastic simulations
time_col_name<-"time"

        pomp(
                #t0=with(sample_data_set,2*time[1]-time[2]), # must be less or equal to the first time in the time column
                #t0=with(sample_data_set,time[1]),
             data = sample_data_set[,c(time_col_name, "meningitis_cases")],
             times= time_col_name,
             t0 = sample_data_set[,time_col_name][1],
             rprocess=euler.sim(Csnippet(SCIRS_stock.c),delta.t=1/52), #stochastic version
             skeleton = vectorfield(Csnippet(SCIRS_determ.c)), # deterministic model
             dmeasure = Csnippet(SCIRS_dmeas.c),
             rmeasure = Csnippet(SCIRS_rmeas.c),
             toEstimationScale=Csnippet(SCIRS_toEst.c),
             fromEstimationScale=Csnippet(SCIRS_fromEst.c),
             initializer = Csnippet(SCIRS_initlz),
             covar = subset_demog_seguenega_bema,
             tcovar= time_col_name, #timevector name corresponding in the covarible dataframe
             zeronames=c("Inc"),
             statenames = c("Susc", "Carrier", "Ill", "Recov", "Inc"),
             paramnames = c("gamma", "beta0", "a0", "alpha", "epsilon_a",
                            "epsilon_b", "mu", "teta", "rho", "phi", "act_comp_mening_death"
                            ,"Susc_0","Carrier_0","Ill_0","Recov_0"
                            )
        ) -> SCIRS_pomp

#ploting the data to be fitted by the model
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
                         #,times=seq(2006.012,2006.99,by=0.019)
                         )

plotTraj(SCIRS.traj, colour = "black")

# The following will compute and plot some simulations. First, I plot n simulations and compare them with the data.

SCIRS_pomp %>% 
        simulate(params=c(SCIRS_theta,SCIRS_init_state),nsim=11,as.data.frame=TRUE,include.data=TRUE) -> SCIRS_sim

SCIRS_sim %>%
        ggplot(aes(x=time,y=meningitis_cases,group=sim,color=(sim=="data")))+
        guides(color=FALSE)+
        geom_line()+ geom_point()+ facet_wrap(~sim,ncol=2)


plotTraj(SCIRS_sim[,which(!names(SCIRS_sim)%in%c("semaine", "population"))], colour = "black")

# check if total population is correct at each time dt
head(rowSums(SCIRS.traj[,c(1:4)]))


# Fitting the deterministic POMP model by maximising the likelihood of 
# the observed data given the model results and parameters.

# Params I want to be fixed. 
fixed_pars<-c("gamma", "mu", "rho", "act_comp_mening_death")
to_estimate_pars<- c(setdiff(c(names(SCIRS_theta), "Susc_0", "Carrier_0"), fixed_pars))

SCIRS.tm <- traj.match(SCIRS_pomp, 
                       start = c(SCIRS_theta, SCIRS_init_state), 
                       est = to_estimate_pars,
                       transform = TRUE
                       #,method="Nelder-Mead"
                       )

print_fit_result<- function(pomp_fitted_object, to_estimate_pars){
        # a function to print the fit result.
        cat("\n Log likelihood: ", logLik(pomp_fitted_object))
        
        cat('\n estimated params values: \n')
        round(coef(pomp_fitted_object),6)[to_estimate_pars]
}

# print the estimates from the fitted pomp model
print_fit_result(SCIRS.tm, to_estimate_pars)

# running the stochastic model with the fitted params from the maximum likelihood estimates
# from trajectory matching the deterministic model (SCIRS.tm)
# Then plot the distribution of the outpout as mean , median and 50 and 95th percentile

SCIRS.tm.sim <- simulate(SCIRS.tm, nsim = 100, as.data.frame = TRUE)
SCIRS.tm.sim<-subset(SCIRS.tm.sim, select = -c(semaine))
plotTraj(SCIRS.tm.sim, data = sample_data_set, state.names = "meningitis_cases")


#===============================================================================================
#=FITTING THE STOCHASTIC MODEL BY ESTIMATION MAXIMUM LIKELIHOOD USING ITERATED FILTERING METHOD
## fitting the stochastic model with Maximum likelihood by iterated filtering (MIF)
#===============================================================================================
fit_stoch_model = FALSE  # swicth on and off, the 

if(fit_stoch_model){
        #prop.sd <- rep(0.001, length(to_estimate_pars))
        prop.sd<- c((SCIRS_theta[c(to_estimate_pars)][1:7]/ 10) ,1, 1)
        names(prop.sd) <- to_estimate_pars
        
        SCIRS.mf <- mif(SCIRS.tm, Nmif = 100, Np = 1000, cooling.fraction.50 = 0.001, 
                        rw.sd = prop.sd)
        SCIRS.mf.sim <- simulate(SCIRS.mf, nsim = 10, as.data.frame = TRUE, include.data = TRUE)
        plotTraj(SCIRS.mf.sim, data = data.frame(test_data_bema), state.names = "obs")
}
#==============================================================================================




