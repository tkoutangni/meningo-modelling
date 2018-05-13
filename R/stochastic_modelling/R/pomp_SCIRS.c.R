
## define deterministic skeleton for POMP Object.
SCIRS_determ.c<-'
        double a, beta, births;
        double year = 365.25;
        double pi = M_PI;
        double rate[11]; 
        double term[11];
        
        // double population = Susc + Carrier + Ill + Recov;
        // I commented the line above as i store population size in
        // a covariable data-frame.

        // Seasonal function for invasion. Their are some evidence that the invasion
        // parameter < a > could increase up to 100 time during dry season
        
        a = a0*((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a));
        
        // When we make no assumption about the magnitude of variations of parameter
        // < a >
        // a = a0*(1 + epsilon_a*cos(2*pi*(t-teta)/year));
        
        // Seasonal function for transmission
        beta = beta0*(1 + epsilon_b*cos(2*pi*(t-teta)/year));
        
        // What about using a sin function instead of cosine to model the seasonal variations?? 
         //a = a0*((49.5*epsilon_a)*sin(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a));
         //a = a0*(1+epsilon_a*sin(2*pi*(t-teta)/year));
         //beta = beta0*(1+epsilon_b*sin(2*pi*(t-teta)/year));
        
        // births
        births = mu*population + ((gamma*Ill)*act_comp_mening_death);
        // births = 1e-12; //negligable birth and natural death rates.
        

        // act_comp_mening_death is a variable that I can set to 0 or 1
        // from the parameters file, to decide if births are compensated
        // by both natural and meningitis related death. Since meningitis case fatality
        // is relatively low it effect on large population may be marginal. 
        // but in this case we are simulating relatively small populations (size around 10000).
        
        // The transition rates
        rate[0] = births; // births into suceptibles
        rate[1] = beta*(Carrier + Ill)/population; // infection 
        rate[2] = a;  //invasion
        rate[3] = alpha; // carriage recovery 
        rate[4] = rho; // Disease recovery
        rate[5] = phi; // Resuceptibility
        
        rate[6] = mu; // natural death of S
        rate[7] = mu;  // natural death of R
        rate[8] = gamma;  // meningitis death of I
        rate[9] = mu;  // natural death of I
        rate[10] = mu;  // natural death of C
        
        // Number of individuals moving between the model compartements within dt
        term[0] = rate[0];
        term[1] = rate[1]*Susc;
        term[2] = rate[2]*Carrier;
        term[3] = rate[3]*Carrier;
        term[4] = rate[4]*Ill;
        term[5] = rate[5]*Recov;
        
        term[6] = rate[6]*Susc;
        term[7] = rate[7]*Recov;
        term[8] = rate[8]*Ill;
        term[9] = rate[9]*Ill;
        term[10] = rate[10]*Carrier;
        
        // balance the equations
        DSusc = term[0] + term[5] - term[1] - term[6] ;
        DCarrier= term[1] - term[2] - term[3] - term[10];
        DIll = term[2] - term[4] - term[8] - term[9];
        DRecov = term[3] + term[4] - term[5] - term[7];
        DInc = term[2]; // true incidence is cumulative I over dt
 Rprintf(\"%lg %lg %lg %lg %lg %lg\\n\",Susc, Carrier, Ill, Recov, Inc, Susc+Carrier+Ill+Recov);
'

## define the stockastic skeleton for POMP object
SCIRS_stock.c<-'
double a, beta, births;
double year = 365.25;
double pi = M_PI;
double rate[11];
double trans[11];
// Population size in a covariate data.frame instead.
//double population = Susc + Carrier + Ill + Recov;

// Seasonal function for invasion. Their are some evidence that the 
// invasion parameter < a > could be up to 100 -fold the baseline or 
// average value during dry season teta is the time when epidemic picks
a = a0*((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a));

// otherwise when no assumption is made on magnitude of variations of 
// the parameters
//a = a0*(1 + epsilon_a*cos(2*pi*(t-teta)/year));

// seasonal function for transmission
beta = beta0*(1 + epsilon_b*cos(2*pi*(t-teta)/year));

//New  births
//births = mu*population + ((gamma*Ill)*act_comp_mening_death);
births = 1e-20;


rate[0] = births; // births into susceptibles

rate[1] = beta*(Carrier + Ill)/population; // infection.
rate[2] = mu;  // natural death of S

rate[3] = a;     //invasion
rate[4] = alpha; // carriage recovery 
rate[5] = mu;    // natural death of C

rate[6] = rho;    // disease recovery
rate[7] = gamma;  // meningitis death of I
rate[8] = mu;     // natural death of I

rate[9] = phi;  // loss of immunity
rate[10] = mu;  // natural death of R

trans[0] = rpois(rate[0]*dt);	// Assuming births are Poisson distributed
//trans[0] = rpois(43.98/1000*dt);

reulermultinom(2,Susc,&rate[1],dt,&trans[1]);
reulermultinom(3,Carrier,&rate[3],dt,&trans[3]);
reulermultinom(3,Ill,&rate[6],dt,&trans[6]);
reulermultinom(2,Recov,&rate[9],dt,&trans[9]); 

// balance the equations with transitions
Susc    += trans[0] + trans[9] - trans[1] - trans[2];
Carrier += trans[1] - trans[3] - trans[4] - trans[5];
Ill     += trans[3] - trans[6] - trans[7] - trans[8];
Recov   += trans[4] + trans[6] - trans[9] - trans[10];
Inc     += trans[3]; // true incidence is cumulative I over dt
//double total_birth = trans[0];
//double total_susc_death = trans[2];
//double total_recov_death = trans[10];
 Rprintf(\"%lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg \\n\",Susc, Carrier, Ill, Recov, Inc, Susc+Carrier+Ill+Recov, trans[0], trans[2], trans[5], trans[8], trans[10]);
//Rprintf(\"%lg \\n\",trans[0]);
'


#Intial state variables to initialize the model simulation.
SCIRS_initlz <-"
        Susc = nearbyint(population*Susc_0);
        Carrier = nearbyint(population*Carrier_0);
        Ill = nearbyint(population*Ill_0);
        Recov = nearbyint(population*Recov_0);
        //Recov = nearbyint(population - (Susc + Carrier + Ill));
        Inc = 0;
        Rprintf(\"%lg %lg %lg %lg %lg\\n\",Susc, Carrier, Ill, Recov, Inc);
"

# # Intial state variables to initialize the model simulation.
# SCIRS_initlz <-"
# double m = population/(Susc_0+Carrier_0+Ill_0+Recov_0);
# Susc = nearbyint(m*Susc_0);
# Carrier = nearbyint(m*Carrier_0);
# Ill = nearbyint(m*Ill_0);
# Recov = nearbyint(m*Recov_0);
# //Recov = nearbyint(population - (Susc + Carrier + Ill));
# Inc = 0;
# //Rprintf(\"%lg \\n\",m);
# Rprintf(\"%lg %lg %lg %lg %lg\\n\",Susc, Carrier, Ill, Recov, Inc);
# 
# 
# "

# SCIRS_initlz <-"
# double m = population/(Susc_0+Carrier_0+Ill_0+Recov_0);
# Susc = m*Susc_0;
# Carrier = m*Carrier_0;
# Ill = m*Ill_0;
# Recov = m*Recov_0;
# //Recov = nearbyint(population - (Susc + Carrier + Ill));
# Inc = 0;
# //Rprintf(\"%lg \\n\",m);
# Rprintf(\"%lg %lg %lg %lg %lg\\n\",Susc, Carrier, Ill, Recov, Inc);
# 
# "
## define sampling random point observations
SCIRS_rmeas.c<-"
meningitis_cases = rpois(Inc > 0 ? Inc : 0);
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


## define prior density
SCIRS_dprior.c <-"
        lik = dunif(beta0, 0, 120, 1) +
        dunif(a0, 0.002, 0.12, 1) +
        dunif(epsilon_a, 0, 1, 1) +
        dunif(epsilon_b, 0, 1, 1) +
        dunif(phi, 0.2, 12, 1) +
        dunif(Susc_0, 0, 1, 1) + 
        dunif(Carrier_0, 0, 0.3, 1);
        lik = give_log ? lik : exp(lik);
"


## Model tranformed params values c code
SCIRS_toEst.c<-"
// fixed params
        Tgamma = log(gamma);
        Tmu    = log(mu);
        Trho   = log(rho);
        Tact_comp_mening_death = 1;
        Talpha = log(alpha);
        Tteta = log(teta);
        // Unknown params
        Tbeta0 = log(beta0);
        Tphi = log(phi);
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
        Talpha = exp(alpha);
        Tteta = exp(teta);
        // Unknown params
        Tbeta0 = exp(beta0);
        Tphi = exp(phi);
        Tepsilon_a = expit(epsilon_a);
        Tepsilon_b = expit(epsilon_b);
        Ta0 = exp(a0);
        from_log_barycentric (&TSusc_0, &Susc_0, 4);
"

