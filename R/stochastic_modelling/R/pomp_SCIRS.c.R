
## define deterministic skeleton for POMP Object.
SCIRS_determ.c<-'
        double a, beta, births;
        double year = 325.0;
        double pi = 3.141593;
        double rate[11]; 
        double term[11];
        
        // I commented the following out as i"m handling population size from the
        // covariable data-frame

        // double population = Susc + Carrier + Ill + Recov;
        
        // Seasonal function for invasion
        a = a0*((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a));
        
        // When we make no assumption about the magnitude of the variations
        // a = a0*(1 + epsilon_a*cos(2*pi*(t-teta)/year));
        
        // Seasonal function for transmission
        beta = beta0*(1 + epsilon_b*cos(2*pi*(t-teta)/year));
        
        // What about using a sin function instead of cosine to model the seasonal variations??? 
         //a = a0*((49.5*epsilon_a)*sin(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a));
         //a = a0*(1+epsilon_a*sin(2*pi*(t-teta)/year));
         //beta = beta0*(1+epsilon_b*sin(2*pi*(t-teta)/year));
        
        // births
        births = mu*population + ((gamma*Ill)*act_comp_mening_death);

        // act_comp_mening_death is a variable that I can set to 0 or 1
        // from the parameters file, to decide if births are compensated
        // by both natural and meningitis related death. Since meningitis case fatality
        // is relatively low one might suspect if effect on total population is marginal. 
        // but with small population sizes it can have an effect.
        
        // The transition rates
        rate[0] = births; // births into suceptibles
        rate[1] = beta*Susc*(Carrier + Ill)/population; // infection 
        rate[2] = a;  //invasion
        rate[3] = alpha; // carriage recovery 
        rate[4] = rho; // Disease recovery
        rate[5] = phi; // Resuceptibility
        
        rate[6] = mu; // natural death of S
        rate[7] = mu;  // natural death of R
        rate[8] = gamma;  // meningitis death of I
        rate[9] = mu;  // natural death of I
        rate[10] = mu;  // natural death of C
        
        // Number of individuals moving between the model compartements during dt
        term[0] = rate[0];
        term[1] = rate[1];
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
'

## define the stockastic skeleton for POMP object
SCIRS_stock.c<-'
        double a, beta, births;
        double year = 325.0;
        double pi = 3.141593;
        double rate[11];
        double trans[11];
        
        // I commented the following out as i"m handling population size from the
        // covariable data-frame

        //double population = Susc + Carrier + Ill + Recov;
        
        // seasonal function for invasion
        a = a0*((49.5*epsilon_a)*cos(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a));
        
        //a = a0*(1 + epsilon_a*cos(2*pi*(t-teta)/year));
        
        // seasonal function for transmission
        beta = beta0*(1 + epsilon_b*cos(2*pi*(t-teta)/year));

       // What about using a sin function instead of cosine to model the seasonal variations??? 
         //a = a0*((49.5*epsilon_a)*sin(2*pi*(t-teta)/year)+(1 + 49.5*epsilon_a));
         //a = a0*(1+epsilon_a*sin(2*pi*(t-teta)/year));
         //beta = beta0*(1+epsilon_b*sin(2*pi*(t-teta)/year));
        
        // births
        births = mu*population + ((gamma*Ill)*act_comp_mening_death);

        rate[0] = births; // births into suceptibles

        // The following rate will be used below to draw the number of individual to transition 
        // between each model compartement at time dt
        
        rate[1] = beta*(Carrier + Ill)/population; // infection.
        rate[2] = a;  //invasion
        rate[3] = alpha; // carriage recovery 
        rate[4] = rho; // Disease recovery
        rate[5] = phi; // Resuceptibility
        
        rate[6] = mu; // natural death of S
        rate[7] = mu;  // natural death of R
        rate[8] = gamma;  // meningitis death of I
        rate[9] = mu;  // natural death of I
        rate[10] = mu;  // natural death of C
        
        trans[0] = rpois(rate[0]*dt);	// Assuming births are Poisson distributed
        
        reulermultinom(1,Susc,&rate[1],dt,&trans[1]);
        
        reulermultinom(1,Susc,&rate[6],dt,&trans[6]); 
        
        reulermultinom(1,Carrier,&rate[2],dt,&trans[2]);
        
        reulermultinom(1,Carrier,&rate[3],dt,&trans[3]); 
        reulermultinom(1,Carrier,&rate[10],dt,&trans[10]);
        
        reulermultinom(1,Ill,&rate[4],dt,&trans[4]);
        
        reulermultinom(1,Ill,&rate[8],dt,&trans[8]); 
        reulermultinom(1,Ill,&rate[9],dt,&trans[9]);
        
        reulermultinom(1,Recov,&rate[5],dt,&trans[5]); 
        reulermultinom(1,Recov,&rate[7],dt,&trans[7]);
        
        // balance the equations with transitions
        Susc += trans[0] - trans[1] - trans[6] + trans[5];
        Carrier += trans[1] - trans[2] - trans[3] - trans[10];
        Ill += trans[2] - trans[4] - trans[8] - trans[9];
        Recov += trans[3] + trans[4] - trans[5] - trans[7];
        Inc += trans[2]; // true incidence is cumulative I over dt
'


