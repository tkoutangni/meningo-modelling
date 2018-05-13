#===============================================================
#Parameters to initialize and simulate the pomp models
# ==============================================================
# Constants
week = 7
year = 365.25
month = 30


life_expectency = 52 # weeks (52 years)*52weeks
duration_of_meningitis_acute_phase = 1 #week
case_fatality = 0.1 # case fatality of 10% per week
timing_of_seasonal_pick = 13  # Timing of seasonal pick in weeks. 
# Symptoms of bacterial meningitis can appear quickly or over several days. Typically they develop within 3 to 7 days after exposure src(https://www.cdc.gov/meningitis/bacterial.html)
duration_of_assymptomatic_carriage = 1 # weeks (we can assume a carriage duration of between 1 week to 
# 6 months or 1 year typically)
average_case_carrier_ratio = 0.001 # per month (estimate from the meta-analysis was monthly)
natural_immunity_duration = 6 #weeks (assume imunity duration of between 1 month and 5 years)
average_transmission_per_day = 0.25 #day (estimate from previous work was per day)


# initial values of model state variables (in proportion)
# SCIRS_init_state <- c(
#         Susc_0 = 0.96,
#         #0.99,
#         Carrier_0 = 0.0001,
#         # assumed approximately 1% of carriers in the population at beguning
#         Ill_0 = 0,
#         Recov_0 = 1 - (0.96 + 0.0001),
#         #0,
#         Inc_0 = 0
# )

teta_fixed <- c(
        # parameters are in years or per years as appropriate
        #Fixed params
        gamma = case_fatality*52,#1e-20, # death rate from meningitis
        mu    = 1e-20, #1/life_expectency,#natural death rate per year 
        rho   = 1/duration_of_meningitis_acute_phase*52, #rate of recovery from disease 
        act_comp_mening_death = 1, # dimensionless
        alpha = 1/duration_of_assymptomatic_carriage*52, #year -1
        teta = timing_of_seasonal_pick/52 #years
)



SCIRS_theta <- c(
        teta_fixed,
        beta0 = 60 ,
        a0 = average_case_carrier_ratio*12,
        epsilon_a = 0.01, #0.003086651,
        epsilon_b = 0.01, #0.9707787,
        phi = 1/natural_immunity_duration*52 #year-1
)

hs_init_guess <-
        function(district_name = "SEGUENEGA",
                 hs_name_contains = "bema") {
                specified_district_previous_est <-
                        previous_estimates[grep(pattern = district_name, rownames(previous_estimates)),]
                specified_district_previous_est %>%
                        .[grep(hs_name_contains, rownames(.)),]
                # .[,c(names(get_unknown_theta_guess(.)))]
        }

theta_bema <- unlist(c(teta_fixed,
                       get_unknown_theta_guess(
                               hs_init_guess(hs_name_contains = "bema")[1,]
                       )))

theta_goubre <- unlist(c(teta_fixed,
                         get_unknown_theta_guess(
                                 hs_init_guess(hs_name_contains = "goubr")[1,]
                         )))

theta_kossouka <- unlist(c(teta_fixed,
                           get_unknown_theta_guess(
                                   hs_init_guess(hs_name_contains = "kossouka")[1,]
                           )))

theta_bouga <- unlist(c(teta_fixed,
                        get_unknown_theta_guess(
                                hs_init_guess(hs_name_contains = "bouga")[1, ]
                        )))

# initial states variable
#

inits_states_guess <- function(hs_name_contains, population_size) {
        inits = hs_init_guess(hs_name_contains = hs_name_contains)[1, ][c("CarrierProp", "Susc0")]/population_size
        #print(paste(inits["CarrierProp"],inits["Susc0"],sep = " "))
        
        Carrier_0 = inits[, "CarrierProp"]
        Susc_0 = inits[, "Susc0"]
        Recov_0 = 1 - Carrier_0 - Susc_0
        
        return(c(
                Carrier_0 = Carrier_0,
                Susc_0 = Susc_0 ,
                Ill_0 = 0,
                Recov_0 = Recov_0,
                Inc_0 = 0
        ))
}


goubr_inits <- c(
        Carrier_0 = 0.0002219263,
        Susc_0 = 1 - 0.0002219263,
        Ill_0 = 0,
        Recov_0 = 0,
        Inc_0 = 0
)

bouga_inits <- c(
        Carrier_0 = 0.0003385318,
        Susc_0 = 1 - 0.0003385318,
        Ill_0 = 0,
        Recov_0 = 0,
        Inc_0 = 0
        
)

# Parameters and initial states to simulate and fit the deter and stoch model with epidemic data.

guess_epi_initial_states<-function(Carrier_0, Recov_0){
                Susc_0 = 1 - Carrier_0 - Recov_0
                Carrier_0 = Carrier_0
                Ill_0 = 0
                Recov_0 = 1- (Susc_0 + Carrier_0)
                Inc_0 = 0      
        return(c(
                Susc_0 = Susc_0, 
                Carrier_0 = Carrier_0,
                Ill_0 = Ill_0,
                Recov_0 = Recov_0))
}


epi_initial_state_guess <- guess_epi_initial_states(0.01, 0.0)

epi_initial_param_guess<-c(
        teta_fixed,
        # estimates from previous work
        beta0 = 60,
        a0 = 0.007,
        alpha = 52, #6 month
        teta = 0.264,
        epsilon_a = 0.5,
        epsilon_b = 0.5,
        phi = 1 # 6 month
)


# epi_initial_param_guess<-c(
#         # parameters units are years or /years
#         teta_fixed,
#         # estimates from previous work
#         beta0 = 80,
#         a0 = 0.06,
#         alpha = 1/(1/52),
#         teta = (112/year)+2006,
#         epsilon_a = 0.1,
#         epsilon_b = 0.1,
#         phi = 1/(10/12)
# )



# epi_initial_param_guess<-c(
#         #parameters are per week or in weeks units
#         teta_fixed,
#         # estimates from previous work
#         beta0 = average_transmission_per_week,
#         a0 = average_case_carrier_ratio,
#         alpha = 1/duration_of_assymptomatic_carriage, # per week
#         teta = 13,
#         epsilon_a = 0.01,
#         epsilon_b = 0.0,
#         phi = 1/duration_of_natural_immunity
# )

lower_bound<-c(
        beta0 = 0, a0 = 0.002*12, alpha = 1/(1/52), 
        teta = (91/year)+2006, epsilon_a = 0, epsilon_b = 0, 
        phi = 1/(1/12) , Susc_0 = 0, Susc_0 = 0, Carrier_0 = 0,  
        Ill_0 = 0,  Recov_0 = 0 
        
)

upper_bound<-c(
        beta0 = 300, a0 = 0.033*12, alpha = 1/(52/52), 
        teta = (112/year)+2006, epsilon_a = 0.975, epsilon_b = 0.975, 
        phi = 1/(60/12) , Susc_0 = 1,  Susc_0 = 1, Carrier_0 = 0.3,  
        Ill_0 = 0,  Recov_0 = 1 
        
)





# function defined to get unkown parmeters values from the matrice of result of previous works;

#get_unknown_theta_guess()

# Vector of parameters estimates for bema health center 2006 in previous work using deterministic
# Params for simulations when time vectore is semaine 1:52
# SCIRS_theta<-c(
#         gamma = 5.2 / year,
#         mu    = (1 / 54) / year,
#         rho   = 52 / year,
#         act_comp_mening_death = 1, # dimensionless
#         # estimate from previous work
#         beta0 = 0.2988836,
#         a0 = 0.0001737079,
#         alpha = 0.1331496,
#         teta = 91,
#         epsilon_a = 0.5,#0.003086651,
#         epsilon_b = 0.5,#0.9707787,
#         phi = 0.003297274
# )


# simulate the model with parameters on a weekly basis (7 days)
# SCIRS_theta<-c(
#         # recovery of from disease (1 week on average)
#         rho   = 1/week,
#         # 10% case fatality within a week (acute phase of disease last a week on average)
#         gamma = 0.1 ,
#         # average life expectancy of 54 years in the region
#         mu    = 1 / 52,
#         # dimensionless
#         act_comp_mening_death = 1,
#         # rate of infection of a susceptible per week
#         beta0 = 0.2988836*week,
#         # rate at which assymptomatic carriers become ill per week
#         a0 = 0.0001737079*week,
#         # Carriage clearance rate per week among assymptomatic carriers
#         alpha = 0.1331496*week,
#         # timing of hyperendemic or epidemic curve pick (in weeks)
#         teta = 13,
#         # seasonal forcing of invasion rate
#         epsilon_a = 0.003086651,
#         # seasonal forcing of infection rate
#         epsilon_b = 0.9707787,
#         # Rate of loss of natural immunity per week
#         phi = 0.003297274*week
# )
                                                                                                                                                         

