#===============================================================
#Parameters to initialize and simulate the pomp models 
# ==============================================================
# Constants
week = 7
year = 365.25
month = 30

# For simulation with time in julian years (parameter time scale is yearly based)
SCIRS_theta<-c(
        gamma = 5.2 ,
        mu    = (1 / 54) ,
        rho   = 52 ,
        act_comp_mening_death = 1, # dimensionless
        # estimates from previous work
        beta0 = 80,
        a0 = 0.0001737079*year,
        alpha = 0.1331496*year,
        teta = 95,
        epsilon_a = 0.01,#0.003086651,
        epsilon_b = 0.01,#0.9707787,
        phi = 0.003297274*year
)


# initial values
SCIRS_init_state <- c(
        Susc_0 = .99,
        Carrier_0 = .01, # assumed approximately 1% of carriers in the population at beguning
        Ill_0 = 0,
        Recov_0 = 0,
        Inc_0 = 0
)

# values used as initial guess in previous determinsitic modelling works
# SCIRS_theta<-c(
#         # Uncertain parameters
#         beta0 = 80/year, # average/baseline transmission
#         a0 = .012/year, # invasion/ rate of disease among assymptomatic carriers
#         alpha = 26/year, # carriage clearance rate
#         phi = 1/(2*week), # assuming an average carriage duration of 2 weeks.
#         epsilon_a = 0, # amplitude of seasonal forcing of invasion
#         epsilon_b = 0, # amplitude of seasonal forcing of transmission
# 
#         # Parameter more or less certain infered or estimated from the litterature.
# 
#         gamma = 5.2/year, # meningitis death rate
#         mu = 1/54/year, # natural death rate
#         rho = 1/week,   #Disease recovery rate.Meningitis acute phase last on average a week.
#         teta = 91, # the average number of days from the begining of the calendar year when disease
#         # pick
#         # b = mu + gamma*Ill
#         act_comp_mening_death = 1 # this is constant to compansate birth with meningitis related death
# )

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
#         epsilon_a = 0.1,#0.003086651,
#         epsilon_b = 0.0,#0.9707787,
#         phi = 0.003297274
# )
# ====================================================
#Other parameters values or time unit tried
#=====================================================
# SCIRS_theta<-c(
#         gamma = 5.2 ,
#         mu    = 1 / 54,
#         rho   = 52 ,
#         act_comp_mening_death = 1, # dimensionless
#         # estimate from previous work
#         beta0 = 60,
#         a0 = 0.002*12,
#         alpha = 26,
#         teta = 91,
#         epsilon_a = 0.03086651,
#         epsilon_b = 0.03, #0.9707787,
#         phi = 1.2
# )

# life_expectency = 54 # years
# Duration_disease = 1/52 #years (1 week on average)
# meningitis_deaths = 10/100 #(up to 10 cases in 100 can die of disease in a week, assuming disease last on average a week)
# meningitis_deaths_rate_in_a_year = meningitis_deaths*52
# Duration_carriage = 2/52 # years (assume an average of 2 weeks duration for carriage)
# Duration_immunity = 1 #year
# 
# SCIRS_theta<-c(
#         gamma = meningitis_deaths_rate_in_a_year , # meningitis death per year
#         mu    = 1 / life_expectency, # natural death rate
#         rho   = 1/Duration_disease ,
#         act_comp_mening_death = 1, # dimensionless
#         # estimate from previous work
#         beta0 = 26, # strictly positive number
#         a0 = 0.012*12,
#         alpha = 1/Duration_carriage,
#         teta = 14/52, #years : soit semaine 16 soit 91ème jour depuis 1er janvier 
#         epsilon_a = 0.01,#0.003086651,
#         epsilon_b = 0.01,#0.9707787,
#         phi = 1
# )
