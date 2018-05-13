# May Need to source 
# "R/run_first.R" and 'R/rpackages.R' if they have not been sourced yet

# The raw data with population zise column
head(raw_data_subset_sort)
str(raw_data_subset_sort)

# pick data from the district of seguenega to start with.
get_district_data<-function(raw_data_subset_sort, district_name){
        
        selected_district<-subset(raw_data_subset_sort, district==district_name)
        
        cbind(sapply(selected_district, class)) # check each column class
        # change the number of cases to integer class
        selected_district$meningite.cas<-as.integer(selected_district$meningite.cas)
        #convert distance from string to numeric
        selected_district$distance<-as.numeric(selected_district$distance)
        #convert semaine from numeric to integer
        selected_district$semaine<-as.integer(selected_district$semaine)
        #convert annee from numeric to integer
        selected_district$annee<-as.integer(selected_district$annee)
        
        cbind(sapply(selected_district, class)) # check each column class
        return(selected_district)
}

################################
# Get data per district
#################################

lena_district_data<-get_district_data(raw_data_subset_sort, "LENA")
seguenega_district<-get_district_data(raw_data_subset_sort, "SEGUENEGA")

# seguenega_district<-subset(raw_data_subset_sort, district=="SEGUENEGA")
# 
# cbind(sapply(seguenega_district, class)) # check each column class
# # change the number of cases to integer class
# seguenega_district$meningite.cas<-as.integer(seguenega_district$meningite.cas)
# #convert distance from string to numeric
# seguenega_district$distance<-as.numeric(seguenega_district$distance)
# #convert semaine from numeric to integer
# seguenega_district$semaine<-as.integer(seguenega_district$semaine)
# #convert annee from numeric to integer
# seguenega_district$annee<-as.integer(seguenega_district$annee)
# 
# cbind(sapply(seguenega_district, class)) # check each column class
# pick the time serie of meningitis weekly cases from one health center (eg. bema)

#bema<-seguenega_district[grep("bema", c(seguenega_district$fs)),]

head(subset(seguenega_district[grep("bema", c(seguenega_district$fs)),], 
            district=="SEGUENEGA", 
            select=c(region.x, 
                     district, 
                     fs, 
                     annee, 
                     semaine , 
                     meningite.cas,
                     population)))

# store demographics data for this health center time serie into a seperate variable
# demog_seguenega_bema<-subset(seguenega_district[grep("bema", c(seguenega_district$fs)),], 
#                         district=="SEGUENEGA", 
#                         select=c(region.x, 
#                                  district, 
#                                  fs, 
#                                  annee, 
#                                  semaine ,
#                                  population))



# store actual meningitis cases notification data for this health center into a seperate variable

meningitis_seguenega_bema<- subset(seguenega_district[grep("bema", c(seguenega_district$fs)),], 
                                   district=="SEGUENEGA", 
                                   select=c(region.x, 
                                            district, 
                                            fs, 
                                            annee, 
                                            semaine , 
                                            meningite.cas,
                                            population,
                                            distance))
# view each column  class
cbind(sapply(meningitis_seguenega_bema, class))


# add a new time vector with approximate time lenght equal a week
meningitis_seguenega_bema %>%
        mutate(date = seq(as.Date("2004-01-04"), length = length(semaine), by = "week")) %>%
        mutate(year=as.integer(format(date,"%Y"))) %>%
        # it is important to hard code year here because the variable is not accessible from here
        mutate(time=(julian(date,origin=as.Date("2004-01-04")))/365.25+2004) -> meningo_data_seguenega_bema

# get demographic data into a seperate variable for future use in pomp object
meningo_data_seguenega_bema %>%
        subset(select= c(semaine, year, time, population)) -> demog_seguenega_bema


# take the time and case count cols only
meningo_data_seguenega_bema %>%
        subset(select=c(semaine, time, meningite.cas)) %>%
        # Important to replace meningitis.cases by meningitis_cases as C++ code may
        # later interpret .cases as a method call on the object meningitis and generate compilation 
        # errors.
        rename(., meningitis_cases = meningite.cas) -> ready_meningo_data_seguenega_bema



# check the proportion of missing value for the reported weekly cases
cat("\n Proportion of missing values for meningitis cases in this health center is: \n",
    mean(is.na(ready_meningo_data_seguenega_bema$meningitis_cases)))
# plot the time serie

ready_meningo_data_seguenega_bema %>%
        subset(., time>=2006 & time<=2011) %>%
        ggplot(aes(x=time,y = meningitis_cases))+geom_line()+
        geom_point() -> plot_cases

# plot the population evolution for a given time frame

demog_seguenega_bema %>%
        subset(!duplicated(year)) %>% # not duplicated year
        subset(time>=2005 & time<=2012) %>%
        ggplot(aes(x=time,y=population))+
        geom_line()+geom_point() -> plot_demog

multi_ggplot(plot_cases, plot_demog, cols = 1)

#==========================================================
# loading parameters estimates values from previous work
# ==========================================================
load("data/processed_data/new_processed_february_2016/mle_a0_beta0_ForcEstimates_matrice.RData")

previous_estimates<-mle_a0_beta0_ForcEstimates_matrice

# get only column for which we want tu reuse the value of previous estimations as starting point

previous_estimates_needed<- previous_estimates

seguenega_district_previous_est<-previous_estimates_needed[grep(pattern = "SEGUENEGA", rownames(previous_estimates)),]



# a function to get specific health center data
get_hs_data<-function(data, district, hs_name_contains){
        meningitis_seguenega<- subset(data[grep(hs_name_contains, c(data$fs)),], 
                                           district==district, 
                                           select=c(region.x, 
                                                    district, 
                                                    fs, 
                                                    annee, 
                                                    semaine , 
                                                    meningite.cas,
                                                    population,
                                                    distance))
        
        # add a new time vector with approximate time lenght equal a week
        meningitis_seguenega %>%
                #as.Date(paste(2004, semaine, 1, sep="-"), "%Y-%U-%u")
                mutate(date = seq(as.Date(paste(min(annee), semaine[1], 1, sep = "-"), "%Y-%U-%u"), length = length(semaine), by = "week")) %>%
                mutate(year=as.integer(format(date,"%Y"))) %>%
                # it is important to hard code year here because the variable is not accessible from here
                # I'm using 2004 as origine here for julian time calculation since the data base started in 2004 so I take first week of that year as origin
                # -1 one day on date to correct for the one day lag due to the choice of day 1 as starting date
                mutate(time=((julian(date , origin=as.Date("2004-01-04")))/365.25 + (2004))) -> meningo_data_seguenega
        
        # get demographic data into a seperate variable for future use in pomp object
        meningo_data_seguenega %>%
                subset(select= c(date, semaine, year, time, population)) -> demog_data
        
        
        # take the time and case count cols only
        meningo_data_seguenega %>%
                subset(select=c(date, semaine, time, meningite.cas)) %>%
                # Important to replace meningitis.cases by meningitis_cases as C++ code may
                # later interpret .cases as a method call on the object meningitis and generate compilation 
                # errors.
                rename(., meningitis_cases = meningite.cas) -> ready_data
        
        return(list(ready_data = ready_data, 
                    demog_data = demog_data))
}




seguenega_bema_data<-get_hs_data(seguenega_district, 
                  district = "SEGUENEGA",
                  hs_name_contains = "bema")
# view each column  class
head(diff(seguenega_bema_data$ready_data$time))
plot(seguenega_bema_data$ready_data$date, seguenega_bema_data$ready_data$meningitis_cases, type="l", las=1, ylab="weekly cases counts", xlab="Times(calendar weeks)")

plot(seguenega_bema_data$ready_data$time, seguenega_bema_data$ready_data$meningitis_cases, type="l", las=1 , ylab="weekly cases counts", xlab="Times(calendar weeks)")



# plot the time serie

subset_visualize_data_and_covariable<-function(data_and_covariable_list, start_time, end_time, ncol = 2, main = "Plot main title"  ){
        
        cat("\n Proportion of missing values for meningitis cases in this health center is: \n",
            mean(is.na(data_and_covariable_list$ready_data$meningitis_cases)))
        
        data_and_covariable_list$ready_data %>%
                subset(., time>=start_time & time<end_time) %>%
                ggplot(aes(x=date,y = meningitis_cases))+geom_line()+
                geom_point() -> plot_cases
        
        # plot the population evolution for a given time frame
        
        data_and_covariable_list$demog_data %>%
                subset(!duplicated(year)) %>% # not duplicated year
                subset(time>=start_time & time<end_time) %>%
                ggplot(aes(x=date,y=population))+
                geom_line()+geom_point() -> plot_demog
        

        #multi_ggplot(plot_cases, plot_demog, cols = 1)
        return(list(plot_cases = plot_cases, plot_demog = plot_demog))
        
        
}



