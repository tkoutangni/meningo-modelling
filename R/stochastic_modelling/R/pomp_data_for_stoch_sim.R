# May Need to source "R/run_first.R" if it has never been sourced in the current R session
# eg. from another script etc ...
#source('R/run_first.R')
source('R/rpackages.R')
#=======================
library(ggplot2)
library(plyr)
library(reshape2)
library(magrittr)
library(pomp)

# the raw data with population zise column
head(raw_data_subset_sort)
# pick data from the district of seguenega to start with.
seguenega_district<-subset(raw_data_subset_sort, district=="SEGUENEGA")

cbind(sapply(seguenega_district, class)) # check each column class
# change the number of cases to integer class
seguenega_district$meningite.cas<-as.integer(seguenega_district$meningite.cas)
#convert distance from string to numeric
seguenega_district$distance<-as.numeric(seguenega_district$distance)
#convert semaine from numeric to integer
seguenega_district$semaine<-as.integer(seguenega_district$semaine)
#convert annee from numeric to integer
seguenega_district$annee<-as.integer(seguenega_district$annee)

cbind(sapply(seguenega_district, class)) # check each column class
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



meningitis_seguenega_bema %>%
        mutate(date = seq(as.Date("2004-01-04"), length = length(semaine), by = "week")) %>%
        mutate(year=as.integer(format(date,"%Y"))) %>%
        mutate(time=(julian(date,origin=as.Date("2004-01-04")))/365.25+2004) -> meningo_data_seguenega_bema

# get demographic data into a seperate variable for future use in pomp object
meningo_data_seguenega_bema %>%
        subset(select= c(semaine, year, time, population)) -> demog_seguenega_bema

# take the time and case count cols only
meningo_data_seguenega_bema %>%
        subset(select=c(semaine, time, meningite.cas)) -> ready_meningo_data_seguenega_bema



# check the proportion of missing value for the reported weekly cases
cat("\n Proportion of missing values for meningitis cases in this health center is: \n",
    mean(is.na(ready_meningo_data_seguenega_bema$meningite.cas)))
# plot the time serie

ready_meningo_data_seguenega_bema %>%
        subset(time>=2006 & time<=2011) %>%
        ggplot(aes(x=time,y=meningite.cas))+geom_line()+
        geom_point() -> plot_cases

# demog_seguenega_bema %>%
#         subset(time>=2006 & time<=2011) %>%
#         ggplot(aes(x=time,y=population))+geom_line()+
#         geom_point() -> plot_demog

# population and case notification data for visulaisation


# plot the population evolution from 2004 to 2012

demog_seguenega_bema %>%
        subset(!duplicated(year)) %>% # not duplicated year
        subset(time>=2005 & time<=2011) %>%
        ggplot(aes(x=time,y=population))+
        geom_line()+geom_point() -> plot_demog

multi_ggplot(plot_cases, plot_demog, cols = 1)

# Important to replace meningitis.cases by meningitis_cases as C++ code may
# later interpret .cases as a method call on the object meningitis and generate comilation 
# errors.
colnames(ready_meningo_data_seguenega_bema)[3]<-"meningitis_cases"


