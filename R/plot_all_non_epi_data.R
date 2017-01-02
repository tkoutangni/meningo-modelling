non_epi_health_center_year_data<-list(
    #seguen_data_index = which(!ls()[c(grep("seguen_", ls()))]%in%c("seguen_2004_2010","seguen_2006_2007","seguen_2006_2010)"))
    seguen_2006 , seguen_2007, seguen_2008, seguen_2009, seguen_2010,
    hounde_2004, hounde_2005, hounde_2006, hounde_2007, hounde_2008, 
    hounde_2009, hounde_2010, lena_2004, lena_2005, lena_2006, lena_2007,
    lena_2008, lena_2010, lena_2010, Kvigue_2008, Kvigue_2009, Kvigue_2010
)

names(non_epi_health_center_year_data)<-c(
    "seguen_2006" , "seguen_2007", "seguen_2008", "seguen_2009", "seguen_2010",
    "hounde_2004", "hounde_2005", "hounde_2006", "hounde_2007", "hounde_2008", 
    "hounde_2009", "hounde_2010", "lena_2004", "lena_2005", "lena_2006", "lena_2007",
    "lena_2008", "lena_2010", "lena_2010", "Kvigue_2008", "Kvigue_2009", "Kvigue_2010"
)


remove_jullian_days = function(zoo_serie){
    zoo_serie$julian_day<-NULL
    return(zoo_serie)
}

# Add population size to the time series
add_population_size = function(zoo_serie, population){
    zoo_serie$population<-population
    return(zoo_serie)
}


# prepare data for plot with ggplot2: no need to plot the julian days so we remove
# them in the mean time.
non_epi_health_center_year_data<-lapply(non_epi_health_center_year_data,remove_jullian_days)


melt_non_epi_data_list = lapply(non_epi_health_center_year_data, 
                                melt_data_all_year)



# Using the remove_missing function from ggplot2 to remove NAs
#toto = remove_missing(test$seguen_2006,name="weekly_incid")

# only consider health centers where we have complete data at least in a year.
hounde_melt_data_all_year = subset(
    rbindlist(melt_non_epi_data_list), 
    grepl("^HOUND", rbindlist(melt_non_epi_data_list)$health_center_name)
    )


other_district_melt_data_all_year = subset(
    rbindlist(melt_non_epi_data_list), 
    !grepl("^HOUND", rbindlist(melt_non_epi_data_list)$health_center_name)
)


hyperendemic_data_to_plot = rbind(other_district_melt_data_all_year, hounde_melt_data_all_year)

hyperendemic_data_plots<- ggplot_time_series_all_years(hyperendemic_data_to_plot, ncol = 4) + theme_bw()

hyperendemic_data_plots
