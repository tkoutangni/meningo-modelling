# Data visualisation for epidemic data.

#rm(list = ls(pattern = "^toto"))
current_workspace_variables = ls()
current_workspace_variables[c(grep("dfEpi", ls()))] # the name of data frames containing all
# health centre that experience epidemic at least once in the course of 2004 - 2012

# Visualize  all the selected data.
per_100000 = 1e+05

plot_time_serie = function(zoo_time_serie){
  xyplot(zoo_time_serie, 
              xlab="Time (Calendar Weeks)",
              ylab="Incid. per 100,000",type="b",pch=16)
}




list_of_zoo_time_serie_epi_hs = list(dfEpiboulsa*per_100000, dfEpiDafra*per_100000, dfEpiDande*per_100000, dfEpiGourcy*per_100000 , 
     dfEpiHounde*per_100000, dfEpiKvigue*per_100000, dfEpiLena*per_100000, dfEpiOrodora*per_100000, 
     dfEpiOuahigouya*per_100000, dfEpiSeguenega*per_100000, dfEpiYako*per_100000)


#plot_time_serie(subset(dfEpiboulsa*per_100000,year(time(dfEpiboulsa))>=2006&year(time(dfEpiboulsa))<=2008))
#plot_time_serie(subset(dfEpiDafra*per_100000,year(time(dfEpiDafra))>=2006&year(time(dfEpiDafra))<=2008))
#plot_time_serie(subset( dfEpiGourcy*per_100000,year(time(dfEpiGourcy))>=2006&year(time(dfEpiGourcy))<=2008))

#plot_time_serie(subset(dfEpiLena*per_100000,year(time(dfEpiLena))>=2006&year(time(dfEpiLena))<=2008))
#plot_time_serie(subset(dfEpiHounde*per_100000,year(time(dfEpiHounde))>=2006&year(time(dfEpiHounde))<=2008))


## a function to add julian days since end of 2003
add_julian_day = function(zoo_time_serie,year){  
    selected_year = zoo_time_serie[year(time(zoo_time_serie))==year]
    data_with_julian_day = julian_day(data=selected_year, orig="2003-12-28")
    return(data_with_julian_day)
} #end of add_julian_day function.


# A function for melting a data frame for use in a ggplot function
melt_data = function(zoo_time_serie,year){
    # this function assume the zoo time serie has a column named "julian_day"
    #remove the "julian_day_column_first
    #[,-which(colnames(zoo_time_serie)%in%c("julian_day"))]
    subset_data = subset(zoo_time_serie,year(time(zoo_time_serie))==year)
    # transforme the zoo time serie into a data.frame with a column
    # containing the dates
    subset_data_frame = fortify(subset_data)
    # melt the data / reshape it in long format for use by ggplot2
    melt_data_frame = melt(subset_data_frame, id=c("Index"),variable.name = "health_center_name",
                           value.name = 'weekly_incid')
    return(melt_data_frame)
} #end melt data function

melt_data_all_year = function(zoo_time_serie){
    
    data_frame = fortify(zoo_time_serie)
    # melt the data / reshape it in long format for use by ggplot2
    melt_data_frame = melt(data_frame, id=c("Index"),variable.name = "health_center_name",
                           value.name = 'weekly_incid')
    return(melt_data_frame)
} #end melt data function


# a function for quick visualization of the epidemic data.

ggplot_time_series = function(melt_data_frame,year){
    p<-ggplot(melt_data_frame,aes(x=Index,y=weekly_incid*per_100000)) + geom_line() + geom_point()
    p<-p+facet_wrap(~health_center_name,ncol=4, scales="free_y") + xlab(paste("Calendar Weeks", year,sep=", ")) + ylab("Weekly Incidence \n per 100,000")
    
    # Apply a locally weighted regression to smooth time series
    #p<- p + stat_smooth(method = "loess", formula = y ~ x, size = 1)
    return (p)
}

ggplot_time_series_all_years = function(melt_data_frame,ncol=4){
    p<-ggplot(data=melt_data_frame[!is.na(melt_data_frame$weekly_incid),] ,aes(x=Index,y=weekly_incid*per_100000)) + geom_line() #+ geom_point()
    p<-p+facet_wrap(~health_center_name,ncol=ncol, scales="free_y") + xlab(paste("Calendar Weeks")) + ylab("Weekly Incidence \n per 100,000")
    
    # Apply a locally weighted regression to smooth time series
    #p<- p + stat_smooth(method = "loess", formula = y ~ x, size = 1)
    return (p)
}

ggplot_rbindlist_data_frames = function(list_of_data_frames){
    # function takes as argument: a clean list of data-frames with the first
    # column being the time serie dates as returned with the fortify 
    # function in ggplot2 package.
    # rbindlist is used to append the elmts (data_frames) of the list by row
    #In lapply the function melt_data is applied to each elmt of list
    # melt_data_all_year is a custum function to reshape the data in a 
    # format appropriate for ggplot2.
    ggplot_time_series_all_years(
        rbindlist(
            lapply(list_of_data_frames, melt_data_all_year))
    )
} # end of fuction  ggplot_rbindlist_data_frames


# keeping only health center with data that is useful.
# a function to remove unwanted health centers in each district.
remove_unwanted_column = function(data_frame_or_zoo_serie,colnames_to_remove){
    # take as first argument a zoo time serie or data frame
    # takes as second argument a vector of the column names to remove
    dfEpi_with_wanted_column = data_frame_or_zoo_serie[,which(!colnames(data_frame_or_zoo_serie)%in%colnames_to_remove)]
    return(dfEpi_with_wanted_column)
}

# select data for a given year from a time serie.
select_ts_year = function(zoo_time_serie,year){
    return(zoo_time_serie[year(time(zoo_time_serie))==year])
}




# important ! to make sure dfEpi_list (if it exist)
# be added to 
# the list that will created bellow
# getting all the time series into a list
rm(list=ls(pattern = "^dfEpi_list"))
dfEpi_list = sapply(ls(pattern = "^dfEpi"),get)

# remoov health centers with no data at all 
# or no epidemic incidence data in a given year
# dfEpiYako collection de donnée uniquement a partir de 2011
# dfEpiOuahigouya pas de données exploitable dans ce district.
dfEpi_list[c("dfEpiYako", "dfEpiOuahigouya")] <- NULL

# applying a simple mooving average to the dfEpi_list elements
# data smoothed on a 2 weeks window.
dfEpi_list = lapply(dfEpi_list,mva_function)

# visualise the smoothed time series
#ggplot_time_series_all_years(melt_data_all_year(dfEpi_list$dfEpiGourcy))


# Health centers which data are not usable or do not meet the.
# epidemic treshold definition.

hc_year_to_remove_2006 =c(
        "LENA|csps bah",
        "GOURCY|csps guiri-guiri",
        "GOURCY|csps koundouba",
        "GOURCY|csps rikiba",
        "BOULSA|csps gargo",
        "BOULSA|csps taffogo",
        "BOULSA|csps tilga",
        "ORODARA|csps badara",
        "ORODARA|csps banflagoué",
        "ORODARA|csps fadona",
        "ORODARA|csps morolaba",
        "ORODARA|csps niamberla",
        "ORODARA|csps sifarasso",
        "ORODARA|csps sindo",
        "SEGUENEGA|csps goungré",
        "SEGUENEGA|csps irim",
        "SEGUENEGA|csps ramsa",
        "HOUNDÉ|csps dahoun")

hc_year_to_keep_2007 = c("GOURCY|csps guiri-guiri",
                           "GOURCY|csps koundouba",
                           "GOURCY|csps rikiba",
                           "SEGUENEGA|dispe kondé tangaye",
                           "SEGUENEGA|csps irim",
                           #"HOUNDÉ|csps fafo",
                            #"BOULSA|csps gargo",
                         "ORODARA|csps niamberla",
                         "ORODARA|csps sindo"
                         )


hc_year_to_keep_2008 = c("GOURCY|csps guiri-guiri",
                           "GOURCY|csps koundouba",
                           "GOURCY|csps rikiba",
                           "SEGUENEGA|dispe kondé tangaye",
                         "BOULSA|csps gargo",
                         "BOULSA|csps tilga",
                         "ORODARA|csps badara",
                         "ORODARA|csps banflagoué")

hc_year_to_keep_2010 = c("SEGUENEGA|csps goungré",
                           "SEGUENEGA|dispe kondé tangaye")




dfEpi_list_2006 = lapply(dfEpi_list, select_ts_year,2006)
dfEpi_list_2007 = lapply(dfEpi_list, select_ts_year,2007)
dfEpi_list_2008 = lapply(dfEpi_list, select_ts_year,2008)
dfEpi_list_2010 = lapply(dfEpi_list, select_ts_year,2010)

## remove unwanted columns


# before removing unwanted column need to get hcenters to remove
temp_colnames_list_2007 = lapply(lapply(dfEpi_list_2007,colnames),setdiff, hc_year_to_keep_2007)
hc_year_to_remove_2007 = as.vector(c(unlist(temp_colnames_list_2007)))
#hc_year_to_remove_2007 = c(temp_colnames_list_2007$dfEpiSeguenega,temp_colnames_list_2007$dfEpiGourcy,temp_colnames_list_2007$dfEpiHounde)

temp_colnames_list_2008 = lapply(lapply(dfEpi_list_2008,colnames),setdiff, hc_year_to_keep_2008)
hc_year_to_remove_2008 = as.vector(c(unlist(temp_colnames_list_2008)))

temp_colnames_list_2010 = lapply(lapply(dfEpi_list_2010,colnames),setdiff, hc_year_to_keep_2010)
hc_year_to_remove_2010 = as.vector(c(unlist(temp_colnames_list_2010)))

#ctually removing the unwanted columns.

dfEpi_list_2006=lapply(dfEpi_list_2006, 
                       remove_unwanted_column,
                       hc_year_to_remove_2006)

# remoove hc with no useful data in 2006
dfEpi_list_2006[c("dfEpiboulsa", "dfEpiOrodora")] <- NULL

dfEpi_list_2007=lapply(dfEpi_list_2007, 
                       remove_unwanted_column,
                       hc_year_to_remove_2007)

dfEpi_list_2007[c("dfEpiDafra","dfEpiHounde", # removed dfEpiHounde purposefully
                  "dfEpiDande","dfEpiboulsa",
                  "dfEpiKvigue","dfEpiLena"
                  )] <- NULL

dfEpi_list_2008=lapply(dfEpi_list_2008, 
                       remove_unwanted_column,
                       hc_year_to_remove_2008)

dfEpi_list_2008[c("dfEpiDafra", "dfEpiDande",
                  "dfEpiHounde","dfEpiKvigue",
                  "dfEpiLena","dfEpiSeguenega" # removed dfEpiSeguenega purposefully
                  )] <- NULL


dfEpi_list_2010=lapply(dfEpi_list_2010, 
                       remove_unwanted_column,
                       hc_year_to_remove_2010)

dfEpi_list_2010[c("dfEpiboulsa","dfEpiDafra", "dfEpiDande",
                  "dfEpiGourcy","dfEpiHounde","dfEpiKvigue",
                  "dfEpiLena","dfEpiOrodora")] <- NULL


# visualise the data to check if there is remaining hc not meeting the def
# after smoothing and discarding hc-to remove.

#apply the add_julian_day function to the dfEpi_list_ elements.
# these zoo time series are to be used later to calibrate the model.
dfEpi_list_2006_with_julian_day = lapply(dfEpi_list_2006, add_julian_day,year=2006)
dfEpi_list_2007_with_julian_day = lapply(dfEpi_list_2007, add_julian_day,year=2007)
dfEpi_list_2008_with_julian_day = lapply(dfEpi_list_2008, add_julian_day,year=2008)
dfEpi_list_2010_with_julian_day = lapply(dfEpi_list_2010, add_julian_day,year=2010)


# Visualised all the health center years data in one plot
# regardless of their district.
# append all years list into one.


###============================================================
# preparing epidemic data_frame for use in optimation routine.
#==============================================================
#
#
#
####===========================================================
# Data visualisation for non-epidemic health center years
#==============================================================
#
#
